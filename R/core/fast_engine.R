# Compiled simulation engine ---------------------------------------------------
# The C++ engine in src/hospital_des.cpp applies the same patient-flow rules as
# the simmer model without its per-event R callbacks. It serves two callers:
#  * find_n_needed(), through fast_capacity_replication(): only the peak occupied
#    beds and the mean bed-less wait per unit, from each replication.
#  * run_hospital_scenario(), through fast_scenario_tables(): the dashboard
#    tables, rebuilt from the engine's event logs in simmer's table formats.
#
# Assumptions and guarantees:
#  * Arrivals and stays are drawn here with exactly the calls, seeds and order
#    of run_simulation() / run_baseline_simulation(), so a replication sees the
#    same patients under either engine.
#  * The search metrics follow capacity_replication(): peak occupancy over the
#    observation window (get_hospital_mon_resources()) and mean fresh-request
#    wait per episode (waiting_state_summary()). Checked equal to simmer,
#    replication by replication.
#  * Dashboard tables: waits, boarding episodes and occupancy are the same
#    patient flow. Events at the same instant may be logged in another order, so
#    figures that average recorded states (not time-weighted) can differ
#    slightly from simmer's.
#  * If the C++ file cannot be compiled (for example, no compiler on the
#    server), both callers fall back to simmer.
# The manuscript pipeline (paper/) does not source this file and is unaffected.

fast_engine_cpp_file <- normalizePath(file.path("src", "hospital_des.cpp"),
                                      winslash = "/", mustWork = FALSE)

# A persistent build cache lets each parallel worker load the compiled library
# instead of compiling it again.
fast_engine_cache_dir <- local({
  directory <- tryCatch(tools::R_user_dir("PatientSurgeModel", which = "cache"),
                        error = function(error) "")
  if (!nzchar(directory) ||
      !(dir.exists(directory) || dir.create(directory, recursive = TRUE, showWarnings = FALSE))) {
    directory <- file.path(tempdir(), "patient-surge-cpp")
  }
  directory
})

# The source's checksum identifies the compiled version, so an R session (or a
# parallel worker) that loaded an older src/hospital_des.cpp reloads it.
fast_engine_version <- unname(tools::md5sum(fast_engine_cpp_file))

# Compiles (or loads from the cache) once per R process and source version.
# Compiled functions cannot be sent to parallel workers, so each worker loads
# its own copy.
fast_engine_load <- function(engine) {
  loaded <- getOption("patient_surge.fast_engine")
  if (is.list(loaded) && identical(loaded$version, engine$version) && is.function(loaded$run)) {
    return(loaded$run)
  }
  compiled <- new.env(parent = baseenv())
  Rcpp::sourceCpp(engine$cpp_file, env = compiled, cacheDir = engine$cache_dir,
                  showOutput = FALSE)
  options(patient_surge.fast_engine = list(version = engine$version,
                                           run = compiled$hospital_des_run))
  compiled$hospital_des_run
}

fast_engine_status <- local({
  engine <- list(cpp_file = fast_engine_cpp_file, cache_dir = fast_engine_cache_dir,
                 version = fast_engine_version)
  if (!isTRUE(get0("use_fast_engine", ifnotfound = FALSE))) {
    "disabled"
  } else {
    tryCatch({
      fast_engine_load(engine)
      "ready"
    }, error = function(error) {
      warning("Compiled engine unavailable; simulations will use simmer. ",
              conditionMessage(error), call. = FALSE)
      "unavailable"
    })
  }
})

# Engine settings passed to the replication workers; NULL means simmer.
fast_engine_settings <- function() {
  if (!identical(fast_engine_status, "ready")) return(NULL)
  list(cpp_file = fast_engine_cpp_file, cache_dir = fast_engine_cache_dir,
       version = fast_engine_version)
}

# Patients of one population, in the same order, with the same stays and the
# same names as the generators built by run_simulation() / add_population().
fast_engine_population <- function(profiles, arrivals, service_seed, units, population,
                                   name_prefix) {
  service_seeds <- with_simulation_seed(service_seed,
    sample.int(.Machine$integer.max, length(profiles)))
  rows <- lapply(seq_along(profiles), function(index) {
    profile_name <- names(profiles)[[index]]
    profile <- profiles[[profile_name]]
    times <- sort(arrivals$arrival_time[arrivals$profile == profile_name])
    if (!length(times) || !length(profile$unit)) return(NULL)
    stays <- make_service_times(length(times), profile, service_seeds[[index]])
    list(patients = data.frame(name = paste0(name_prefix(index, profile_name), seq_along(times) - 1L),
                               profile = profile_name, population = population,
                               scheduled_arrival = times),
         n_steps = rep(length(profile$unit), length(times)),
         step_unit = rep(match(profile$unit, units) - 1L, times = length(times)),
         step_los = as.vector(t(stays)))
  })
  Filter(Negate(is.null), rows)
}

# Engine arguments plus the patient register; arguments mirror run_simulation().
fast_engine_inputs <- function(capacities, duration, n_patients, sim_days,
                               patient_profiles, profile_prob, fallbacks = list(),
                               recheck_interval_days = 1, baseline = NULL,
                               warmup_capacities = capacities, arrival_process = "even",
                               monitor_patients = TRUE, monitor_ed = TRUE,
                               credit_boarding_time = TRUE) {
  arrival_process <- match.arg(arrival_process, c("even", "poisson"))
  units <- names(capacities)
  civilian_enabled <- !is.null(baseline) && isTRUE(baseline$enabled)
  # Same draws, in the same order, as run_simulation() / run_baseline_simulation().
  seeds <- sample.int(.Machine$integer.max, 4L)
  if (civilian_enabled) {
    validate_baseline_config(baseline, warmup_capacities, fallbacks)
    if (n_patients > 0 && duration > 0) {
      validate_patient_configuration(capacities, patient_profiles, profile_prob, fallbacks)
    }
    surge_start <- baseline$warmup_days
    schedule <- with_simulation_seed(seeds[[1]],
      make_baseline_arrivals(baseline, surge_start + sim_days))
    surge <- with_simulation_seed(seeds[[3]],
      make_surge_arrivals(n_patients, duration, profile_prob, arrival_process))
    surge$arrival_time <- surge$arrival_time + surge_start
    populations <- c(
      fast_engine_population(baseline$profiles, schedule, seeds[[2]], units, "civilian",
                             function(index, profile) paste0("civilian_", index, "_")),
      fast_engine_population(patient_profiles, surge, seeds[[4]], units, "surge",
                             function(index, profile) paste0("surge_", index, "_")))
    warmup <- warmup_capacities[units]
  } else {
    validate_patient_configuration(capacities, patient_profiles, profile_prob, fallbacks)
    surge_start <- 0
    surge <- with_simulation_seed(seeds[[3]],
      make_surge_arrivals(n_patients, duration, profile_prob, arrival_process))
    populations <- fast_engine_population(patient_profiles, surge, seeds[[4]], units, "surge",
                                          function(index, profile) paste0("patient_", profile, "_"))
    warmup <- capacities[units]
  }
  patients <- dplyr::bind_rows(lapply(populations, `[[`, "patients"))
  n_steps <- unlist(lapply(populations, `[[`, "n_steps"), use.names = FALSE)
  step_unit <- unlist(lapply(populations, `[[`, "step_unit"), use.names = FALSE)
  step_los <- unlist(lapply(populations, `[[`, "step_los"), use.names = FALSE)
  # Arrivals are handled in time order; each patient's steps move with it.
  patient_order <- order(patients$scheduled_arrival)
  step_index <- split(seq_along(step_unit), rep(seq_along(n_steps), n_steps))[patient_order]
  step_index <- unlist(step_index, use.names = FALSE)
  patients <- patients[patient_order, , drop = FALSE]
  list(engine_args = list(
         warmup_capacity = as.numeric(warmup),
         event_capacity = as.numeric(capacities[units]),
         switch_time = if (civilian_enabled) surge_start else -1,
         candidates = lapply(units, function(primary) {
           match(unique(c(primary, fallbacks[[primary]])), units) - 1L
         }),
         is_ed = units == "ED",
         arrival = as.numeric(patients$scheduled_arrival),
         n_steps = as.integer(n_steps[patient_order]),
         step_unit = as.integer(step_unit[step_index]),
         step_los = as.numeric(step_los[step_index]),
         credit = isTRUE(credit_boarding_time),
         obs_start = surge_start,
         obs_end = surge_start + sim_days),
       units = units, patients = patients, civilian_enabled = civilian_enabled,
       surge_start = surge_start, capacities = capacities[units], warmup_capacities = warmup)
}

# Compiled counterpart of capacity_replication(); same columns and row order.
fast_capacity_replication <- function(replication_id, simulation_args, units, engine,
                                      replication_rng_state) {
  run_engine <- fast_engine_load(engine)
  inputs <- do.call(fast_engine_inputs, simulation_args)
  result <- do.call(run_engine, inputs$engine_args)
  index <- match(units, inputs$units)
  data.frame(
    resource = units,
    maximum_occupied = result$max_occupied[index],
    mean_wait_days = vapply(index, function(i) {
      safe_fraction(result$wait_person_days[[i]], result$wait_episodes[[i]])
    }, numeric(1))
  ) |>
    dplyr::mutate(replication = replication_id,
                  rng_state = rep(list(replication_rng_state), length(units)))
}

# Compiled counterpart of one simmer replication in run_hospital_scenario():
# the tables it reads from simmer, in the same formats.
#  * history: get_hospital_mon_resources(sim, include_warmup = TRUE).
#  * arrivals: get_hospital_mon_arrivals(sim).
#  * activity: simmer::get_mon_arrivals(sim, per_resource = TRUE, ongoing = TRUE)
#    in absolute time; open visits keep end_time = NA.
#  * metadata: attr(sim, "civilian_metadata") fields used there (NULL without
#    civilian flow).
fast_scenario_tables <- function(engine, simulation_args) {
  run_engine <- fast_engine_load(engine)
  inputs <- do.call(fast_engine_inputs, simulation_args)
  result <- do.call(run_engine, c(inputs$engine_args, list(record = TRUE)))
  units <- inputs$units
  patients <- inputs$patients
  surge_start <- inputs$surge_start

  # Consolidated unit states: server = occupied beds, queue = fresh (bed-less) waits.
  states <- result$states
  monitor <- data.frame(resource = units[states$unit + 1L], time = states$time,
                        server = states$server, queue = states$queue,
                        capacity = states$capacity, queue_size = Inf,
                        system = states$server + states$queue, limit = Inf,
                        replication = 1L)
  # One state per unit and instant (the last), as collect_hospital_resources()
  # leaves it; otherwise a request served at once would show a transient queue.
  monitor <- monitor[!duplicated(monitor[c("resource", "time")], fromLast = TRUE), , drop = FALSE]
  history <- if (inputs$civilian_enabled) {
    slice_resource_history(initialize_resource_history(monitor, inputs$warmup_capacities),
                           start = 0, end = surge_start + simulation_args$sim_days,
                           shift = surge_start)
  } else {
    slice_resource_history(initialize_resource_history(monitor, inputs$capacities),
                           0, simulation_args$sim_days)
  }

  visits <- result$visits
  prefix <- c("", logical_queue_prefix, boarding_prefix)[visits$kind + 1L]
  activity <- data.frame(name = patients$name[visits$patient + 1L],
                         start_time = visits$start, end_time = visits$end,
                         activity_time = visits$end - visits$start,
                         resource = paste0(prefix, units[visits$unit + 1L]),
                         replication = 1L)

  # As get_hospital_mon_arrivals(): activity time excludes bed-less waiting.
  fresh <- visits$kind == 1L & !is.na(visits$end)
  bedless_wait <- numeric(nrow(patients))
  waits <- rowsum(visits$end[fresh] - visits$start[fresh], visits$patient[fresh] + 1L)
  bedless_wait[as.integer(rownames(waits))] <- waits[, 1]
  discharge <- result$discharge_time
  arrivals <- data.frame(name = patients$name, start_time = patients$scheduled_arrival,
                         end_time = discharge,
                         activity_time = pmax(0, discharge - patients$scheduled_arrival - bedless_wait),
                         finished = !is.na(discharge), replication = 1L)
  metadata <- NULL
  if (inputs$civilian_enabled) {
    metadata <- list(surge_start = surge_start, patients = patients)
    arrivals <- arrivals |>
      dplyr::mutate(profile = patients$profile, population = patients$population,
                    start_time = .data$start_time - surge_start,
                    end_time = .data$end_time - surge_start,
                    scheduled_arrival = patients$scheduled_arrival - surge_start,
                    present_at_surge = .data$start_time < 0 &
                      (is.na(.data$end_time) | .data$end_time >= 0),
                    arrived_during_warmup = .data$start_time < 0)
  }
  list(history = history, arrivals = dplyr::arrange(arrivals, .data$start_time, .data$name),
       activity = activity, metadata = metadata)
}
