validate_patient_configuration <- function(capacities, patient_profiles, profile_prob, fallbacks) {
  stopifnot(
    is.numeric(capacities),
    length(capacities) > 0,
    !is.null(names(capacities)),
    all(is.finite(capacities)),
    all(capacities >= 0),
    all(capacities == floor(capacities)),
    is.list(patient_profiles),
    length(patient_profiles) > 0,
    is.numeric(profile_prob),
    setequal(names(patient_profiles), names(profile_prob)),
    all(is.finite(profile_prob)),
    all(profile_prob >= 0),
    abs(sum(profile_prob) - 1) < 1e-6,
    is.list(fallbacks)
  )

  for (values in list(names(capacities), names(patient_profiles), names(profile_prob))) {
    if (is.null(values) || anyNA(values) || any(!nzchar(values)) || anyDuplicated(values)) {
      stop("Capacities, profiles and probabilities need unique, nonempty names.")
    }
  }
  for (profile in patient_profiles) {
    if (length(profile$unit) != length(profile$los) ||
        (length(profile$unit) > 0 &&
         (!is.character(profile$unit) || anyNA(profile$unit) ||
          !is.numeric(profile$los) || any(!is.finite(profile$los)) || any(profile$los <= 0)))) {
      stop("Each pathway needs one positive finite mean stay for every ordered unit.")
    }
    if (!is.null(profile$cv) &&
        (length(profile$cv) != length(profile$unit) || !is.numeric(profile$cv) ||
         any(!is.finite(profile$cv)) || any(profile$cv <= 0))) {
      stop("Each pathway's coefficient of variation needs one positive finite value per ordered unit.")
    }
  }
  profile_units <- unique(unlist(lapply(patient_profiles, `[[`, "unit"), use.names = FALSE))
  fallback_units <- unique(c(names(fallbacks), unlist(fallbacks, use.names = FALSE)))
  configured_units <- names(capacities)
  if (length(setdiff(profile_units, configured_units)) > 0) {
    stop("Every patient trajectory unit must have a configured capacity.")
  }
  if (length(setdiff(fallback_units, configured_units)) > 0) {
    stop("Every fallback unit must have a configured capacity.")
  }
  invisible(TRUE)
}

random_stay <- function(mean_days, cv = 0.1) {
  sigma <- sqrt(log(1 + cv^2))
  stats::rlnorm(1, meanlog = log(mean_days) - sigma^2 / 2, sdlog = sigma)
}



# Local streams isolate arrival and service draws from simulation event ordering.
with_simulation_seed <- function(seed, code) {
  previous_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) previous_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    do.call(RNGkind, as.list(previous_kind))
    if (had_seed) assign(".Random.seed", previous_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv)
  })
  set.seed(seed, kind = "L'Ecuyer-CMRG")
  force(code)
}

# Deterministic per-index RNG streams. Replication i draws the same numbers
# whether it runs alone, in a full batch, or as part of any other grouping,
# so stopping a search early never changes what the replications that do
# run actually drew.
make_replication_stream_seeds <- function(n, seed) {
  previous_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) previous_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    do.call(RNGkind, as.list(previous_kind))
    if (had_seed) assign(".Random.seed", previous_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv)
  })
  set.seed(seed, kind = "L'Ecuyer-CMRG")
  state <- .Random.seed
  seeds <- vector("list", n)
  for (index in seq_len(n)) {
    seeds[[index]] <- state
    state <- parallel::nextRNGStream(state)
  }
  seeds
}

make_arrival_times <- function(rate, until, process = c("even", "poisson")) {
  process <- match.arg(process)
  stopifnot(length(rate) == 1L, is.finite(rate), rate >= 0,
            length(until) == 1L, is.finite(until), until >= 0)
  if (rate == 0 || until == 0) return(numeric())
  if (process == "even") {
    times <- (seq_len(ceiling(until * rate)) - 1) / rate
    return(times[times < until])
  }
  # Fixed batches preserve the arrival prefix when extending the horizon.
  batches <- list()
  last_time <- 0
  repeat {
    times <- last_time + cumsum(stats::rexp(256L, rate))
    batches[[length(batches) + 1L]] <- times[times < until]
    if (utils::tail(times, 1L) >= until) break
    last_time <- utils::tail(times, 1L)
  }
  unlist(batches, use.names = FALSE)
}

make_surge_arrivals <- function(rate, duration, profile_prob, process = "even") {
  times <- make_arrival_times(rate, duration, process)
  data.frame(arrival_time = times,
             profile = if (length(times)) sample(names(profile_prob), length(times),
                         replace = TRUE, prob = profile_prob) else character())
}

# profile$cv, when present, is one coefficient of variation per pathway step
# (parallel to profile$los); a missing step's default is the cv argument.
make_service_times <- function(n, profile, seed, cv = 0.1) {
  if (n == 0 || !length(profile$los)) return(matrix(numeric(), n, length(profile$los)))
  step_cv <- profile$cv
  if (is.null(step_cv)) step_cv <- rep(cv, length(profile$los))
  stopifnot(length(step_cv) == length(profile$los), is.numeric(step_cv),
            all(is.finite(step_cv)), all(step_cv > 0))
  with_simulation_seed(seed, {
    sigma <- sqrt(log(1 + step_cv^2))
    means <- rep(profile$los, times = n)
    sigma_rep <- rep(sigma, times = n)
    matrix(stats::rlnorm(length(means), log(means) - sigma_rep^2 / 2, sigma_rep),
           nrow = n, ncol = length(profile$los), byrow = TRUE)
  })
}

logical_queue_prefix <- ".waiting_for__"
boarding_prefix <- ".boarding_for__"

logical_queue_resource <- function(primary) {
  paste0(logical_queue_prefix, primary)
}

boarding_resource <- function(primary) {
  paste0(boarding_prefix, primary)
}


# A shared dispatcher preserves FIFO among requests that can use a free bed,
# with one priority tier above it: a patient who already holds a bed
# elsewhere -- boarding between pathway steps, or occupying a fallback and
# watching for its primary unit to free up -- is served ahead of a patient
# with no bed at all making a fresh request for the same unit (only possible
# on a pathway's first step). Reservations make selection and seize atomic
# across simultaneous events.
new_bed_dispatcher <- function(env, units) {
  state <- new.env(parent = emptyenv())
  state$requests <- list()
  state$priority <- list()
  state$reservations <- list()
  signal_for <- function(patient) paste0(".bed_ready__", patient)
  register <- function(candidates, priority = FALSE) {
    patient <- simmer::get_name(env)
    state$requests[[patient]] <- candidates
    state$priority[[patient]] <- isTRUE(priority)
    0
  }
  dispatch <- function() {
    if (!length(state$requests)) return(".no_bed_allocated")
    available <- vapply(units, function(unit) {
      reserved <- sum(vapply(state$reservations, identical, logical(1), unit))
      max(0, simmer::get_capacity(env, unit) - simmer::get_server_count(env, unit) - reserved)
    }, numeric(1))
    signals <- character()
    # state$priority always holds clean TRUE/FALSE (set via isTRUE() once, in
    # register()), so this can index/unlist directly instead of re-checking
    # isTRUE() per patient via vapply -- dispatch() runs on every bed release
    # and rescans every currently-waiting patient, so this was the dominant
    # cost under congestion (profiled: millions of isTRUE() calls).
    is_priority <- unlist(state$priority[names(state$requests)], use.names = FALSE)
    # order() is stable: within each tier, FCFS (registration order) is kept.
    ordered_patients <- names(state$requests)[order(!is_priority)]
    for (patient in ordered_patients) {
      candidates <- state$requests[[patient]]
      free <- candidates[available[candidates] > 0]
      if (!length(free)) next
      unit <- free[[1]]
      available[[unit]] <- available[[unit]] - 1
      state$reservations[[patient]] <- unit
      state$requests[[patient]] <- NULL
      state$priority[[patient]] <- NULL
      # The caller checks its reservation directly; only wake other patients.
      if (patient != simmer::get_name(env)) signals <- c(signals, signal_for(patient))
    }
    if (length(signals)) signals else ".no_bed_allocated"
  }
  list(register = register, dispatch = dispatch, signal_for = signal_for,
       assigned = function() state$reservations[[simmer::get_name(env)]],
       claim = function() {
         state$reservations[[simmer::get_name(env)]] <- NULL
         1
       })
}

# Builds the trajectory for pathway step `step_index` through the end of the
# pathway. `previous_unit` is the unit whose bed the patient currently holds
# (already seized, not yet released) -- NULL only on a pathway's first step,
# before anything has been seized. Waiting for this step's bed while
# `previous_unit` is held is "boarding" (the .boarding_for__ resource, with
# dispatcher priority); waiting with no bed held is a true, bed-less queue
# (the .waiting_for__ resource, only possible on the first step). Once any
# candidate bed is granted, `previous_unit` (if any) is released immediately,
# before the new bed is seized. If the granted bed is a fallback rather than
# this step's primary unit, the patient keeps watching for the primary while
# being served in the fallback: if the primary frees before this step's
# length of stay elapses, the patient transfers there (with dispatcher
# priority over a fresh request for that same bed) and resumes with the
# remaining stay, unchanged in total duration. Whichever unit the patient
# ends this step in becomes `previous_unit` for the next step.
run_pathway_step <- function(env, profile_name, profile, fallbacks,
                             step_index, previous_unit, service_time_for) {
  dispatcher <- attr(env, "bed_dispatcher")
  if (is.null(dispatcher)) stop("The simulation needs a shared bed dispatcher.")
  if (step_index > length(profile$unit)) {
    finish <- simmer::trajectory(paste0(profile_name, "_finished"))
    if (is.null(previous_unit)) return(finish)
    return(finish |> simmer::release(previous_unit, 1) |> simmer::send(dispatcher$dispatch))
  }
  primary <- profile$unit[[step_index]]
  candidates <- unique(c(primary, fallbacks[[primary]]))
  held <- !is.null(previous_unit)
  waiting_resource <- if (held) boarding_resource(primary) else logical_queue_resource(primary)
  my_signal <- function() dispatcher$signal_for(simmer::get_name(env))
  step_service_time <- service_time_for(step_index)
  id <- paste(profile_name, step_index, sep = "_")

  continuation_for <- function(unit) {
    if (identical(unit, primary)) {
      # Already in the primary unit: no transfer to watch for.
      simmer::trajectory(paste0(id, "_in_", unit)) |>
        simmer::timeout(step_service_time) |>
        simmer::join(run_pathway_step(env, profile_name, profile, fallbacks,
                                      step_index + 1L, primary, service_time_for))
    } else {
      # simmer::trap(handler=X) semantics (confirmed against the package's own
      # documentation for send()/trap()): on signal receipt, the arrival stops
      # its current activity, runs X, THEN CONTINUES with whatever follows the
      # interrupted activity in the surrounding pipe -- it does not replace
      # the rest of the trajectory. So the interrupted timeout() below and its
      # handler both funnel into the SAME shared continuation afterward
      # (untrap, then branch on whether a transfer happened); the handler must
      # NOT itself contain untrap/join, or that shared continuation runs a
      # second time on top of the handler's (already-completed) actions.
      dur_attr <- paste0(".", id, "_duration")
      started_attr <- paste0(".", id, "_started")
      transferred_attr <- paste0(".", id, "_transferred")
      watch_signal <- function() dispatcher$signal_for(simmer::get_name(env))
      transferred_next <- run_pathway_step(env, profile_name, profile, fallbacks,
                                           step_index + 1L, primary, service_time_for)
      stayed_next <- run_pathway_step(env, profile_name, profile, fallbacks,
                                      step_index + 1L, unit, service_time_for)
      remaining_time <- function() {
        elapsed <- simmer::now(env) - simmer::get_attribute(env, started_attr)
        max(simmer::get_attribute(env, dur_attr) - elapsed, .Machine$double.eps)
      }
      # Built fresh each call (used once as the trap's interrupt handler, once
      # for an already-free primary at registration time); "elapsed" is ~0 in
      # the latter case. Deliberately has no untrap/join of its own. Releases
      # .boarding_for__<primary> the moment primary is actually seized --
      # boarding ends there, not after the remaining stay in primary.
      transfer_now <- function(label) {
        simmer::trajectory(paste0(id, "_", label, "_", unit, "_to_", primary)) |>
          simmer::set_attribute(transferred_attr, 1) |>
          simmer::release(unit, 1) |>
          simmer::send(dispatcher$dispatch) |>
          simmer::seize(primary, dispatcher$claim) |>
          simmer::release(boarding_resource(primary), 1) |>
          simmer::timeout(remaining_time)
      }
      # Occupying a fallback while watching for the primary is boarding too
      # (the patient holds a bed elsewhere -- here, the fallback itself --
      # while wanting a different unit), so it is tracked on the very same
      # .boarding_for__<primary> resource as boarding between pathway steps;
      # both feed the same boarding-time statistics and search criterion.
      simmer::trajectory(paste0(id, "_in_", unit)) |>
        simmer::set_attribute(dur_attr, step_service_time) |>
        simmer::set_attribute(started_attr, function() simmer::now(env)) |>
        simmer::set_attribute(transferred_attr, 0) |>
        simmer::seize(boarding_resource(primary), 1) |>
        simmer::trap(watch_signal, handler = transfer_now("transferred")) |>
        simmer::set_attribute(paste0(".", id, "_watch"),
          function() dispatcher$register(primary, priority = TRUE)) |>
        simmer::send(dispatcher$dispatch) |>
        simmer::branch(function() if (is.null(dispatcher$assigned())) 1L else 2L,
          continue = c(TRUE, TRUE),
          simmer::trajectory(paste0(id, "_watching_", unit)) |>
            simmer::timeout(function() simmer::get_attribute(env, dur_attr)),
          transfer_now("immediate")) |>
        simmer::untrap(watch_signal) |>
        # A transfer (handler or immediate) already released
        # .boarding_for__<primary> the moment it seized primary; a normal,
        # never-transferred completion has not, so release it now. Interrupt
        # resumption re-enters this shared point regardless of which path was
        # taken (see the note above transfer_now()), so this must stay a
        # single conditional release, not a second unconditional one.
        simmer::release(boarding_resource(primary),
          function() if (isTRUE(simmer::get_attribute(env, transferred_attr) == 1)) 0 else 1) |>
        simmer::branch(function() if (isTRUE(simmer::get_attribute(env, transferred_attr) == 1)) 1L else 2L,
          continue = c(TRUE, TRUE),
          transferred_next,
          stayed_next)
    }
  }

  candidate_branches <- lapply(candidates, function(unit) {
    prefix <- simmer::trajectory(paste0(id, "_go_", unit))
    if (held) prefix <- prefix |> simmer::release(previous_unit, 1) |> simmer::send(dispatcher$dispatch)
    prefix |> simmer::seize(unit, dispatcher$claim) |> simmer::join(continuation_for(unit))
  })
  take_bed <- do.call(simmer::branch, c(list(
    .trj = simmer::trajectory(paste0(id, "_take")),
    option = function() match(dispatcher$assigned(), candidates),
    continue = rep(TRUE, length(candidate_branches))), candidate_branches))

  simmer::trajectory(paste0(id, "_request")) |>
    simmer::seize(waiting_resource, 1) |>
    simmer::trap(my_signal) |>
    simmer::set_attribute(paste0(".", id, "_request"),
      function() dispatcher$register(candidates, priority = held)) |>
    simmer::send(dispatcher$dispatch) |>
    simmer::branch(function() if (is.null(dispatcher$assigned())) 1L else 2L,
      continue = c(TRUE, TRUE),
      simmer::trajectory(paste0(id, "_await")) |> simmer::wait(),
      simmer::trajectory(paste0(id, "_ready"))) |>
    simmer::untrap(my_signal) |>
    simmer::release(waiting_resource, 1) |>
    simmer::join(take_bed)
}

profile_trajectory <- function(env, profile_name, patient_profiles, fallbacks,
                               recheck_interval_days = 1, service_times = NULL) {
  # Freeze loop arguments before simmer calls the service closures later.
  # recheck_interval_days is accepted for older callers' positional arguments
  # but unused: bed acquisition is fully event-driven, never polled.
  force(service_times)
  if (!profile_name %in% names(patient_profiles)) {
    stop("Unknown patient profile: ", profile_name)
  }
  profile <- patient_profiles[[profile_name]]
  if (is.null(profile$unit) || length(profile$unit) == 0) {
    return(simmer::trajectory(profile_name) |> simmer::timeout(0.1))
  }
  service_time_for <- function(step) {
    step_cv <- if (!is.null(profile$cv)) profile$cv[[step]] else 0.1
    function() {
      if (is.null(service_times)) return(random_stay(profile$los[[step]], cv = step_cv))
      patient <- as.integer(sub("^.*_", "", simmer::get_name(env))) + 1L
      service_times[patient, step]
    }
  }
  run_pathway_step(env, profile_name, profile, fallbacks, 1L, NULL, service_time_for)
}

sample_patient_profile <- function(profile_prob) {
  base::sample(names(profile_prob), size = 1, prob = profile_prob)
}

run_simulation <- function(capacities, duration, n_patients, sim_days,
                           patient_profiles, profile_prob, fallbacks = list(),
                           recheck_interval_days = 1, baseline = NULL,
                           warmup_capacities = capacities, arrival_process = "even",
                           monitor_patients = TRUE) {
  stopifnot(is.logical(monitor_patients), length(monitor_patients) == 1L,
            !is.na(monitor_patients))
  arrival_process <- match.arg(arrival_process, c("even", "poisson"))
  stopifnot(length(duration) == 1L, is.finite(duration), duration >= 0,
            duration == floor(duration), length(n_patients) == 1L,
            is.finite(n_patients), n_patients >= 0,
            arrival_process == "poisson" || n_patients == floor(n_patients),
            length(sim_days) == 1L, is.finite(sim_days), sim_days > 0,
            sim_days >= duration)
  if (!is.null(baseline) && isTRUE(baseline$enabled)) {
    return(run_baseline_simulation(capacities, duration, n_patients, sim_days,
                                    patient_profiles, profile_prob, fallbacks, baseline,
                                    warmup_capacities, arrival_process, monitor_patients))
  }
  validate_patient_configuration(capacities, patient_profiles, profile_prob, fallbacks)
  stopifnot(
    length(recheck_interval_days) == 1,
    is.finite(recheck_interval_days),
    recheck_interval_days > 0
  )
  hospital_sim <- simmer::simmer("hospital-simulation")
  attr(hospital_sim, "bed_dispatcher") <- new_bed_dispatcher(hospital_sim, names(capacities))
  for (unit_name in names(capacities)) {
    hospital_sim <- simmer::add_resource(
      hospital_sim,
      unit_name,
      capacity = as.integer(capacities[[unit_name]]),
      queue_size = Inf
    )
    hospital_sim <- simmer::add_resource(
      hospital_sim,
      logical_queue_resource(unit_name),
      capacity = Inf,
      queue_size = 0
    )
    hospital_sim <- simmer::add_resource(
      hospital_sim,
      boarding_resource(unit_name),
      capacity = Inf,
      queue_size = 0
    )
  }

  seeds <- sample.int(.Machine$integer.max, 4L)
  patient_data <- with_simulation_seed(seeds[[3]],
    make_surge_arrivals(n_patients, duration, profile_prob, arrival_process))
  patient_data$profile_name <- patient_data$profile
  service_seeds <- with_simulation_seed(seeds[[4]],
    sample.int(.Machine$integer.max, length(patient_profiles)))
  trajectories <- stats::setNames(
    lapply(names(patient_profiles), function(profile_name) {
      profile_trajectory(
        hospital_sim,
        profile_name,
        patient_profiles,
        fallbacks,
        recheck_interval_days = recheck_interval_days,
        service_times = make_service_times(sum(patient_data$profile == profile_name),
          patient_profiles[[profile_name]], service_seeds[[match(profile_name, names(patient_profiles))]])
      )
    }),
    names(patient_profiles)
  )

  for (profile_name in names(trajectories)) {
    arrival_times <- sort(patient_data$arrival_time[
      patient_data$profile_name == profile_name
    ])
    if (length(arrival_times) == 0) next

    hospital_sim <- simmer::add_generator(
      hospital_sim,
      name_prefix = paste0("patient_", profile_name, "_"),
      trajectory = trajectories[[profile_name]],
      distribution = simmer::at(arrival_times), mon = monitor_patients
    )
  }

  result <- hospital_sim |>
    simmer::run(until = sim_days) |>
    simmer::wrap()
  attr(result, "observation_metadata") <- list(capacities = capacities, sim_days = sim_days)
  result
}

collect_hospital_resources <- function(simulation, include_resources = NULL) {
  resources <- simmer::get_mon_resources(simulation)
  required_columns <- c(
    "resource", "time", "server", "queue", "capacity",
    "queue_size", "system", "limit"
  )
  stopifnot(all(required_columns %in% names(resources)))
  if (nrow(resources) == 0) return(resources)

  # Boarding episodes are reported from per-visit arrival records (see
  # boarding_time_summary()), not from this state-level monitor; drop them
  # here so they never appear as a spurious extra "resource" in state-based
  # occupancy/queue reporting or plots.
  resources <- resources[!startsWith(resources$resource, boarding_prefix), , drop = FALSE]

  if (!is.null(include_resources)) {
    include_resources <- unique(as.character(include_resources))
  }

  logical_rows <- startsWith(resources$resource, logical_queue_prefix)
  if (!any(logical_rows)) {
    if (!is.null(include_resources)) {
      resources <- resources[resources$resource %in% include_resources, , drop = FALSE]
    }
    return(resources)
  }

  waiting_resources <- unique(resources$resource[logical_rows])
  if (!is.null(include_resources)) {
    waiting_primary <- substring(
      waiting_resources,
      nchar(logical_queue_prefix) + 1L
    )
    waiting_resources <- waiting_resources[waiting_primary %in% include_resources]
    physical_resources <- resources[
      !logical_rows & resources$resource %in% include_resources,
      , drop = FALSE
    ]
  } else {
    physical_resources <- resources[!logical_rows, , drop = FALSE]
  }

  carry_forward <- function(event_times, event_values, query_times, default = 0) {
    order_index <- order(event_times, seq_along(event_times))
    event_times <- event_times[order_index]
    event_values <- event_values[order_index]
    event_index <- findInterval(query_times, event_times)
    values <- rep(default, length(query_times))
    has_value <- event_index > 0
    values[has_value] <- event_values[event_index[has_value]]
    values
  }

  consolidated <- lapply(waiting_resources, function(waiting_resource) {
    primary <- substring(waiting_resource, nchar(logical_queue_prefix) + 1L)
    primary_rows <- resources[resources$resource == primary, , drop = FALSE]
    waiting_rows <- resources[
      resources$resource == waiting_resource, , drop = FALSE
    ]
    if (nrow(primary_rows) == 0) {
      primary_rows <- waiting_rows[1, , drop = FALSE]
      primary_rows$resource <- primary
      primary_rows$server <- 0
      primary_rows$queue <- 0
      primary_rows$capacity <- simmer::get_capacity(simulation, primary)
      primary_rows$queue_size <- simmer::get_queue_size(simulation, primary)
      primary_rows$system <- 0
      primary_rows$limit <- primary_rows$capacity + primary_rows$queue_size
    }

    replication_values <- if ("replication" %in% names(resources)) {
      unique(waiting_rows$replication)
    } else {
      NA_integer_
    }

    dplyr::bind_rows(lapply(replication_values, function(replication_value) {
      if ("replication" %in% names(resources)) {
        primary_rep <- primary_rows[
          primary_rows$replication == replication_value, , drop = FALSE
        ]
        waiting_rep <- waiting_rows[
          waiting_rows$replication == replication_value, , drop = FALSE
        ]
      } else {
        primary_rep <- primary_rows
        waiting_rep <- waiting_rows
      }

      event_times <- sort(unique(c(primary_rep$time, waiting_rep$time)))
      primary_order <- order(primary_rep$time, seq_len(nrow(primary_rep)))
      primary_rep <- primary_rep[primary_order, , drop = FALSE]
      template_index <- pmax(findInterval(event_times, primary_rep$time), 1L)
      result <- primary_rep[template_index, , drop = FALSE]
      result$resource <- primary
      result$time <- event_times
      result$server <- carry_forward(
        primary_rep$time, primary_rep$server, event_times
      )
      result$queue <- carry_forward(
        primary_rep$time, primary_rep$queue, event_times
      ) + carry_forward(
        waiting_rep$time, waiting_rep$server, event_times
      )
      result$system <- result$server + result$queue
      result
    }))
  })

  consolidated_primary <- unique(vapply(
    waiting_resources,
    function(waiting_resource) {
      substring(waiting_resource, nchar(logical_queue_prefix) + 1L)
    },
    character(1)
  ))

  dplyr::bind_rows(
    physical_resources[
      !physical_resources$resource %in% consolidated_primary, , drop = FALSE
    ],
    consolidated
  ) |>
    dplyr::arrange(.data$time, .data$resource)
}

get_hospital_mon_resources <- function(simulation, include_resources = NULL,
                                        include_warmup = FALSE) {
  resources <- collect_hospital_resources(simulation, include_resources)
  metadata <- attr(simulation, "civilian_metadata")
  if (is.null(metadata)) {
    observation <- attr(simulation, "observation_metadata")
    if (is.null(observation)) return(resources)
    capacities <- observation$capacities
    if (!is.null(include_resources)) capacities <- capacities[intersect(names(capacities), include_resources)]
    return(slice_resource_history(initialize_resource_history(resources, capacities),
                                  0, observation$sim_days))
  }
  capacities <- metadata$warmup_capacities
  if (is.null(capacities)) capacities <- metadata$capacities
  if (!is.null(include_resources)) capacities <- capacities[intersect(names(capacities), include_resources)]
  resources <- initialize_resource_history(resources, capacities)
  slice_resource_history(resources,
                         start = if (include_warmup) 0 else metadata$surge_start,
                         end = metadata$observation_end, shift = metadata$surge_start)
}

get_hospital_mon_arrivals <- function(simulation) {
  metadata <- attr(simulation, "civilian_metadata")
  arrivals <- simmer::get_mon_arrivals(simulation, ongoing = TRUE) |>
    dplyr::filter(.data$start_time >= 0)
  arrivals_by_resource <- simmer::get_mon_arrivals(
    simulation,
    per_resource = TRUE, ongoing = TRUE
  )
  # simmer omits replication on completely empty monitor tables.
  if (!"replication" %in% names(arrivals)) arrivals$replication <- rep(1L, nrow(arrivals))
  if (!"replication" %in% names(arrivals_by_resource))
    arrivals_by_resource$replication <- rep(1L, nrow(arrivals_by_resource))
  logical_wait <- arrivals_by_resource |>
    dplyr::filter(startsWith(.data$resource, logical_queue_prefix)) |>
    dplyr::group_by(.data$name, .data$replication) |>
    dplyr::summarise(
      logical_wait_days = sum(.data$activity_time, na.rm = TRUE),
      .groups = "drop"
    )
  arrivals <- arrivals |>
    dplyr::left_join(logical_wait, by = c("name", "replication")) |>
    dplyr::mutate(
      logical_wait_days = dplyr::coalesce(.data$logical_wait_days, 0),
      activity_time = pmax(0, .data$activity_time - .data$logical_wait_days)
    ) |>
    dplyr::select(-dplyr::all_of("logical_wait_days"))
  if (is.null(metadata)) return(dplyr::arrange(arrivals, .data$start_time, .data$name))
  arrivals |>
    dplyr::left_join(metadata$patients, by = "name") |>
    dplyr::mutate(start_time = .data$start_time - metadata$surge_start,
                  end_time = .data$end_time - metadata$surge_start,
                  scheduled_arrival = .data$scheduled_arrival - metadata$surge_start,
                  present_at_surge = .data$start_time < 0 &
                    (is.na(.data$end_time) | .data$end_time >= 0),
                  arrived_during_warmup = .data$start_time < 0) |>
    dplyr::arrange(.data$start_time, .data$name)
}

estimate_peak_unit_demand <- function(patient_profiles, profile_prob, n_patients,
                                      duration, sim_days,
                                      units = c("GenMed", "ICU")) {
  stopifnot(
    length(n_patients) == 1,
    length(duration) == 1,
    length(sim_days) == 1,
    n_patients > 0,
    duration >= 1,
    sim_days >= 1
  )

  arrival_times <- make_arrival_times(n_patients, duration, "even")
  events <- stats::setNames(lapply(units, function(unit_name) {
    data.frame(unit = character(), time = numeric(), change = numeric())
  }), units)

  for (profile_name in names(profile_prob)) {
    path <- patient_profiles[[profile_name]]
    if (is.null(path$unit) || length(path$unit) == 0) next
    start_offsets <- c(0, utils::head(cumsum(path$los), -1))
    profile_probability <- profile_prob[[profile_name]]

    for (path_index in seq_along(path$unit)) {
      unit_name <- path$unit[[path_index]]
      if (!unit_name %in% units) next
      start_times <- arrival_times + start_offsets[[path_index]]
      end_times <- pmin(start_times + path$los[[path_index]], sim_days)
      valid <- start_times < sim_days & end_times > start_times
      if (!any(valid)) next
      events[[unit_name]] <- rbind(
        events[[unit_name]],
        data.frame(
          unit = unit_name,
          time = c(start_times[valid], end_times[valid]),
          change = c(
            rep(profile_probability, sum(valid)),
            rep(-profile_probability, sum(valid))
          )
        )
      )
    }
  }

  dplyr::bind_rows(lapply(units, function(unit_name) {
    unit_events <- events[[unit_name]]
    if (nrow(unit_events) == 0) {
      return(data.frame(unit = unit_name, expected_peak_beds = 0))
    }
    occupancy <- unit_events |>
      dplyr::group_by(.data$time) |>
      dplyr::summarise(change = sum(.data$change), .groups = "drop") |>
      dplyr::arrange(.data$time) |>
      dplyr::mutate(expected_occupancy = cumsum(.data$change))
    data.frame(
      unit = unit_name,
      expected_peak_beds = max(occupancy$expected_occupancy, 0)
    )
  }))
}
# Per-unit mean boarding-episode duration within the observation window
# [0, sim_days), read entirely from the .boarding_for__<unit> resource's own
# STATE history (get_mon_resources()) -- no per-patient (arrival) monitoring
# needed, so this works with monitor_patients = FALSE. This is an exact
# identity, not an approximation: the time integral of that resource's
# `server` count always equals the sum of every episode's duration (an
# episode still open at the horizon contributes only its elapsed portion,
# the same conservative treatment used elsewhere), and the number of
# episodes started equals the number of times `server` increased (each
# episode is exactly one seize/release pair). mean_boarding_days is that
# integral divided by the episode count. Unlike the previous per-visit
# criterion, this cannot recover the single longest episode -- only the mean.
boarding_state_summary <- function(simulation, units, sim_days) {
  metadata <- attr(simulation, "civilian_metadata")
  raw <- simmer::get_mon_resources(simulation)
  boarding <- raw[startsWith(raw$resource, boarding_prefix), , drop = FALSE]
  boarding$resource <- substring(boarding$resource, nchar(boarding_prefix) + 1L)
  boarding <- boarding[boarding$resource %in% units, , drop = FALSE]
  inf_capacities <- stats::setNames(rep(Inf, length(units)), units)
  history <- initialize_resource_history(boarding, inf_capacities)
  start <- if (is.null(metadata)) 0 else metadata$surge_start
  end <- if (is.null(metadata)) sim_days else metadata$observation_end
  sliced <- slice_resource_history(history, start = start, end = end, shift = start)
  dplyr::bind_rows(lapply(units, function(unit_name) {
    rows <- sliced[sliced$resource == unit_name, , drop = FALSE]
    rows <- rows[order(rows$time), , drop = FALSE]
    if (nrow(rows) < 2) {
      return(data.frame(resource = unit_name, mean_boarding_days = 0))
    }
    dt <- diff(rows$time)
    person_days <- sum(utils::head(rows$server, -1) * dt)
    episodes <- sum(pmax(0, diff(rows$server)))
    data.frame(resource = unit_name, mean_boarding_days = safe_fraction(person_days, episodes))
  }))
}

# Top-level worker avoids exporting the optimizer's cache and nested closures.
capacity_replication <- function(replication_id, simulation_args, units) {
  replication_rng_state <- get(".Random.seed", envir = .GlobalEnv)
  # mean_boarding_days is read from resource state history alone (see
  # boarding_state_summary()), so capacity selection still needs no
  # individual arrival records, same as before boarding time replaced queue
  # length as the acceptance criterion.
  simulation_args$monitor_patients <- FALSE
  simulation <- do.call(run_simulation, simulation_args)
  resources <- get_hospital_mon_resources(simulation, include_resources = units)
  occupancy <- resource_state_intervals(resources) |>
    dplyr::group_by(.data$resource) |>
    dplyr::summarise(maximum_occupied = safe_max(.data$server), .groups = "drop")
  boarding <- boarding_state_summary(simulation, units, simulation_args$sim_days)
  summary <- dplyr::left_join(data.frame(resource = units), occupancy, by = "resource") |>
    dplyr::left_join(boarding, by = "resource")
  summary$mean_boarding_days[is.na(summary$mean_boarding_days)] <- 0
  if (anyNA(summary$mean_boarding_days)) stop("Target resource monitoring is incomplete for optimization.")
  dplyr::mutate(summary, replication = replication_id,
    rng_state = rep(list(replication_rng_state), nrow(summary)))
}

# Explicit arguments keep the optimizer cache and history out of worker exports.
unlimited_demand_replication <- function(replication_id, simulation_args, units) {
  simulation_args$monitor_patients <- FALSE
  simulation <- do.call(run_simulation, simulation_args)
  simmer::get_mon_resources(simulation) |>
    dplyr::filter(!startsWith(.data$resource, logical_queue_prefix),
                  .data$resource %in% units) |>
    dplyr::group_by(.data$resource) |>
    dplyr::summarise(
      maximum_occupied = as.integer(ceiling(safe_max(.data$server))),
      .groups = "drop")
}

# Independent replication means; the t interval measures Monte Carlo error.
# With one replication, uncertainty is unavailable rather than zero. Mean
# boarding time is a complementary diagnostic, not the acceptance criterion
# (which uses peak boarding time per replication; see find_n_needed()).
summarize_boarding_means <- function(resources, thresholds, confidence_level = 0.95) {
  resources |>
    dplyr::group_by(.data$resource) |>
    dplyr::summarise(replications = dplyr::n(), sd_boarding = stats::sd(.data$mean_boarding_days),
      mean_boarding_days = mean(.data$mean_boarding_days), .groups = "drop") |>
    dplyr::mutate(mcse = .data$sd_boarding / sqrt(.data$replications),
      threshold = unname(thresholds[.data$resource]),
      confidence_level = confidence_level,
      critical_value = stats::qt((1 + confidence_level) / 2, pmax(1, .data$replications - 1)),
      lower = .data$mean_boarding_days - .data$critical_value * .data$mcse,
      upper = .data$mean_boarding_days + .data$critical_value * .data$mcse,
      passes = .data$mean_boarding_days <= .data$threshold,
      interval_crosses_threshold = .data$lower <= .data$threshold & .data$upper >= .data$threshold) |>
    dplyr::select(-"critical_value")
}

find_n_needed <- function(capacities, duration, n_patients, sim_days,
                          patient_profiles, profile_prob, fallbacks = list(),
                          num_sims = 20,
                          max_evaluations = 100,
                          minimum_step = 1, demand_safety_factor = 1.3,
                          # rho = 0.5 with the default lower_ci rule (below) means: require
                          # statistical confidence that the TRUE joint compliance
                          # probability exceeds 50% (better than a coin flip), rather than
                          # requiring a high observed proportion that could be a lucky draw.
                          reliability_level = 0.5, refinement_margin = 0.10,
                          # Boarding-time limits are in days (e.g. 1 = 24 hours), not the
                          # patient-count limits the pre-boarding queue criterion used.
                          boarding_time_limit_GenMed = 1, boarding_time_limit_ICU = 1,
                          weight_GenMed = 1, weight_ICU = 1,
                          acceptance_rule = c("lower_ci", "point_estimate"),
                          acceptance_confidence = 0.95,
                          workers = 1, search_seed = 2026, verbose = FALSE,
                          baseline = NULL, warmup_capacities = capacities,
                          arrival_process = "even", final_num_sims = 50L,
                          initialization = c("analytical", "incremental")) {
  optimization_started <- proc.time()[["elapsed"]]
  # reliability_level is the required proportion of joint peak-compliant runs;
  # this is the criterion reported as the search's acceptance target and the
  # one checked before triggering the independent holdout evaluation.
  # refinement_margin makes growth and refinement (but not the reported
  # criterion) target reliability_level + refinement_margin instead, so the
  # search does not stop growing or shrink capacity right at the boundary
  # where Monte Carlo noise makes the independent holdout evaluation likely
  # to disagree. A selected candidate that meets the margin automatically
  # meets the unmargined reliability_level as well.
  # One fixed replication bank and exact boarding-time limits for all candidate selection.
  # The independent final bank is never used to tune capacity.
  # weight_GenMed/weight_ICU only affect which feasible candidate the
  # refinement step prefers (total_added below); they never change whether a
  # candidate passes the joint criterion. Defaults (1, 1) reproduce the
  # previous unweighted objective exactly.
  # acceptance_rule = "lower_ci" (default) requires the lower bound of an
  # exact binomial confidence interval (at acceptance_confidence) on the
  # observed joint proportion to meet or exceed reliability_level -- a
  # stricter, more conservative test than comparing the observed proportion
  # itself. acceptance_rule = "point_estimate" instead accepts whenever the
  # observed joint proportion alone meets required_successes. Growth/
  # refinement still steer toward passes_margin (always point-estimate-based
  # on reliability_level + refinement_margin); only the reported
  # passes field, and therefore whether holdout evaluation is triggered, is
  # affected by acceptance_rule.
  arrival_process <- match.arg(arrival_process, c("even", "poisson"))
  initialization <- match.arg(initialization)
  acceptance_rule <- match.arg(acceptance_rule)
  stopifnot(final_num_sims >= 1, final_num_sims == floor(final_num_sims),
            is.finite(search_seed), search_seed >= 1,
            search_seed <= .Machine$integer.max - 200000L,
            is.finite(boarding_time_limit_GenMed), boarding_time_limit_GenMed >= 0,
            is.finite(boarding_time_limit_ICU), boarding_time_limit_ICU >= 0,
            is.finite(weight_GenMed), weight_GenMed > 0,
            is.finite(weight_ICU), weight_ICU > 0,
            is.finite(acceptance_confidence), acceptance_confidence > 0, acceptance_confidence < 1)
  validate_patient_configuration(capacities, patient_profiles, profile_prob, fallbacks)
  if (!is.null(baseline) && isTRUE(baseline$enabled)) {
    validate_baseline_config(baseline, warmup_capacities, fallbacks)
  }
  stopifnot(
    all(c("GenMed", "ICU") %in% names(capacities)),
    length(num_sims) == 1, is.finite(num_sims), num_sims >= 1, num_sims == floor(num_sims),
    max_evaluations >= 2,
    minimum_step >= 1,
    demand_safety_factor >= 1,
    is.finite(reliability_level),
    reliability_level > 0,
    reliability_level <= 1,
    is.finite(refinement_margin),
    refinement_margin >= 0
  )

  # Profiling found repeated dependency discovery dominated short evaluations.
  # Resolve once per search; candidate inputs still travel as explicit arguments.
  worker_dependencies <- future::getGlobalsAndPackages(
    quote(list(capacity_replication, unlimited_demand_replication)),
    envir = environment(find_n_needed))
  configured_capacities <- capacities
  initial_capacities <- as.integer(ceiling(capacities[c("GenMed", "ICU")]))
  names(initial_capacities) <- c("GenMed", "ICU")
  demand <- estimate_peak_unit_demand(
    patient_profiles = patient_profiles,
    profile_prob = profile_prob,
    n_patients = n_patients,
    duration = duration,
    sim_days = sim_days
  )
  expected_peak <- stats::setNames(demand$expected_peak_beds, demand$unit)
  estimated_capacities <- as.integer(ceiling(
    expected_peak[c("GenMed", "ICU")] * demand_safety_factor
  ))
  estimated_capacities <- pmax(initial_capacities, estimated_capacities)
  names(estimated_capacities) <- names(initial_capacities)
  estimated_additional_beds <- pmax(0L, estimated_capacities - initial_capacities)
  names(estimated_additional_beds) <- names(initial_capacities)

  # Routine offered load plus the finite-horizon surge trajectory is a starting
  # guess only. It ignores congestion/fallback redistribution, not a capacity bound.
  routine_load <- stats::setNames(c(0, 0), names(initial_capacities))
  if (!is.null(baseline) && isTRUE(baseline$enabled)) {
    for (profile in names(baseline$profiles)) {
      path <- baseline$profiles[[profile]]
      for (unit in names(routine_load)) {
        routine_load[[unit]] <- routine_load[[unit]] + baseline$arrival_rates[[profile]] *
          sum(path$los[path$unit == unit])
      }
    }
  }
  analytical_start <- pmax(initial_capacities,
    ceiling(routine_load + demand_safety_factor * expected_peak[names(initial_capacities)]))

  cache <- new.env(parent = emptyenv())
  search_evaluation_count <- 0L
  final_evaluation_count <- 0L
  evaluation_history <- list()
  replication_history <- list()
  auxiliary_history <- list()
  cache_hits <- 0L

  replication_chunk_size <- function(replications) {
    as.integer(max(1L, ceiling(replications / max(1L, workers))))
  }

  evaluate <- function(candidate, replications = num_sims,
                        stage = c("search", "holdout")) {
    stage <- match.arg(stage)
    replications <- as.integer(if (stage == "holdout") replications else num_sims)
    candidate <- as.integer(candidate[c("GenMed", "ICU")])
    names(candidate) <- c("GenMed", "ICU")
    key <- paste(c(stage, candidate, replications), collapse = ":")
    if (exists(key, envir = cache, inherits = FALSE)) {
      cache_hits <<- cache_hits + 1L
      return(get(key, envir = cache, inherits = FALSE))
    }
    if (stage == "search" && search_evaluation_count >= max_evaluations) return(NULL)

    if (stage == "search") {
      search_evaluation_count <<- search_evaluation_count + 1L
      stage_evaluation <- search_evaluation_count
      evaluation_seed <- search_seed
    } else {
      final_evaluation_count <<- final_evaluation_count + 1L
      stage_evaluation <- final_evaluation_count
      evaluation_seed <- search_seed + 200000L
    }

    simulation_capacities <- configured_capacities
    evaluation_started <- proc.time()[["elapsed"]]
    simulation_capacities[c("GenMed", "ICU")] <- candidate
    simulation_args <- list(capacities = simulation_capacities, duration = duration,
      n_patients = n_patients, sim_days = sim_days, patient_profiles = patient_profiles,
      profile_prob = profile_prob, fallbacks = fallbacks, baseline = baseline,
      warmup_capacities = warmup_capacities, arrival_process = arrival_process)
    thresholds <- c(GenMed = boarding_time_limit_GenMed, ICU = boarding_time_limit_ICU)

    run_batch <- function(batch_ids, batch_seed) {
      future.apply::future_lapply(
        batch_ids, capacity_replication,
        simulation_args = simulation_args, units = c("GenMed", "ICU"),
        future.globals = worker_dependencies$globals,
        future.packages = worker_dependencies$packages,
        future.seed = batch_seed,
        future.chunk.size = replication_chunk_size(length(batch_ids))
      ) |>
        dplyr::bind_rows()
    }
    joint_successes_among <- function(rows) {
      by_unit <- split(rows[c("replication", "mean_boarding_days")], rows$resource)
      merged <- merge(by_unit[["GenMed"]], by_unit[["ICU"]], by = "replication",
                       suffixes = c("_GenMed", "_ICU"))
      sum(merged$mean_boarding_days_GenMed <= thresholds[["GenMed"]] &
            merged$mean_boarding_days_ICU <= thresholds[["ICU"]])
    }

    required_successes <- ceiling(reliability_level * replications)
    # The margin target is only used to decide when growth/refinement can
    # stop early (see below); the reported passes/reliability always use
    # required_successes above.
    required_successes_margin <- ceiling(min(1, reliability_level + refinement_margin) * replications)
    if (stage == "search") {
      # Batches run in increasing chunks so the loop can stop as soon as the
      # joint accept/reject decision is already sealed; replications that
      # never run could not have changed it either way. Per-index streams
      # (above) make this exactly reproducible for whichever replications do run.
      # Early acceptance requires clearing the margin target, not just the
      # nominal one, so growth/refinement decisions (which use the margin)
      # are not starved of replications by an early stop keyed to the looser
      # nominal threshold.
      # Under acceptance_rule = "lower_ci", a high point-estimate margin with
      # few replications does NOT imply the (wider, small-n) confidence
      # interval will clear reliability_level -- so early ACCEPT is disabled
      # there; only early REJECT (already certain to fail, valid under either
      # rule since a lower confidence bound never exceeds the point estimate)
      # is kept, and the full replication count always runs otherwise. This
      # keeps evaluated_replications large enough for a fair lower_ci check.
      stream_seeds <- make_replication_stream_seeds(replications, evaluation_seed)
      # Batch size starts at one wave per worker (so a clearly bad candidate
      # can still be pruned via certain_to_fail after the very first batch)
      # and doubles each round after that. Under lower_ci, early accept is
      # disabled (see above) so most candidates that don't fail outright run
      # to the full replication count anyway; growing the batch geometrically
      # cuts the number of future_lapply dispatch round-trips needed to get
      # there without weakening the early-reject check.
      batch_size <- max(1L, workers)
      evaluated_replications <- 0L
      resource_batches <- list()
      repeat {
        remaining <- replications - evaluated_replications
        if (remaining <= 0L) break
        take <- min(batch_size, remaining)
        batch_ids <- seq.int(evaluated_replications + 1L, evaluated_replications + take)
        resource_batches[[length(resource_batches) + 1L]] <-
          run_batch(batch_ids, stream_seeds[batch_ids])
        evaluated_replications <- evaluated_replications + take
        successes_so_far <- joint_successes_among(dplyr::bind_rows(resource_batches))
        remaining <- replications - evaluated_replications
        certain_to_fail <- successes_so_far + remaining < required_successes
        certain_to_pass_margin <- acceptance_rule != "lower_ci" &&
          successes_so_far >= required_successes_margin
        if (certain_to_fail || certain_to_pass_margin) break
        batch_size <- batch_size * 2L
      }
      resources <- dplyr::bind_rows(resource_batches)
    } else {
      evaluated_replications <- replications
      resources <- run_batch(seq_len(replications), evaluation_seed)
    }

    maximum_occupancy <- stats::setNames(
      vapply(c("GenMed", "ICU"), function(unit_name) {
        unit_rows <- resources[resources$resource == unit_name, , drop = FALSE]
        if (nrow(unit_rows) == 0) return(0L)
        as.integer(ceiling(safe_max(unit_rows$maximum_occupied)))
      }, integer(1)),
      c("GenMed", "ICU")
    )

    boarding_for <- function(unit_name, column = "mean_boarding_days") {
      unit_rows <- resources[resources$resource == unit_name, , drop = FALSE]
      replications_found <- unit_rows$replication
      expected_replications <- seq_len(evaluated_replications)
      missing_replications <- setdiff(expected_replications, replications_found)
      unexpected_replications <- setdiff(replications_found, expected_replications)

      if (anyDuplicated(replications_found) ||
          length(missing_replications) > 0 ||
          length(unexpected_replications) > 0) {
        stop(
          sprintf(
            paste0(
              "Invalid replication monitoring for %s: expected %d unique replications; ",
              "found %d. Missing: %s. Unexpected: %s."
            ),
            unit_name,
            evaluated_replications,
            length(unique(replications_found)),
            if (length(missing_replications) == 0) "none" else paste(missing_replications, collapse = ", "),
            if (length(unexpected_replications) == 0) "none" else paste(unexpected_replications, collapse = ", ")
          ),
          call. = FALSE
        )
      }

      unit_rows[[column]][match(expected_replications, replications_found)]
    }
    boarding_matrix <- data.frame(
      replication = seq_len(evaluated_replications),
      GenMed = boarding_for("GenMed"),
      ICU = boarding_for("ICU"),
      check.names = FALSE
    )

    unit_pass <- data.frame(
      GenMed = boarding_matrix$GenMed <= thresholds[["GenMed"]],
      ICU = boarding_matrix$ICU <= thresholds[["ICU"]]
    )
    joint_pass <- unit_pass$GenMed & unit_pass$ICU
    unit_successful_replications <- c(
      GenMed = sum(unit_pass$GenMed),
      ICU = sum(unit_pass$ICU)
    )
    # Observed proportion among replications actually run. When a search
    # evaluation stops early this is the honest evidence available; the
    # accept/reject decision below still uses the full target count.
    unit_reliability <- unit_successful_replications / evaluated_replications
    # Acceptance counts replications where mean boarding time (per replication,
    # itself an exact per-replication average -- see boarding_state_summary())
    # stays within the limit in both units simultaneously.
    mean_boarding <- c(GenMed = mean(boarding_matrix$GenMed), ICU = mean(boarding_matrix$ICU))
    mean_intervals <- summarize_boarding_means(resources, thresholds)

    joint_successes <- sum(joint_pass)
    # Exact binomial lower bound on the joint proportion at acceptance_confidence;
    # always computed (cheap) so it is available for inspection/logging even
    # under the point-estimate rule, which does not use it to decide passes.
    joint_lower_ci <- stats::binom.test(joint_successes, evaluated_replications,
      conf.level = acceptance_confidence)$conf.int[[1]]
    passes <- if (acceptance_rule == "lower_ci") {
      isTRUE(joint_lower_ci >= reliability_level)
    } else {
      joint_successes >= required_successes
    }

    result <- list(
      capacities = candidate,
      maximum_occupancy = maximum_occupancy,
      queues = mean_boarding,
      mean_intervals = mean_intervals,
      unit_reliability = unit_reliability,
      reliability = safe_mean(as.numeric(joint_pass)),
      successful_replications = joint_successes,
      unit_successful_replications = unit_successful_replications,
      replications = evaluated_replications,
      thresholds = thresholds,
      acceptance_rule = acceptance_rule,
      joint_lower_ci = joint_lower_ci,
      passes = passes,
      # Internal-only: used to steer growth/refinement toward a capacity with
      # some margin above reliability_level, not part of the reported/holdout
      # acceptance criterion (result$passes, above, is unaffected by the margin;
      # it is affected by acceptance_rule, as computed above).
      passes_margin = joint_successes >= required_successes_margin
    )
    evaluation_id <- length(evaluation_history) + 1L
    evaluation_history[[evaluation_id]] <<- data.frame(
      evaluation_id = evaluation_id, stage = stage, stage_evaluation = stage_evaluation,
      seed = evaluation_seed, replications = evaluated_replications,
      target_replications = replications,
      GenMed = candidate[["GenMed"]], ICU = candidate[["ICU"]],
      added_beds = sum(candidate - initial_capacities),
      GenMed_threshold = thresholds[["GenMed"]], ICU_threshold = thresholds[["ICU"]],
      acceptance_criterion = "joint_maximum_boarding_time_GenMed_ICU",
      reliability_target = reliability_level,
      required_successes = required_successes,
      required_successes_margin = required_successes_margin,
      GenMed_mean_boarding_days = mean_boarding[["GenMed"]], ICU_mean_boarding_days = mean_boarding[["ICU"]],
      GenMed_mcse = mean_intervals$mcse[match("GenMed", mean_intervals$resource)],
      ICU_mcse = mean_intervals$mcse[match("ICU", mean_intervals$resource)],
      joint_successes = joint_successes, joint_reliability = result$reliability,
      joint_lower_ci = joint_lower_ci, acceptance_rule = acceptance_rule,
      acceptance_confidence = acceptance_confidence,
      GenMed_reliability = unit_reliability[["GenMed"]], ICU_reliability = unit_reliability[["ICU"]],
      passes = result$passes, passes_margin = result$passes_margin,
      elapsed_seconds = proc.time()[["elapsed"]] - evaluation_started)
    replication_history[[evaluation_id]] <<- resources |>
      dplyr::mutate(evaluation_id = evaluation_id, stage = stage, seed = evaluation_seed,
        threshold = unname(thresholds[.data$resource]),
        unit_pass = .data$mean_boarding_days <= .data$threshold,
        joint_pass = joint_pass[.data$replication])
    assign(key, result, envir = cache)

    if (verbose) {
      cat(sprintf(
        paste0(
          "%s evaluation %d: GenMed=%d (mean boarding %.2fd, %.0f%% peak compliance), ",
          "ICU=%d (mean boarding %.2fd, %.0f%% peak compliance), joint peak compliance %.0f%%, ",
          "pass=%s (margin pass=%s)\n"
        ),
        tools::toTitleCase(stage), stage_evaluation,
        candidate[["GenMed"]], result$queues[["GenMed"]],
        100 * result$unit_reliability[["GenMed"]],
        candidate[["ICU"]], result$queues[["ICU"]],
        100 * result$unit_reliability[["ICU"]],
        100 * result$reliability, result$passes, result$passes_margin
      ))
    }
    result
  }

  objective_weights <- c(GenMed = weight_GenMed, ICU = weight_ICU)
  total_added <- function(candidate) {
    sum((candidate[c("GenMed", "ICU")] - initial_capacities) * objective_weights)
  }

  target_units <- c("GenMed", "ICU")
  total_generated_patients <- as.integer(ceiling(duration * n_patients))
  if (!is.null(baseline) && isTRUE(baseline$enabled)) {
    # A safe bound includes every civilian scheduled during warm-up and follow-up.
    total_generated_patients <- total_generated_patients + sum(ceiling(
      baseline$arrival_rates * (baseline$warmup_max_days + sim_days)))
  }
  poisson_arrivals <- arrival_process == "poisson" ||
    (!is.null(baseline) && isTRUE(baseline$enabled) && identical(baseline$arrival_process, "poisson"))
  if (poisson_arrivals) {
    # Poisson counts have no finite absolute bound. This is a numerical search
    # limit, not a guarantee of unconstrained demand or global optimality.
    total_generated_patients <- as.integer(stats::qpois(1 - 1e-10, total_generated_patients))
  }
  unlimited_capacity_value <- max(500L, total_generated_patients)
  unlimited_simulation_used <- FALSE
  unlimited_max_occupancy <- stats::setNames(
    rep(NA_integer_, length(configured_capacities)),
    names(configured_capacities)
  )
  unlimited_reference_capacities <- initial_capacities
  safety_capacities <- pmax(
    initial_capacities,
    stats::setNames(rep(total_generated_patients, length(target_units)), target_units)
  )
  names(safety_capacities) <- target_units
  frontier_complete <- FALSE
  current_capacity_validated <- FALSE

  # use_margin = TRUE drives growth/refinement toward reliability_level +
  # refinement_margin (see evaluate()); use_margin = FALSE (default) reflects
  # the reported/holdout-triggering criterion.
  failing_units_for <- function(evaluation_result, use_margin = FALSE) {
    if (is.null(evaluation_result)) return(target_units)
    pass_field <- if (use_margin) "passes_margin" else "passes"
    if (isTRUE(evaluation_result[[pass_field]])) return(character())
    names(evaluation_result$unit_reliability)[evaluation_result$unit_reliability < 1]
  }

  estimate_unlimited_demand <- function(replications = num_sims) {
    auxiliary_started <- proc.time()[["elapsed"]]
    simulation_args <- list(
      capacities = stats::setNames(
        rep(unlimited_capacity_value, length(configured_capacities)),
        names(configured_capacities)),
      duration = duration, n_patients = n_patients, sim_days = sim_days,
      patient_profiles = patient_profiles, profile_prob = profile_prob,
      fallbacks = fallbacks, baseline = baseline,
      warmup_capacities = warmup_capacities, arrival_process = arrival_process)
    observed <- future.apply::future_lapply(
      seq_len(replications),
      unlimited_demand_replication,
      simulation_args = simulation_args, units = names(configured_capacities),
      future.globals = worker_dependencies$globals,
      future.packages = worker_dependencies$packages,
      future.seed = search_seed + 100000L,
      future.chunk.size = replication_chunk_size(replications)
    ) |>
      dplyr::bind_rows() |>
      dplyr::group_by(.data$resource) |>
      dplyr::summarise(
        maximum_occupied = max(.data$maximum_occupied),
        .groups = "drop"
      )

    maximum_occupancy <- stats::setNames(
      rep(0L, length(configured_capacities)),
      names(configured_capacities)
    )
    maximum_occupancy[observed$resource] <- observed$maximum_occupied
    auxiliary_history[[length(auxiliary_history) + 1L]] <<- data.frame(
      stage = "unlimited_demand", seed = search_seed + 100000L,
      replications = replications, capacity_per_unit = unlimited_capacity_value,
      elapsed_seconds = proc.time()[["elapsed"]] - auxiliary_started)
    maximum_occupancy
  }
  # Current capacity is evaluated once on the same bank as every other candidate.
  capacities <- initial_capacities
  result <- evaluate(capacities)
  current_capacity_validated <- !is.null(result) && isTRUE(result$passes_margin)
  if (current_capacity_validated) frontier_complete <- TRUE

  if (!current_capacity_validated) {
    reference_result <- result
    active_units <- failing_units_for(reference_result)
    if (length(active_units) == 0) active_units <- target_units

    # Estimate unconstrained primary demand with enough beds to exceed the
    # total number of arrivals. This is a reference, not a hard ceiling,
    # because constrained upstream units can route additional patients through
    # fallbacks to GenMed or ICU.
    if (initialization == "incremental") {
      unlimited_max_occupancy <- estimate_unlimited_demand()
      unlimited_simulation_used <- TRUE
    }
    unlimited_reference_capacities <- pmax(
      initial_capacities,
      if (unlimited_simulation_used) as.integer(unlimited_max_occupancy[target_units]) else initial_capacities
    )
    names(unlimited_reference_capacities) <- target_units

    if (verbose && unlimited_simulation_used) {
      demand_text <- paste0(
        names(unlimited_max_occupancy),
        "=",
        unlimited_max_occupancy,
        collapse = ", "
      )
      cat(sprintf(
        "Unlimited-capacity demand (%d beds per unit): %s\n",
        unlimited_capacity_value,
        demand_text
      ))
      cat(sprintf(
        "Fallback-safe search ceiling: GenMed=%d, ICU=%d\n",
        safety_capacities[["GenMed"]],
        safety_capacities[["ICU"]]
      ))
    }

    capacities <- initial_capacities
    result <- reference_result
    if (initialization == "analytical") {
      trial <- pmin(safety_capacities, analytical_start)
      if (any(trial > capacities)) {
        trial_result <- evaluate(trial)
        if (!is.null(trial_result)) {
          capacities <- trial
          result <- trial_result
        }
      }
    } else if (initialization == "incremental") {
      # Seed near the simulated unconstrained-demand peak instead of growing
      # from the current (typically much smaller) capacity one doubling step
      # at a time. The refinement phase below still shrinks toward the true
      # minimum from here; this only changes the starting point, not the
      # acceptance criterion or the refinement logic.
      trial <- pmin(safety_capacities,
        pmax(initial_capacities, ceiling(demand_safety_factor * unlimited_reference_capacities)))
      if (any(trial > capacities)) {
        trial_result <- evaluate(trial)
        if (!is.null(trial_result)) {
          capacities <- trial
          result <- trial_result
        }
      }
    }
    growth_steps <- stats::setNames(
      rep(as.integer(minimum_step), length(target_units)),
      target_units
    )

    # Grow only units that currently fail. The increments double after each
    # evaluation and are clipped at demand observed with unlimited capacity.
    while (!is.null(result) && !result$passes_margin) {
      failing_units <- failing_units_for(result, use_margin = TRUE)
      active_units <- union(active_units, failing_units)
      failing_units <- intersect(
        failing_units,
        target_units[capacities[target_units] < safety_capacities[target_units]]
      )
      if (length(failing_units) == 0) break

      trial <- capacities
      trial[failing_units] <- pmin(
        safety_capacities[failing_units],
        trial[failing_units] + growth_steps[failing_units]
      )
      if (identical(as.integer(trial), as.integer(capacities))) break

      trial_result <- evaluate(trial)
      if (is.null(trial_result)) break
      capacities <- trial
      result <- trial_result
      growth_steps[failing_units] <- pmin(
        pmax(0L, safety_capacities[failing_units] - initial_capacities[failing_units]),
        pmax(minimum_step, 2L * growth_steps[failing_units])
      )
    }
    # Reduce each resource with bounded binary searches, then look for
    # GenMed<->ICU trade-offs using an adaptive step (see search_minimal_unit
    # and trade_direction below) instead of a small fixed neighborhood.
    if (!is.null(result) && result$passes) {
      refine_unit <- function(unit_name, current_capacities, current_result) {
        lower <- initial_capacities[[unit_name]]
        upper <- current_capacities[[unit_name]]
        best_capacities <- current_capacities
        best_result <- current_result
        complete <- TRUE

        while (lower < upper) {
          midpoint <- floor((lower + upper) / 2)
          trial <- best_capacities
          trial[[unit_name]] <- midpoint
          trial_result <- evaluate(trial)
          if (is.null(trial_result)) {
            complete <- FALSE
            break
          }
          if (trial_result$passes_margin) {
            best_capacities <- trial
            best_result <- trial_result
            upper <- midpoint
          } else {
            lower <- midpoint + 1L
          }
        }

        list(
          capacities = best_capacities,
          result = best_result,
          complete = complete
        )
      }

      refinement_status <- logical()
      refinement_order <- c("ICU", "GenMed", "ICU")
      for (unit_name in refinement_order) {
        refined <- refine_unit(unit_name, capacities, result)
        capacities <- refined$capacities
        result <- refined$result
        refinement_status <- c(refinement_status, refined$complete)
        if (!refined$complete) break
      }

      local_complete <- all(refinement_status)
      if (local_complete) {
        # Exponential ("galloping") search for the smallest feasible
        # `unit_name` capacity at or above floor_value, then bisect within
        # the bracket found. Avoids probing near `ceiling` (safety_capacities,
        # often far larger than needed) unless the search actually needs it.
        search_minimal_unit <- function(unit_name, base_capacities, floor_value, ceiling_value) {
          # Check the ceiling first: if even the maximum allowed `unit_name`
          # capacity does not pass, every smaller value fails too (monotone
          # in `unit_name`), so the exponential ramp-up below would only
          # rediscover that after ~log2(ceiling - floor) wasted evaluations.
          # This matters most from `trade_direction`, which calls this
          # repeatedly for compensations that often cannot succeed at all.
          if (ceiling_value > floor_value) {
            ceiling_trial <- base_capacities
            ceiling_trial[[unit_name]] <- ceiling_value
            ceiling_result <- evaluate(ceiling_trial)
            if (is.null(ceiling_result)) {
              return(list(capacities = base_capacities, result = NULL, feasible = NA, complete = FALSE))
            }
            if (!ceiling_result$passes_margin) {
              return(list(capacities = ceiling_trial, result = ceiling_result, feasible = FALSE, complete = TRUE))
            }
          }
          increment <- 1L
          lo <- floor_value - 1L
          hi <- NA_integer_
          hi_result <- NULL
          probe_value <- floor_value
          repeat {
            trial <- base_capacities
            trial[[unit_name]] <- probe_value
            probe_result <- evaluate(trial)
            if (is.null(probe_result)) {
              return(list(capacities = base_capacities, result = NULL, feasible = NA, complete = FALSE))
            }
            if (probe_result$passes_margin) {
              hi <- probe_value
              hi_result <- probe_result
              break
            }
            lo <- probe_value
            if (probe_value >= ceiling_value) {
              return(list(capacities = trial, result = probe_result, feasible = FALSE, complete = TRUE))
            }
            increment <- increment * 2L
            probe_value <- min(ceiling_value, probe_value + increment)
          }
          best_capacities <- base_capacities
          best_capacities[[unit_name]] <- hi
          best_result <- hi_result
          while (lo + 1L < hi) {
            midpoint <- lo + (hi - lo) %/% 2L
            trial <- base_capacities
            trial[[unit_name]] <- midpoint
            trial_result <- evaluate(trial)
            if (is.null(trial_result)) {
              return(list(capacities = best_capacities, result = best_result, feasible = TRUE, complete = FALSE))
            }
            if (trial_result$passes_margin) {
              hi <- midpoint
              best_capacities <- trial
              best_result <- trial_result
            } else {
              lo <- midpoint
            }
          }
          list(capacities = best_capacities, result = best_result, feasible = TRUE, complete = TRUE)
        }

        # Reduces from_unit by a shrinking step (starting at the largest
        # power of two within its current headroom above initial_capacities)
        # and re-searches to_unit (upward from its current value) for the
        # smallest capacity that compensates. A step is kept only when the
        # resulting weighted total_added() is strictly lower; on success the
        # same step size is retried (push further), otherwise it halves.
        trade_direction <- function(from_unit, to_unit, current_capacities, current_result) {
          best_capacities <- current_capacities
          best_result <- current_result
          complete <- TRUE
          improved <- FALSE
          headroom <- best_capacities[[from_unit]] - initial_capacities[[from_unit]]
          if (headroom > 0) {
            step <- 2L^floor(log2(headroom))
            while (step >= 1L) {
              candidate_from <- max(initial_capacities[[from_unit]], best_capacities[[from_unit]] - step)
              if (candidate_from < best_capacities[[from_unit]]) {
                base_capacities <- best_capacities
                base_capacities[[from_unit]] <- candidate_from
                search <- search_minimal_unit(to_unit, base_capacities,
                  floor_value = best_capacities[[to_unit]], ceiling_value = safety_capacities[[to_unit]])
                if (!search$complete) {
                  complete <- FALSE
                  break
                }
                if (isTRUE(search$feasible) &&
                    total_added(search$capacities) < total_added(best_capacities)) {
                  best_capacities <- search$capacities
                  best_result <- search$result
                  improved <- TRUE
                  next
                }
              }
              step <- step %/% 2L
            }
          }
          list(capacities = best_capacities, result = best_result, complete = complete, improved = improved)
        }

        trade_complete <- TRUE
        repeat {
          pass_improved <- FALSE
          for (pair in list(c("GenMed", "ICU"), c("ICU", "GenMed"))) {
            traded <- trade_direction(pair[[1]], pair[[2]], capacities, result)
            capacities <- traded$capacities
            result <- traded$result
            if (traded$improved) pass_improved <- TRUE
            if (!traded$complete) {
              trade_complete <- FALSE
              break
            }
          }
          if (!trade_complete || !pass_improved) break
        }
        local_complete <- trade_complete
      }
      frontier_complete <- all(refinement_status) && local_complete
    }

  }

  selection_result <- result
  if (!is.null(result) && isTRUE(result$passes)) {
    # Never tune capacity using this independent final bank.
    result <- evaluate(capacities, replications = final_num_sims, stage = "holdout")
  }
  final_intervals <- if (final_evaluation_count > 0L) {
    dplyr::bind_rows(lapply(target_units, function(unit) {
      interval <- stats::binom.test(result$unit_successful_replications[[unit]], result$replications)$conf.int
      data.frame(resource = unit, successes = result$unit_successful_replications[[unit]],
        replications = result$replications, probability = result$unit_reliability[[unit]],
        lower_95 = interval[[1]], upper_95 = interval[[2]])
    }))
  } else data.frame()
  final_joint_interval <- if (final_evaluation_count > 0L) {
    interval <- stats::binom.test(result$successful_replications, result$replications)$conf.int
    data.frame(criterion = "GenMed_and_ICU", successes = result$successful_replications,
      replications = result$replications, probability = result$reliability,
      lower_95 = interval[[1]], upper_95 = interval[[2]])
  } else data.frame()
  if (is.null(result)) {
    result <- list(
      queues = c(GenMed = NA_real_, ICU = NA_real_),
      unit_reliability = c(GenMed = NA_real_, ICU = NA_real_),
      reliability = NA_real_,
      joint_lower_ci = NA_real_,
      passes = FALSE
    )
  }

  optimization_result <- list(
    avg_boarding_days_GenMed = result$queues[["GenMed"]],
    avg_boarding_days_ICU = result$queues[["ICU"]],
    reliability_GenMed = result$unit_reliability[["GenMed"]],
    reliability_ICU = result$unit_reliability[["ICU"]],
    joint_reliability = result$reliability,
    joint_lower_ci = result$joint_lower_ci,
    reliability_level = reliability_level,
    reliability_diagnostic = "maximum_boarding_time_below_limit_per_replication",
    N_added = capacities[["GenMed"]] - initial_capacities[["GenMed"]],
    N_added_ICU = capacities[["ICU"]] - initial_capacities[["ICU"]],
    GenMed_N = capacities[["GenMed"]],
    ICU_N = capacities[["ICU"]],
    estimated_GenMed_N = estimated_capacities[["GenMed"]],
    estimated_ICU_N = estimated_capacities[["ICU"]],
    expected_peak_GenMed = expected_peak[["GenMed"]],
    expected_peak_ICU = expected_peak[["ICU"]],
    analytical_reference_scope = "surge_only; excludes routine civilian demand",
    initialization = initialization,
    analytical_start = analytical_start,
    routine_offered_load = routine_load,
    estimated_additional_GenMed = estimated_additional_beds[["GenMed"]],
    estimated_additional_ICU = estimated_additional_beds[["ICU"]],
    unlimited_simulation_used = unlimited_simulation_used,
    unlimited_capacity_value = unlimited_capacity_value,
    unlimited_max_occupancy_GenMed = unlimited_max_occupancy[["GenMed"]],
    unlimited_max_occupancy_ICU = unlimited_max_occupancy[["ICU"]],
    unlimited_reference_GenMed = unlimited_reference_capacities[["GenMed"]],
    unlimited_reference_ICU = unlimited_reference_capacities[["ICU"]],
    safety_capacity_GenMed = safety_capacities[["GenMed"]],
    safety_capacity_ICU = safety_capacities[["ICU"]],
    search_evaluations = search_evaluation_count,
    final_evaluations = final_evaluation_count,
    final_num_sims = final_num_sims,
    final_intervals = final_intervals,
    final_joint_interval = final_joint_interval,
    final_mean_intervals = if (final_evaluation_count > 0L) result$mean_intervals else data.frame(),
    acceptance_criterion = "joint_maximum_boarding_time_GenMed_ICU",
    mean_boarding_days_GenMed = result$queues[["GenMed"]],
    mean_boarding_days_ICU = result$queues[["ICU"]],
    evaluation_history = dplyr::bind_rows(evaluation_history),
    replication_history = dplyr::bind_rows(replication_history),
    auxiliary_history = dplyr::bind_rows(auxiliary_history),
    cache_hits = cache_hits,
    total_simulations = sum(vapply(c(evaluation_history, auxiliary_history),
      function(x) x$replications[[1]], numeric(1))),
    search_configuration = list(capacities = configured_capacities,
      warmup_capacities = warmup_capacities, baseline = baseline,
      patient_profiles = patient_profiles, profile_prob = profile_prob, fallbacks = fallbacks,
      duration = duration, n_patients = n_patients, sim_days = sim_days,
      arrival_process = arrival_process, search_seed = search_seed,
      initialization = initialization,
      num_sims = num_sims, final_num_sims = final_num_sims,
      max_evaluations = max_evaluations,
      minimum_step = minimum_step, demand_safety_factor = demand_safety_factor,
      reliability_level = reliability_level, refinement_margin = refinement_margin,
      boarding_time_limit_GenMed = boarding_time_limit_GenMed, boarding_time_limit_ICU = boarding_time_limit_ICU,
      weight_GenMed = weight_GenMed, weight_ICU = weight_ICU,
      acceptance_rule = acceptance_rule, acceptance_confidence = acceptance_confidence,
      workers = workers,
      rng_kind = RNGkind(), future_version = as.character(utils::packageVersion("future.apply"))),
    selection_passed = !is.null(selection_result) && isTRUE(selection_result$passes),
    arrival_process = arrival_process,
    capacity_bound_scope = if (poisson_arrivals) "Poisson numerical search limit" else "Total scheduled arrivals",
    evaluations = search_evaluation_count + final_evaluation_count,
    converged = isTRUE(result$passes),
    refinement_complete = frontier_complete,
    search_complete = isTRUE(result$passes)
  )
  # Wall-clock duration includes setup, all candidate evaluations and holdout.
  optimization_result$optimization_elapsed_seconds <-
    proc.time()[["elapsed"]] - optimization_started
  if (verbose) {
    cat(sprintf("Total optimization time: %.2f seconds (%.2f minutes).\n",
      optimization_result$optimization_elapsed_seconds,
      optimization_result$optimization_elapsed_seconds / 60))
  }
  optimization_result
}
