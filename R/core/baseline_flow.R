# Civilian flow functions have no Shiny dependency. Rates are patients/day.
baseline_defaults <- function() {
  list(enabled = FALSE, profiles = list(), arrival_rates = numeric(),
       arrival_process = "poisson", warmup_days = 50,
       recheck_interval_days = 1)
}

validate_baseline_config <- function(config, capacities, fallbacks = list()) {
  if (is.null(config) || !isTRUE(config$enabled)) return(invisible(TRUE))
  if (!is.null(config$arrival_process)) match.arg(config$arrival_process, c("even", "poisson"))
  rates <- config$arrival_rates
  if (!is.numeric(rates) || length(rates) == 0 || any(!is.finite(rates)) ||
      any(rates <= 0) || is.null(names(rates)) || any(!nzchar(names(rates))) ||
      anyDuplicated(names(rates)) || anyDuplicated(names(config$profiles)) ||
      !setequal(names(rates), names(config$profiles))) {
    stop("Civilian profiles need unique names and positive finite arrival rates.")
  }
  validate_patient_configuration(capacities, config$profiles, rates / sum(rates), fallbacks)
  for (profile in config$profiles) {
    if (length(profile$unit) == 0 || length(profile$unit) != length(profile$los) ||
        any(!is.finite(profile$los)) || any(profile$los <= 0)) {
      stop("Each civilian profile needs an ordered pathway and positive mean stays.")
    }
  }
  settings <- unlist(config[c("warmup_days", "recheck_interval_days")])
  if (length(settings) != 2L || any(!is.finite(settings)) || any(settings <= 0)) {
    stop("Warm-up duration and recheck interval must be positive finite numbers.")
  }
  invisible(TRUE)
}

make_baseline_arrivals <- function(config, until) {
  if (!isTRUE(config$enabled)) {
    return(data.frame(profile = character(), arrival_time = numeric(), population = character()))
  }
  process <- if (is.null(config$arrival_process)) "poisson" else config$arrival_process
  seeds <- sample.int(.Machine$integer.max, length(config$profiles))
  dplyr::bind_rows(lapply(seq_along(config$profiles), function(index) {
    profile <- names(config$profiles)[[index]]
    rate <- config$arrival_rates[[profile]]
    times <- with_simulation_seed(seeds[[index]], make_arrival_times(rate, until, process))
    data.frame(profile = rep(profile, length(times)), arrival_time = times,
               population = rep("civilian", length(times)))
  })) |>
    dplyr::arrange(.data$arrival_time, .data$profile)
}

# Insert a left-boundary state and a terminal state; preserve event changes.
initialize_resource_history <- function(resources, capacities) {
  initial <- data.frame(resource = names(capacities), time = 0, server = 0, queue = 0,
                         capacity = as.numeric(capacities), queue_size = Inf, system = 0, limit = Inf)
  if ("replication" %in% names(resources)) initial$replication <- 1L
  dplyr::bind_rows(initial, resources) |>
    dplyr::arrange(.data$time, .data$resource)
}

slice_resource_history <- function(resources, start, end, shift = start) {
  if (!nrow(resources)) return(resources)
  dplyr::bind_rows(lapply(split(resources, resources$resource), function(rows) {
    rows <- rows[order(rows$time), , drop = FALSE]
    initial <- rows[max(c(1L, which(rows$time <= start))), , drop = FALSE]
    if (!any(rows$time <= start)) initial[c("server", "queue", "system")] <- 0
    initial$time <- start
    terminal <- rows[max(c(1L, which(rows$time <= end))), , drop = FALSE]
    terminal$time <- end
    result <- dplyr::bind_rows(initial, rows[rows$time > start & rows$time < end, ], terminal)
    result$time <- result$time - shift
    result
  })) |>
    dplyr::arrange(.data$time, .data$resource)
}

# The warm-up length is a fixed modeling choice (baseline$warmup_days), not the
# result of an automated stability test; justify it from raw-occupancy plots
# (see paper/plot_raw_dynamics.R) rather than from a pass/fail screen.
run_baseline_simulation <- function(capacities, duration, n_patients, sim_days,
                                    patient_profiles, profile_prob, fallbacks, baseline,
                                    warmup_capacities = capacities, arrival_process = "even",
                                    monitor_patients = TRUE, monitor_ed = TRUE) {
  stopifnot(is.logical(monitor_patients), length(monitor_patients) == 1L,
            !is.na(monitor_patients),
            is.logical(monitor_ed), length(monitor_ed) == 1L, !is.na(monitor_ed))
  stopifnot(
    is.numeric(warmup_capacities),
    setequal(names(warmup_capacities), names(capacities)),
    all(is.finite(warmup_capacities)),
    all(warmup_capacities >= 0),
    all(warmup_capacities == floor(warmup_capacities)),
    all(capacities[names(warmup_capacities)] >= warmup_capacities)
  )
  validate_baseline_config(baseline, warmup_capacities, fallbacks)
  if (n_patients > 0 && duration > 0) {
    validate_patient_configuration(capacities, patient_profiles, profile_prob, fallbacks)
  }
  stopifnot(sim_days > 0, duration >= 0, duration == floor(duration),
            n_patients >= 0, arrival_process == "poisson" || n_patients == floor(n_patients))
  env <- simmer::simmer("civilian-and-surge")
  dispatcher <- new_bed_dispatcher(env, names(capacities))
  attr(env, "bed_dispatcher") <- dispatcher
  for (unit in names(warmup_capacities)) {
    mon <- should_monitor_resource(unit, monitor_ed)
    env <- simmer::add_resource(env, unit, capacity = as.integer(warmup_capacities[[unit]]),
                                queue_size = Inf, mon = mon)
    env <- simmer::add_resource(env, logical_queue_resource(unit), capacity = Inf, queue_size = 0, mon = mon)
    env <- simmer::add_resource(env, boarding_resource(unit), capacity = Inf, queue_size = 0, mon = mon)
  }
  seeds <- sample.int(.Machine$integer.max, 4L)
  schedule <- with_simulation_seed(seeds[[1]],
    make_baseline_arrivals(baseline, baseline$warmup_days + sim_days))
  # Sample the event before warm-up; changing capacity cannot change its mix.
  surge <- with_simulation_seed(seeds[[3]],
    make_surge_arrivals(n_patients, duration, profile_prob, arrival_process))
  registry <- list(data.frame(name = character(), profile = character(),
                              population = character(), scheduled_arrival = numeric()))
  add_population <- function(population, profiles, arrivals, service_seed) {
    service_seeds <- with_simulation_seed(service_seed,
      sample.int(.Machine$integer.max, length(profiles)))
    for (index in seq_along(profiles)) {
      profile <- names(profiles)[[index]]
      times <- sort(arrivals$arrival_time[arrivals$profile == profile])
      if (!length(times)) next
      prefix <- paste0(population, "_", index, "_")
      trajectory <- profile_trajectory(env, profile, profiles, fallbacks,
                                        baseline$recheck_interval_days,
                                        make_service_times(length(times), profiles[[profile]], service_seeds[[index]]))
      # simmer::at supplies delays relative to generator creation, not absolute time.
      env <<- simmer::add_generator(env, prefix, trajectory,
        simmer::at(times - simmer::now(env)), mon = monitor_patients)
      registry[[length(registry) + 1L]] <<- data.frame(
        name = paste0(prefix, seq_along(times) - 1L), profile = profile,
        population = population, scheduled_arrival = times)
    }
  }
  add_population("civilian", baseline$profiles, schedule, seeds[[2]])
  surge_start <- baseline$warmup_days
  env <- simmer::run(env, until = surge_start)
  # Capacity added for the event becomes available only after the civilian warm-up.
  changed_units <- names(capacities)[capacities != warmup_capacities[names(capacities)]]
  if (length(changed_units)) {
    activation <- simmer::trajectory("activate_event_capacity")
    for (unit in changed_units) {
      activation <- simmer::set_capacity(
        activation, unit, as.integer(capacities[[unit]])
      )
    }
    activation <- simmer::send(activation, dispatcher$dispatch)
    env <- simmer::add_generator(
      env, ".activate_event_capacity_", activation,
      simmer::at(surge_start - simmer::now(env)), mon = FALSE
    )
    # Process the zero-time activation before registering surge arrivals.
    activation_steps <- 0L
    while (any(vapply(changed_units, function(unit) {
      simmer::get_capacity(env, unit) != as.integer(capacities[[unit]])
    }, logical(1)))) {
      env <- simmer::stepn(env, 1)
      activation_steps <- activation_steps + 1L
      if (activation_steps > 10000L || simmer::now(env) > surge_start) {
        stop("Event capacity could not be activated at surge onset.", call. = FALSE)
      }
    }
    if (simmer::now(env) != surge_start) {
      stop("Event capacity was activated outside surge onset.", call. = FALSE)
    }
  }
  surge$arrival_time <- surge$arrival_time + surge_start
  add_population("surge", patient_profiles, surge, seeds[[4]])
  env <- simmer::run(env, until = surge_start + sim_days)
  result <- simmer::wrap(env)
  attr(result, "civilian_metadata") <- list(
    surge_start = surge_start, observation_end = surge_start + sim_days, capacities = capacities,
    warmup_capacities = warmup_capacities,
    patients = dplyr::bind_rows(registry), baseline = baseline)
  result
}
