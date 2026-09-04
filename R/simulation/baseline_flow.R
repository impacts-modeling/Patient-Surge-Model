# Civilian flow functions have no Shiny dependency. Rates are patients/day.
baseline_defaults <- function() {
  list(enabled = FALSE, profiles = list(), arrival_rates = numeric(),
       warmup_min_days = 110, warmup_max_days = 365, window_days = 14,
       occupancy_tolerance = 0.2, queue_tolerance = 0.5,
       recheck_interval_days = 1)
}

validate_baseline_config <- function(config, capacities, fallbacks = list()) {
  if (is.null(config) || !isTRUE(config$enabled)) return(invisible(TRUE))
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
  settings <- unlist(config[c("warmup_min_days", "warmup_max_days", "window_days",
                             "occupancy_tolerance", "queue_tolerance", "recheck_interval_days")])
  if (length(settings) != 6L || any(!is.finite(settings)) || any(settings <= 0) ||
      config$warmup_min_days < 3 * config$window_days ||
      config$warmup_max_days < config$warmup_min_days) {
    stop("Warm-up needs positive settings, a minimum of three windows, and maximum >= minimum.")
  }
  invisible(TRUE)
}

make_baseline_arrivals <- function(config, until) {
  if (!isTRUE(config$enabled)) {
    return(data.frame(profile = character(), arrival_time = numeric(), population = character()))
  }
  dplyr::bind_rows(lapply(names(config$profiles), function(profile) {
    rate <- config$arrival_rates[[profile]]
    times <- (seq_len(ceiling(until * rate)) - 1) / rate
    data.frame(profile = profile, arrival_time = times[times < until], population = "civilian")
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

# A reproducible screening diagnostic, not a proof of statistical stationarity.
baseline_warmup_diagnostic <- function(resources, capacities, time, config) {
  resources <- initialize_resource_history(resources, capacities)
  windows <- dplyr::bind_rows(lapply(seq_len(3), function(index) {
    start <- time - (4 - index) * config$window_days
    rows <- slice_resource_history(resources, start, start + config$window_days)
    rows |>
      dplyr::group_by(.data$resource) |>
      dplyr::arrange(.data$time, .by_group = TRUE) |>
      dplyr::mutate(dt = dplyr::lead(.data$time, default = config$window_days) - .data$time) |>
      dplyr::summarise(occupied = sum(.data$server * .data$dt) / config$window_days,
                       queue = sum(.data$queue * .data$dt) / config$window_days,
                       .groups = "drop") |>
      dplyr::mutate(window = index)
  }))
  windows |>
    dplyr::group_by(.data$resource) |>
    dplyr::summarise(occupied_range = diff(range(.data$occupied)),
                     queue_range = diff(range(.data$queue)),
                     last_mean_occupied = dplyr::last(.data$occupied),
                     last_mean_queue = dplyr::last(.data$queue), .groups = "drop") |>
    dplyr::mutate(check_time = time,
                  occupancy_limit = config$occupancy_tolerance * pmax(1, capacities[.data$resource]),
                  queue_limit = config$queue_tolerance,
                  passed = .data$occupied_range <= .data$occupancy_limit &
                    .data$queue_range <= .data$queue_limit)
}

run_baseline_simulation <- function(capacities, duration, n_patients, sim_days,
                                    patient_profiles, profile_prob, fallbacks, baseline) {
  validate_baseline_config(baseline, capacities, fallbacks)
  if (n_patients > 0 && duration > 0) {
    validate_patient_configuration(capacities, patient_profiles, profile_prob, fallbacks)
  }
  stopifnot(sim_days > 0, duration >= 0, duration == floor(duration),
            n_patients >= 0, n_patients == floor(n_patients))
  env <- simmer::simmer("civilian-and-surge")
  for (unit in names(capacities)) {
    env <- simmer::add_resource(env, unit, capacity = as.integer(capacities[[unit]]), queue_size = Inf)
    env <- simmer::add_resource(env, logical_queue_resource(unit), capacity = Inf, queue_size = 0)
  }
  schedule <- make_baseline_arrivals(baseline, baseline$warmup_max_days + sim_days)
  registry <- list()
  add_population <- function(population, profiles, arrivals) {
    for (index in seq_along(profiles)) {
      profile <- names(profiles)[[index]]
      times <- sort(arrivals$arrival_time[arrivals$profile == profile])
      if (!length(times)) next
      prefix <- paste0(population, "_", index, "_")
      trajectory <- profile_trajectory(env, profile, profiles, fallbacks,
                                        baseline$recheck_interval_days)
      # simmer::at supplies delays relative to generator creation, not absolute time.
      env <<- simmer::add_generator(env, prefix, trajectory, simmer::at(times - simmer::now(env)))
      registry[[length(registry) + 1L]] <<- data.frame(
        name = paste0(prefix, seq_along(times) - 1L), profile = profile,
        population = population, scheduled_arrival = times)
    }
  }
  add_population("civilian", baseline$profiles, schedule)
  diagnostics <- list()
  check_times <- unique(c(seq(baseline$warmup_min_days, baseline$warmup_max_days,
                              by = baseline$window_days), baseline$warmup_max_days))
  for (check_time in check_times) {
    env <- simmer::run(env, until = check_time)
    diagnostic <- baseline_warmup_diagnostic(collect_hospital_resources(env), capacities,
                                             check_time, baseline)
    diagnostics[[length(diagnostics) + 1L]] <- diagnostic
    if (all(diagnostic$passed)) break
  }
  if (!all(diagnostic$passed)) {
    stop("Civilian warm-up did not pass the stability screen before the maximum duration. ",
         "Review demand/capacity, lengthen warm-up, or revise the documented tolerances. ",
         "The observation period was not started.")
  }
  surge_start <- check_time
  surge <- base::expand.grid(day = seq_len(duration), patient = seq_len(n_patients))
  surge$arrival_time <- surge_start + surge$day - 1 + (surge$patient - 1) / max(1, n_patients)
  surge$profile <- if (nrow(surge)) {
    base::sample(names(profile_prob), nrow(surge), replace = TRUE, prob = profile_prob)
  } else character()
  add_population("surge", patient_profiles, surge)
  env <- simmer::run(env, until = surge_start + sim_days)
  result <- simmer::wrap(env)
  attr(result, "civilian_metadata") <- list(
    surge_start = surge_start, observation_end = surge_start + sim_days, capacities = capacities,
    patients = dplyr::bind_rows(registry),
    warmup_diagnostics = dplyr::bind_rows(diagnostics), baseline = baseline)
  result
}
