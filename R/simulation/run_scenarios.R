# Source hospital_trajectory.R, baseline_flow.R and this file to run without Shiny.
# Parameters and returned raw tables can be saved with saveRDS for paper figures.
run_hospital_scenario <- function(config, duration, n_patients, sim_days,
                                  num_sims = 1L, seed = 2026L, scenario_id = "scenario",
                                  scenario_mode = c("surge", "civilian_only")) {
  scenario_mode <- match.arg(scenario_mode)
  # Explicit mode ignores surge settings, while the zero-arrival API remains supported.
  if (scenario_mode == "civilian_only") {
    duration <- 0
    n_patients <- 0
  }
  stopifnot(length(num_sims) == 1, num_sims >= 1, num_sims == floor(num_sims),
            length(seed) == 1, is.finite(seed), abs(seed) <= .Machine$integer.max,
            length(scenario_id) == 1, !is.na(scenario_id),
            length(duration) == 1, is.finite(duration), duration >= 0, duration == floor(duration),
            length(n_patients) == 1, is.finite(n_patients), n_patients >= 0,
            n_patients == floor(n_patients), length(sim_days) == 1, is.finite(sim_days), sim_days > 0)
  baseline <- config$baseline
  if (is.null(baseline)) baseline <- baseline_defaults()
  if (n_patients == 0 || duration == 0) scenario_mode <- "civilian_only"
  if (scenario_mode == "civilian_only" && !isTRUE(baseline$enabled)) {
    stop("Enable Routine Civilian Flow and configure civilian profiles before running civilian-only operations.")
  }
  validate_baseline_config(baseline, config$capacities, config$fallbacks)
  results <- future.apply::future_lapply(seq_len(num_sims), function(replication) {
    sim <- run_simulation(config$capacities, duration, n_patients, sim_days,
                          config$patient_profiles, config$profile_prob, config$fallbacks,
                          baseline = baseline)
    metadata <- attr(sim, "civilian_metadata")
    decorate <- function(rows) {
      dplyr::mutate(rows, replication = .env$replication, scenario_id = .env$scenario_id,
                    scenario_mode = .env$scenario_mode)
    }
    arrivals <- get_hospital_mon_arrivals(sim)
    if (!is.null(metadata)) {
      # Neutral name for comparisons; retain present_at_surge for existing scripts.
      arrivals$present_at_observation_start <- arrivals$present_at_surge
    }
    if (!"population" %in% names(arrivals)) {
      arrivals$population <- rep("surge", nrow(arrivals))
      arrivals$profile <- sub("_[0-9]+$", "", sub("^patient_", "", arrivals$name))
    }
    activity <- simmer::get_mon_arrivals(sim, per_resource = TRUE, ongoing = TRUE) |>
      dplyr::filter(.data$start_time >= 0)
    if (!is.null(metadata)) {
      activity <- activity |>
        dplyr::left_join(metadata$patients[c("name", "profile", "population")], by = "name") |>
        dplyr::mutate(start_time = .data$start_time - metadata$surge_start,
                      end_time = .data$end_time - metadata$surge_start)
    } else {
      activity$population <- rep("surge", nrow(activity))
      activity$profile <- sub("_[0-9]+$", "", sub("^patient_", "", activity$name))
    }
    activity$is_logical_wait <- startsWith(activity$resource, logical_queue_prefix)
    activity <- dplyr::arrange(activity, .data$start_time, .data$name, .data$resource, .data$end_time)
    list(resources = decorate(get_hospital_mon_resources(sim)),
         resource_history = decorate(get_hospital_mon_resources(sim, include_warmup = TRUE)),
         arrivals = decorate(arrivals),
         patient_resource_activity = decorate(activity),
         warmup_diagnostics = if (is.null(metadata)) data.frame() else decorate(metadata$warmup_diagnostics),
         runs = data.frame(replication = replication, scenario_id = scenario_id,
                           scenario_mode = scenario_mode,
                           warmup_days = if (is.null(metadata)) 0 else metadata$surge_start,
                           sim_days = sim_days, seed = seed))
  }, future.seed = as.integer(seed))
  tables <- stats::setNames(lapply(names(results[[1]]), function(name) {
    dplyr::bind_rows(lapply(results, `[[`, name))
  }), names(results[[1]]))
  c(tables, list(configuration = config, scenario_mode = scenario_mode,
                 parameters = data.frame(duration = duration, n_patients = n_patients,
                                         sim_days = sim_days, num_sims = num_sims, seed = seed,
                                         scenario_mode = scenario_mode)))
}
