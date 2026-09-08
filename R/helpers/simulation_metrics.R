safe_mean <- function(x, default = 0) {
  finite_values <- x[is.finite(x)]
  if (length(finite_values) == 0) {
    return(default)
  }
  mean(finite_values)
}

safe_max <- function(x, default = 0) {
  finite_values <- x[is.finite(x)]
  if (length(finite_values) == 0) {
    return(default)
  }
  max(finite_values)
}

safe_median <- function(x, default = 0) {
  finite_values <- x[is.finite(x)]
  if (length(finite_values) == 0) {
    return(default)
  }
  median(finite_values)
  #mean(finite_values)
}

safe_fraction <- function(numerator, denominator, default = 0) {
  if (!is.finite(numerator) || !is.finite(denominator) || denominator <= 0) {
    return(default)
  }
  numerator / denominator
}

make_resource_summary <- function(data, var = "server") {
  stopifnot(var %in% names(data))
  keys <- intersect(c("scenario_id", "resource", "replication"), names(data))
  daily <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::group_modify(function(rows, key) {
      rows <- rows[order(rows$time), , drop = FALSE]
      start <- min(rows$time)
      end <- max(rows$time)
      if (end <= start) return(data.frame(time1 = numeric(), median_val = numeric()))
      boundaries <- seq(floor(start), ceiling(end), by = 1)
      grid <- sort(unique(c(rows$time, boundaries[boundaries > start & boundaries < end])))
      index <- findInterval(grid, rows$time)
      values <- rows[[var]][pmax(1L, index)]
      intervals <- data.frame(time1 = floor(utils::head(grid, -1)) + 1,
        value = utils::head(values, -1), dt = diff(grid))
      intervals |>
        dplyr::group_by(.data$time1) |>
        dplyr::summarise(median_val = sum(.data$value * .data$dt) / sum(.data$dt),
                         .groups = "drop")
    }) |>
    dplyr::ungroup()
  # Retain the historical column name for plot consumers; it now holds a mean.
  daily |>
    dplyr::group_by(dplyr::across(dplyr::all_of(intersect(
      c("scenario_id", "time1", "resource"), names(daily))))) |>
    dplyr::summarise(median_val = safe_mean(.data$median_val), .groups = "drop")
}

# One logical resource visit is one bed request, attributed to its primary unit
# even when a fallback provides the bed. The default cohort is discharged
# patients; completed_only=FALSE retains the earlier all-request analysis.
bed_wait_summary <- function(run, confidence_level = 0.95, completed_only = TRUE) {
  activity <- run$patient_resource_activity
  if (completed_only) {
    keys <- c("scenario_id", "replication", "name")
    stopifnot(all(c(keys, "finished") %in% names(run$arrivals)))
    completed <- run$arrivals |>
      dplyr::filter(.data$finished %in% TRUE) |>
      dplyr::select(dplyr::all_of(keys)) |>
      dplyr::distinct()
    activity <- dplyr::semi_join(activity, completed, by = keys)
  }
  required <- c("name", "resource", "start_time", "end_time", "population", "replication", "scenario_id")
  stopifnot(all(required %in% names(activity)), confidence_level > 0, confidence_level < 1)
  prefix <- ".waiting_for__"
  requests <- activity |>
    dplyr::filter(startsWith(.data$resource, prefix)) |>
    dplyr::left_join(run$runs[c("scenario_id", "replication", "sim_days")],
                     by = c("scenario_id", "replication")) |>
    dplyr::mutate(resource = substring(.data$resource, nchar(prefix) + 1L),
      resolved = is.finite(.data$end_time) & .data$end_time >= .data$start_time &
        .data$end_time <= .data$sim_days,
      cohort = dplyr::if_else(.data$start_time < 0, "Waiting at day 0", "Requested during observation")) |>
    dplyr::filter(.data$start_time < .data$sim_days,
      .data$start_time >= 0 | !.data$resolved | .data$end_time > 0) |>
    dplyr::mutate(wait_days = dplyr::if_else(.data$resolved, .data$end_time - .data$start_time, NA_real_),
      elapsed_wait_days = dplyr::if_else(.data$resolved, .data$wait_days, .data$sim_days - .data$start_time))
  if (!nrow(requests)) return(list(requests = requests, replications = data.frame(), summary = data.frame()))
  replications <- requests |>
    dplyr::group_by(.data$scenario_id, .data$replication, .data$resource, .data$population, .data$cohort) |>
    dplyr::summarise(requests = dplyr::n(), pending = sum(!.data$resolved),
      mean_wait_days = if (all(.data$resolved)) mean(.data$wait_days) else NA_real_,
      percent_observed_waiting = 100 * mean(.data$elapsed_wait_days > 1e-10),
      mean_resolved_wait_days = safe_mean(.data$wait_days, NA_real_),
      mean_resolved_positive_wait_days = safe_mean(.data$wait_days[.data$wait_days > 1e-10], NA_real_),
      p90_resolved_wait_days = if (any(.data$resolved))
        as.numeric(stats::quantile(.data$wait_days, .9, na.rm = TRUE)) else NA_real_, .groups = "drop")
  summary <- replications |>
    dplyr::group_by(.data$scenario_id, .data$resource, .data$population, .data$cohort) |>
    dplyr::summarise(replications_with_requests = dplyr::n(),
      total_requests = sum(.data$requests), pending_requests = sum(.data$pending),
      sd_mean_wait = stats::sd(.data$mean_wait_days),
      mean_wait_days = mean(.data$mean_wait_days),
      percent_observed_waiting = mean(.data$percent_observed_waiting),
      mean_resolved_wait_days = safe_mean(.data$mean_resolved_wait_days, NA_real_),
      mean_resolved_positive_wait_days = safe_mean(.data$mean_resolved_positive_wait_days, NA_real_),
      mean_p90_resolved_wait_days = safe_mean(.data$p90_resolved_wait_days, NA_real_), .groups = "drop") |>
    dplyr::mutate(mcse = .data$sd_mean_wait / sqrt(.data$replications_with_requests),
      lower = .data$mean_wait_days - stats::qt((1 + confidence_level) / 2,
        pmax(1, .data$replications_with_requests - 1)) * .data$mcse,
      upper = .data$mean_wait_days + stats::qt((1 + confidence_level) / 2,
        pmax(1, .data$replications_with_requests - 1)) * .data$mcse,
      confidence_level = confidence_level)
  # No zero is invented for a replica with no requests. Pending requests make
  # the full-cohort mean/CI unavailable; resolved-only metrics are labelled.
  list(requests = requests, replications = replications, summary = summary)
}

bed_wait_table <- function(run) {
  data <- bed_wait_summary(run)$summary
  if (!nrow(data)) return(data.frame(Status = "No completed patients with bed requests in the observation period."))
  data |>
    dplyr::transmute(Unit = .data$resource, Population = .data$population, Cohort = .data$cohort,
      `Mean wait (days)` = round(.data$mean_wait_days, 3),
      `95% CI` = ifelse(is.finite(.data$lower), sprintf("%.3f to %.3f", .data$lower, .data$upper),
                        "Requires at least 2 replications with completed patients"),
      `Observed waiting (%)` = round(.data$percent_observed_waiting, 1))
}

# Unrounded, one observation per scenario/resource/replication/metric.
resource_replication_metrics <- function(data) {
  keys <- intersect(c("scenario_id", "resource", "replication"), names(data))
  dplyr::full_join(summary_utilization(data, by_replication = TRUE),
                   summary_queue(data, by_replication = TRUE), by = keys) |>
    tidyr::pivot_longer(cols = -dplyr::all_of(keys), names_to = "metric", values_to = "value")
}

# Positive differences mean comparison minus reference. Intervals describe
# Monte Carlo uncertainty in the mean difference, not input/model uncertainty.
# Replications must be independent across pairs and share random streams within pairs.
compare_hospital_scenarios <- function(reference, comparison, confidence_level = 0.95) {
  stopifnot(length(confidence_level) == 1L, is.finite(confidence_level),
            confidence_level > 0, confidence_level < 1)
  run_keys <- c("replication", "seed", "sim_days", "warmup_days")
  run_signature <- function(x) {
    stopifnot(all(run_keys %in% names(x$runs)), !anyDuplicated(x$runs$replication),
              length(unique(x$runs$scenario_id)) == 1L)
    x$runs[order(x$runs$replication), run_keys, drop = FALSE]
  }
  if (!isTRUE(all.equal(run_signature(reference), run_signature(comparison), check.attributes = FALSE))) {
    stop("Paired comparisons require matching replications, seeds, horizons and warm-up durations.")
  }
  if (!identical(reference$configuration$baseline, comparison$configuration$baseline)) {
    stop("Paired comparisons require the same routine civilian configuration.")
  }
  # Check actual pre-observation states as well as configuration metadata.
  history_signature <- function(x) {
    rows <- x$resource_history
    if (is.null(rows)) stop("Resource history is required to verify initial conditions.")
    rows <- rows[rows$time < 0, setdiff(names(rows), c("scenario_id", "scenario_mode")), drop = FALSE]
    rows[order(rows$replication, rows$resource, rows$time), , drop = FALSE]
  }
  if (!isTRUE(all.equal(history_signature(reference), history_signature(comparison), check.attributes = FALSE))) {
    stop("Paired comparisons require identical hospital histories before observation.")
  }
  keys <- c("replication", "resource", "metric")
  prepare <- function(x) {
    resource_replication_metrics(x$resources) |>
      dplyr::select(dplyr::all_of(c(keys, "value")))
  }
  a <- prepare(reference)
  b <- prepare(comparison)
  if (anyDuplicated(a[keys]) || anyDuplicated(b[keys]) ||
      nrow(dplyr::anti_join(a, b, by = keys)) || nrow(dplyr::anti_join(b, a, by = keys))) {
    stop("Every metric must have exactly one matching observation in each replication.")
  }
  pairs <- dplyr::inner_join(a, b, by = keys, suffix = c("_reference", "_comparison")) |>
    dplyr::mutate(difference = .data$value_comparison - .data$value_reference)
  if (any(!is.finite(pairs$difference))) stop("Paired metrics must be finite.")
  summary <- pairs |>
    dplyr::group_by(.data$resource, .data$metric) |>
    dplyr::summarise(n_pairs = dplyr::n(),
      reference_mean = mean(.data$value_reference), comparison_mean = mean(.data$value_comparison),
      mean_difference = mean(.data$difference), sd_difference = stats::sd(.data$difference),
      .groups = "drop") |>
    dplyr::mutate(mcse = .data$sd_difference / sqrt(.data$n_pairs),
      critical_value = stats::qt((1 + confidence_level) / 2, pmax(1, .data$n_pairs - 1)),
      lower = .data$mean_difference - .data$critical_value * .data$mcse,
      upper = .data$mean_difference + .data$critical_value * .data$mcse,
      confidence_level = confidence_level) |>
    dplyr::select(-"critical_value")
  decorate <- function(x) dplyr::mutate(x,
    reference_scenario = as.character(reference$runs$scenario_id[[1]]),
    comparison_scenario = as.character(comparison$runs$scenario_id[[1]]))
  list(pairs = decorate(pairs), summary = decorate(summary))
}

make_resource_data <- function(data, var = "server") {
  make_resource_summary(data, var)
}


make_resource_plot <- function(data, var = "server") {
  stopifnot(var %in% names(data)) # Validate input column

  title <- switch(
    var,
    "server" = "Average Resource Utilization Over Time",
    "queue"  = "Queue Lengths Over Time",
    paste("Plot of", var)
  )

  yaxis_label <- switch(
    var,
    "server" = "Number of Beds Occupied",
    "queue"  = "Number of Patients Waiting",
    paste("Value of", var)
  )

  plot_data <- make_resource_data(data, var = var)

  p <- plotly::plot_ly(
    plot_data,
    x = ~time1, y = ~median_val,
    color = ~resource,
    type = "scatter", mode = "lines",
    opacity = 1,
    line = list(width = 2)
  ) |>
    plotly::layout(
      title = "",
      xaxis = list(title = "Time (days)"),
      yaxis = list(title = yaxis_label) # ,
      # legend = list(
      #   orientation = "h", # horizontal legend
      #   x = 0.5, # left aligned
      #   y = 1.18, # above the plot
      #   xanchor = "center",
      #   yanchor = "top",
      #   # itemwidth = 10, # forces wrapping → creates 2 rows
      #   valign = "top"
      # )
    )
}

# Input must contain the day-zero state and the terminal observation state.
resource_state_intervals <- function(data) {
  stopifnot(all(c("resource", "replication", "time") %in% names(data)))
  groups <- intersect(c("scenario_id", "resource", "replication"), names(data))
  data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
    dplyr::arrange(.data$time, .by_group = TRUE) |>
    dplyr::mutate(state_duration = pmax(0, dplyr::lead(.data$time,
      default = dplyr::last(.data$time)) - .data$time))
}

summary_utilization <- function(data, by_replication = FALSE) {
  required_columns <- c("resource", "replication", "time", "server", "capacity")
  stopifnot(all(required_columns %in% names(data)))

  utilization_by_sim <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(intersect(c("scenario_id", "replication"), names(data))))) |>
    dplyr::mutate(observation_end = safe_max(time)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      utilization = dplyr::if_else(
        is.finite(capacity) & capacity > 0,
        server / capacity,
        0
      )
    ) |>
    
    # Cambiar a promedio diario
    # dplyr::mutate(time1 = ceiling(time)) |>
    # dplyr::group_by(time1, resource, replication) |>
    # dplyr::summarise(
    #   utilization = safe_mean(utilization),
    #   server = safe_mean(server),
    #   .groups = "drop"
    # ) |>
    
    dplyr::group_by(dplyr::across(dplyr::all_of(intersect(c("scenario_id", "resource", "replication"), names(data))))) |>
    dplyr::arrange(time, .by_group = TRUE) |>
    dplyr::mutate(
      state_duration = pmax(
        dplyr::lead(time, default = dplyr::first(observation_end)) - time,
        0
      )
    ) |>
    dplyr::summarise(
      avg_utilization = safe_fraction(sum(utilization * state_duration), sum(state_duration)) * 100,
      peak_utilization = safe_max(utilization) * 100,
      avg_capacity = safe_fraction(sum(server * state_duration), sum(state_duration)),
      max_capacity = safe_max(server),
      time_at_capacity = sum(
        state_duration[server == capacity & capacity > 0],
        na.rm = TRUE
      ),
      percent_at_capacity = safe_fraction(
        time_at_capacity,
        sum(state_duration)
      ) * 100,
      .groups = "drop"
    )

  if (by_replication) return(utilization_by_sim)
  utilization_summary <- utilization_by_sim |>
    dplyr::group_by(dplyr::across(dplyr::all_of(intersect(c("scenario_id", "resource"), names(data))))) |>
    dplyr::summarise(
      avg_utilization = safe_mean(avg_utilization),
      peak_utilization = safe_max(peak_utilization),
      avg_capacity = safe_mean(avg_capacity),
      max_capacity = safe_max(max_capacity),
      time_at_capacity = safe_mean(time_at_capacity),
      percent_at_capacity = safe_mean(percent_at_capacity),
      .groups = "drop"
    )

  colnames(utilization_summary) <- c(intersect("scenario_id", names(data)), "Resource", "Average Bed Utilization (%)", "Peak Bed Utilization (%)", "Average Occupied Beds", "Maximum Occupied Beds", "Time at Full Capacity (days)", "Percent of Time at Full Capacity (%)")
  utilization_summary <- dplyr::mutate(utilization_summary, dplyr::across(where(is.numeric), ~round(.x, 2)))
  utilization_summary
}

summary_queue <- function(data, by_replication = FALSE) {
  required_columns <- c("resource", "replication", "server", "queue")
  stopifnot(all(required_columns %in% names(data)))

  queue_by_sim <- resource_state_intervals(data) |>
    # filter(time > last_days) |>
    # dplyr::mutate(time1 = ceiling(time)) |>
    # dplyr::group_by(time1, resource, replication) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(intersect(c("scenario_id", "resource", "replication"), names(data))))) |>
    dplyr::summarise(
      avg_queue_length = safe_fraction(sum(queue * state_duration), sum(state_duration)),
      max_queue_length = safe_max(queue),
      avg_congestion_index = safe_fraction(sum((queue > 0) * state_duration), sum(state_duration)),
      avg_wait_time_per_patient = safe_fraction(sum(queue * state_duration), sum((server + queue) * state_duration)),
      .groups = "drop"
    )
  if (by_replication) return(queue_by_sim)
  queue_analysis <- queue_by_sim |>
    dplyr::group_by(dplyr::across(dplyr::all_of(intersect(c("scenario_id", "resource"), names(data))))) |>
    dplyr::summarise(
      avg_queue_length = safe_mean(avg_queue_length),
      avg_max_queue_length = safe_median(max_queue_length),
      avg_congestion_index = safe_mean(avg_congestion_index),
      avg_wait_time_per_patient = safe_mean(avg_wait_time_per_patient),
      .groups = "drop"
    )

  queue_analysis <- dplyr::mutate(queue_analysis, dplyr::across(where(is.numeric), ~round(.x, 2)))
  colnames(queue_analysis) <- c(
    intersect("scenario_id", names(data)), "Resource", "Average Queue Length (Patients)",
    "Median Maximum Queue Length (Patients)", "Fraction of Time with a Queue",
    "Queued Patient-Time Fraction"
  )
  queue_analysis
}
