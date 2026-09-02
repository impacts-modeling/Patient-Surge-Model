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
}

safe_fraction <- function(numerator, denominator, default = 0) {
  if (!is.finite(numerator) || !is.finite(denominator) || denominator <= 0) {
    return(default)
  }
  numerator / denominator
}

make_resource_summary <- function(data, var = "server") {
  # More efficient data processing
  plot_data <- data |>
    dplyr::mutate(time1 = ceiling(time)) |>
    dplyr::group_by(time1, resource, replication) |>
    dplyr::summarise(
      median_val = safe_max(.data[[var]]),
      .groups = "drop"
    ) |>
    dplyr::group_by(time1, resource) |>
    dplyr::summarise(
      median_val = safe_mean(median_val),
      .groups = "drop"
    )
  return(plot_data)
}

make_resource_data <- function(data, var = "server") {
  # More efficient data processing
  plot_data <- data |>
    dplyr::mutate(time1 = ceiling(time)) |>
    dplyr::group_by(time1, resource, replication) |>
    dplyr::summarise(
      median_val = safe_max(.data[[var]]),
      .groups = "drop"
    ) |>
    dplyr::group_by(time1, resource) |>
    dplyr::summarise(
      median_val = safe_mean(median_val),
      .groups = "drop"
    )
  return(plot_data)
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

summary_utilization <- function(data) {
  required_columns <- c("resource", "replication", "time", "server", "capacity")
  stopifnot(all(required_columns %in% names(data)))

  utilization_by_sim <- data |>
    dplyr::group_by(replication) |>
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
    
    dplyr::group_by(resource, replication) |>
    dplyr::arrange(time, .by_group = TRUE) |>
    dplyr::mutate(
      state_duration = pmax(
        dplyr::lead(time, default = dplyr::first(observation_end)) - time,
        0
      )
    ) |>
    dplyr::summarise(
      avg_utilization = safe_mean(utilization) * 100,
      peak_utilization = safe_max(utilization) * 100,
      avg_capacity = safe_mean(server),
      max_capacity = safe_max(server),
      time_at_capacity = sum(
        state_duration[server == capacity],
        na.rm = TRUE
      ),
      percent_at_capacity = safe_fraction(
        time_at_capacity,
        dplyr::first(observation_end)
      ) * 100,
      .groups = "drop"
    )

  utilization_summary <- utilization_by_sim |>
    dplyr::group_by(resource) |>
    dplyr::summarise(
      avg_utilization = safe_mean(avg_utilization),
      peak_utilization = safe_max(peak_utilization),
      avg_capacity = safe_mean(avg_capacity),
      max_capacity = safe_max(max_capacity),
      time_at_capacity = safe_mean(time_at_capacity),
      percent_at_capacity = safe_mean(percent_at_capacity),
      .groups = "drop"
    )

  colnames(utilization_summary) <- c("Resource", "Average Bed Utilization (%)", "Peak Bed Utilization (%)", "Average Occupied Beds", "Maximum Occupied Beds", "Time at Full Capacity (days)", "Percent of Time at Full Capacity (%)")
  utilization_summary[, -1] <- round(utilization_summary[, -1], 2)
  utilization_summary
}

summary_queue <- function(data) {
  required_columns <- c("resource", "replication", "server", "queue")
  stopifnot(all(required_columns %in% names(data)))

  queue_by_sim <- data |>
    # filter(time > last_days) |>
    # dplyr::mutate(time1 = ceiling(time)) |>
    # dplyr::group_by(time1, resource, replication) |>
    dplyr::group_by(resource, replication) |>
    dplyr::summarise(
      avg_queue_length = safe_mean(queue),
      max_queue_length = safe_max(queue),
      avg_congestion_index = safe_mean(as.numeric(queue > 0)),
      avg_wait_time_per_patient = safe_fraction(sum(queue, na.rm = TRUE), sum(server + queue, na.rm = TRUE)),
      .groups = "drop"
    )
  queue_analysis <- queue_by_sim |>
    dplyr::group_by(resource) |>
    dplyr::summarise(
      avg_queue_length = safe_mean(avg_queue_length),
      avg_max_queue_length = safe_median(max_queue_length),
      avg_congestion_index = safe_mean(avg_congestion_index),
      avg_wait_time_per_patient = safe_mean(avg_wait_time_per_patient),
      .groups = "drop"
    )

  queue_analysis[, -1] <- round(queue_analysis[, -1], 2)
  colnames(queue_analysis) <- c(
    "Resource", "Average Queue Length (Patients)",
    "Average Maximum Queue Length (Patients)", "Average Congestion Index",
    "Average Waiting Time Fraction"
  )
  queue_analysis
}
