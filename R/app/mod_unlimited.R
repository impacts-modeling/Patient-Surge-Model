# Unlimited-capacity scenario (analytic) --------------------------------------
# No simulation: occupancy at every time is computed from the model's own inputs
# (R/analitic/occupancy_analytic.R). Assumptions are listed in unlimited_assumption_note.
unlimited_levels <- c(0.05, 0.5, 0.95)
unlimited_grid_step <- 0.05    # days; discretisation of log-normal stays
unlimited_output_step <- 0.25  # days; spacing of reported times

unlimited_assumption_note <- paste(
  "Unlimited capacity: nobody waits, boards or uses a fallback, so each patient simply spends",
  "the sampled length of stay in every unit of the pathway. Occupancy at each time is computed",
  "exactly from the model inputs (Poisson civilian arrivals; evenly spaced or Poisson surge arrivals;",
  "log-normal stays discretised on a 0.05-day grid). The band is the 5th-95th percentile of the number",
  "of occupied beds at that instant (a prediction band for the random occupancy, not a confidence",
  "interval). Peaks are the peak over time of the expected occupancy and of the pointwise percentiles,",
  "not the distribution of the maximum. With routine civilian flow, the increase vs. civilian baseline is the peak",
  "expected occupancy with surge minus the civilian-only peak, per unit. Bed counts, queues, waits and boarding do",
  "not apply to this scenario."
)

# config: surge configuration with $baseline and $arrival_process (profile_config()).
run_unlimited_analytic <- function(config, n_patients, duration, sim_days, include_surge = TRUE) {
  if (include_surge) {
    stopifnot(is.finite(n_patients), n_patients > 0, is.finite(duration), duration > 0,
              is.finite(sim_days), sim_days >= duration,
              config$arrival_process == "poisson" || n_patients == floor(n_patients))
  } else {
    # Routine civilian operation only: no surge stream, so the civilian flow is required.
    if (!isTRUE(config$baseline$enabled)) {
      stop("Enable Routine Civilian Flow in Hospital Setup to compute the civilian-only scenario.", call. = FALSE)
    }
    stopifnot(is.finite(sim_days), sim_days > 0)
    n_patients <- 0
    duration <- 0
  }
  units <- setdiff(names(config$capacities), names(internal_hospital_units))
  elapsed <- system.time(
    analytic <- analytic_unlimited_demand(
      config$patient_profiles, config$profile_prob, n_patients, duration, sim_days,
      baseline = config$baseline, surge_process = config$arrival_process, units = units,
      step = unlimited_grid_step, output_step = unlimited_output_step, levels = unlimited_levels)
  )[["elapsed"]]
  # Civilian-only peaks (same horizon and warm-up) give the baseline against which the
  # surge increase is reported. Only available when civilian flow is enabled; if this
  # extra calculation fails, the table is simply shown without the baseline columns.
  civilian_peaks <- NULL
  if (include_surge && isTRUE(config$baseline$enabled)) {
    civilian_peaks <- tryCatch(
      run_unlimited_analytic(config, n_patients, duration, sim_days, include_surge = FALSE)$peaks,
      error = function(e) NULL)
  }
  c(analytic, list(scenario_mode = "unlimited", scenario_id = "unlimited_analytic",
                   profile_config = config, include_surge = include_surge,
                   civilian_peaks = civilian_peaks, elapsed_seconds = elapsed))
}

# One row per unit (ED hidden): peak of the expected occupancy and of the 95th percentile.
# With civilian flow, also the civilian-only peak of the expected occupancy and the
# increase over it (surge + civilian peak minus civilian-only peak, per unit).
unlimited_peak_table <- function(data) {
  peaks <- hide_internal_units(data$peaks, "resource")
  peak_of <- function(peak_table, unit, statistic, column = "peak_value") {
    peak_table[[column]][peak_table$resource == unit & peak_table$statistic == statistic]
  }
  units <- unique(peaks$resource)
  peak_mean <- vapply(units, function(unit) peak_of(peaks, unit, "peak_mean"), numeric(1))
  # Column order: expected peak, [civilian-only peak, increase], 95th percentile, day of peak.
  table <- data.frame(
    Resource = unit_display_label(units),
    `Peak expected occupied beds` = round(peak_mean, 1),
    check.names = FALSE
  )
  if (!is.null(data$civilian_peaks)) {
    civilian <- hide_internal_units(data$civilian_peaks, "resource")
    civilian_mean <- vapply(units, function(unit) {
      value <- peak_of(civilian, unit, "peak_mean")
      if (length(value)) value else NA_real_
    }, numeric(1))
    table[["Civilian-only peak (beds)"]] <- round(civilian_mean, 1)
    table[["Increase vs. civilian baseline (beds)"]] <- round(peak_mean - civilian_mean, 1)
  }
  table[["Peak 95th percentile (beds)"]] <- vapply(
    units, function(unit) peak_of(peaks, unit, "peak_q0.95"), numeric(1))
  table[["Day of peak"]] <- vapply(
    units, function(unit) peak_of(peaks, unit, "peak_mean", "time_of_peak"), numeric(1))
  table
}

make_unlimited_plot <- function(trajectory) {
  plot_data <- relabel_units(hide_internal_units(trajectory, "resource"), "resource")
  units <- unique(plot_data$resource)
  colors <- grDevices::hcl.colors(max(3L, length(units)), "Dark 3")
  p <- plotly::plot_ly()
  for (i in seq_along(units)) {
    rows <- plot_data[plot_data$resource == units[[i]], ]
    p <- p |>
      plotly::add_ribbons(data = rows, x = ~time, ymin = ~q0.05, ymax = ~q0.95,
        name = paste(units[[i]], "P5-P95"), legendgroup = units[[i]],
        fillcolor = colors[[i]], opacity = .18, line = list(color = "transparent"),
        showlegend = FALSE, hoverinfo = "skip") |>
      plotly::add_lines(data = rows, x = ~time, y = ~mean,
        name = units[[i]], legendgroup = units[[i]], line = list(color = colors[[i]], width = 2),
        text = ~paste0("Day: ", time, "<br>Expected occupied beds: ", round(mean, 1),
          "<br>P5-P95: ", q0.05, "-", q0.95),
        hoverinfo = "text+name")
  }
  p |>
    plotly::layout(
      title = "",
      # Legend above the plot; the note under the x axis is split in two shorter,
      # smaller lines so it is not cut off.
      legend = list(orientation = "h", x = 0, y = 1.02, xanchor = "left", yanchor = "bottom"),
      xaxis = list(
        title = list(
          text = "Day (0 = start of observation; negative = civilian warm-up)<br>Expected occupancy; band: P5-P95",
          font = list(size = 12))),
      yaxis = list(title = "Number of Beds Occupied"),
      margin = list(t = 60, b = 80)
    )
}

mod_unlimited_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("status")),
    shiny::fluidRow(
      shinydashboard::box(
        title = "Expected Occupied Beds (Unlimited Capacity)", status = "success",
        solidHeader = TRUE, width = 7, collapsible = TRUE,
        plotly::plotlyOutput(ns("occupancy_plot"))
      ),
      shinydashboard::box(
        title = "Peak Occupancy", status = "success", solidHeader = TRUE, width = 5,
        collapsible = TRUE,
        shiny::tableOutput(ns("peak_table")),
        style = "overflow-x: auto;"
      )
    )
  )
}

# analytic_data: reactive returning the list from run_unlimited_analytic(), or NULL.
mod_unlimited_server <- function(id, analytic_data) {
  shiny::moduleServer(id, function(input, output, session) {
    output$status <- shiny::renderUI({
      data <- analytic_data()
      if (is.null(data)) {
        return(shiny::div(class = "alert alert-info",
          "Unlimited capacity selected. Click Run Simulation to compute the analytic occupancy (no simulation is run)."))
      }
      shiny::div(class = "alert alert-info", unlimited_assumption_note)
    })
    output$occupancy_plot <- plotly::renderPlotly({
      shiny::req(analytic_data())
      make_unlimited_plot(analytic_data()$trajectory)
    })
    output$peak_table <- shiny::renderTable({
      shiny::req(analytic_data())
      unlimited_peak_table(analytic_data())
    })
  })
}
