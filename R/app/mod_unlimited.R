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
  "interval); it has no Monte Carlo error but ignores uncertainty in the inputs. Peaks are the peak",
  "over time of the expected occupancy and of the pointwise percentiles, not the distribution of the",
  "maximum. Bed counts, queues, waits and boarding do not apply to this scenario."
)

# config: surge configuration with $baseline and $arrival_process (profile_config()).
run_unlimited_analytic <- function(config, n_patients, duration, sim_days) {
  stopifnot(is.finite(n_patients), n_patients > 0, is.finite(duration), duration > 0,
            is.finite(sim_days), sim_days >= duration,
            config$arrival_process == "poisson" || n_patients == floor(n_patients))
  units <- setdiff(names(config$capacities), names(internal_hospital_units))
  elapsed <- system.time(
    analytic <- analytic_unlimited_demand(
      config$patient_profiles, config$profile_prob, n_patients, duration, sim_days,
      baseline = config$baseline, surge_process = config$arrival_process, units = units,
      step = unlimited_grid_step, output_step = unlimited_output_step, levels = unlimited_levels)
  )[["elapsed"]]
  c(analytic, list(scenario_mode = "unlimited", scenario_id = "unlimited_analytic",
                   profile_config = config, elapsed_seconds = elapsed))
}

# One row per unit (ED hidden): peak of the expected occupancy and of two percentiles.
unlimited_peak_table <- function(data) {
  peaks <- hide_internal_units(data$peaks, "resource")
  value_of <- function(unit, statistic, column = "peak_value") {
    peaks[[column]][peaks$resource == unit & peaks$statistic == statistic]
  }
  units <- unique(peaks$resource)
  data.frame(
    Resource = units,
    `Peak expected occupied beds` = round(vapply(units, value_of, numeric(1), "peak_mean"), 1),
    `Day of peak (day 0 = surge onset)` = vapply(units, value_of, numeric(1), "peak_mean", "time_of_peak"),
    `Peak 50th percentile (beds)` = vapply(units, value_of, numeric(1), "peak_q0.5"),
    `Peak 95th percentile (beds)` = vapply(units, value_of, numeric(1), "peak_q0.95"),
    check.names = FALSE
  )
}

make_unlimited_plot <- function(trajectory) {
  plot_data <- hide_internal_units(trajectory, "resource")
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
      xaxis = list(title = "Day (0 = surge onset) - expected occupancy; band: P5-P95"),
      yaxis = list(title = "Number of Beds Occupied")
    )
}

mod_unlimited_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("status")),
    shiny::fluidRow(
      shinydashboard::box(
        title = "Expected Occupied Beds (Unlimited Capacity)", status = "success",
        solidHeader = TRUE, width = 8, collapsible = TRUE,
        plotly::plotlyOutput(ns("occupancy_plot"))
      ),
      shinydashboard::box(
        title = "Peak Occupancy", status = "success", solidHeader = TRUE, width = 4,
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
      shiny::div(class = "alert alert-info", unlimited_assumption_note, shiny::tags$br(),
        sprintf("Computed in %.1f seconds.", data$elapsed_seconds))
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
