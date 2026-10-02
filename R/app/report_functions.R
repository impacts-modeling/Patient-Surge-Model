format_report_value <- function(x) {
  if (length(x) == 0 || is.null(x)) {
    return(NA_character_)
  }
  if (is.numeric(x)) {
    return(format(round(x, 3), trim = TRUE, scientific = FALSE))
  }
  as.character(x)
}

make_parameter_table <- function(params) {
  data.frame(
    Parameter = names(params),
    Value = vapply(params, format_report_value, character(1)),
    stringsAsFactors = FALSE
  )
}

make_expansion_table <- function(n_result) {
  if (is.null(n_result)) {
    return(data.frame(
      Metric = "Bed expansion search",
      Value = "Not run before report download",
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    Metric = c(
      "Additional Med/Surg beds",
      "Additional ICU HxS beds",
      "Validated total Med/Surg capacity",
      "Validated total ICU capacity",
      "Unified search evaluations",
      "Independent final evaluations",
      "Joint mean-wait compliance target met",
      "Final mean GenMed wait time (days)",
      "Final mean ICU wait time (days)"
    ),
    Value = c(
      n_result$N_added %||% NA_integer_,
      n_result$N_added_ICU %||% NA_integer_,
      n_result$GenMed_N %||% NA_integer_,
      n_result$ICU_N %||% NA_integer_,
      n_result$search_evaluations %||% NA_integer_,
      n_result$final_evaluations %||% NA_integer_,
      isTRUE(n_result$converged),
      n_result$mean_wait_days_GenMed %||% NA_real_,
      n_result$mean_wait_days_ICU %||% NA_real_
    ),
    stringsAsFactors = FALSE
  )
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    return(y)
  }
  x
}

add_report_text_page <- function(title, lines, cex = 0.8) {
  grid::grid.newpage()
  grid::grid.text(
    title,
    x = grid::unit(0.05, "npc"),
    y = grid::unit(0.95, "npc"),
    just = c("left", "top"),
    gp = grid::gpar(fontface = "bold", fontsize = 18)
  )
  grid::grid.text(
    paste(lines, collapse = "\n"),
    x = grid::unit(0.05, "npc"),
    y = grid::unit(0.88, "npc"),
    just = c("left", "top"),
    gp = grid::gpar(fontfamily = "mono", fontsize = 8 * cex)
  )
}

add_report_table_page <- function(title, table_data, rows_per_page = 16) {
  table_data <- as.data.frame(
    table_data,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  table_data[] <- lapply(table_data, function(column) {
    values <- ifelse(is.na(column), "-", as.character(column))
    iconv(values, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  })

  wrap_width <- max(18, floor(105 / max(1, ncol(table_data))))
  table_data[] <- lapply(table_data, function(column) {
    vapply(column, function(value) {
      paste(strwrap(value, width = wrap_width), collapse = "\n")
    }, character(1))
  })

  if (nrow(table_data) == 0) {
    table_data <- data.frame(Message = "No data available", check.names = FALSE)
  }
  page_groups <- split(
    seq_len(nrow(table_data)),
    ceiling(seq_len(nrow(table_data)) / rows_per_page)
  )
  total_pages <- length(page_groups)

  for (page_index in seq_along(page_groups)) {
    page_data <- table_data[page_groups[[page_index]], , drop = FALSE]
    row_fill <- rep(c("#F4F7FB", "#FFFFFF"), length.out = nrow(page_data))
    table_theme <- gridExtra::ttheme_minimal(
      base_size = 9,
      padding = grid::unit(c(3.5, 3), "mm"),
      core = list(
        fg_params = list(
          col = "#243447",
          fontsize = 8.5,
          hjust = 0,
          x = 0.04
        ),
        bg_params = list(
          fill = rep(row_fill, times = ncol(page_data)),
          col = "#D9E2EC",
          lwd = 0.6
        )
      ),
      colhead = list(
        fg_params = list(
          col = "#FFFFFF",
          fontface = "bold",
          fontsize = 9.5,
          hjust = 0,
          x = 0.04
        ),
        bg_params = list(
          fill = "#1F4E78",
          col = "#1F4E78",
          lwd = 0.8
        )
      )
    )
    header_width <- 18
    
    names(page_data) <- vapply(
      names(page_data),
      function(header) {
        paste(
          strwrap(header, width = header_width),
          collapse = "\n"
        )
      },
      character(1)
    )
    
    table_grob <- gridExtra::tableGrob(
      page_data,
      rows = NULL,
      theme = table_theme
    )

    column_lengths <- vapply(seq_along(page_data), function(column_index) {
      max(
        nchar(names(page_data)[[column_index]]),
        nchar(gsub("\n.*", "", page_data[[column_index]])),
        na.rm = TRUE
      )
    }, numeric(1))
    column_weights <- pmax(8, pmin(column_lengths, 36))
    table_grob$widths <- grid::unit(
      column_weights / sum(column_weights),
      "npc"
    )

    grid::grid.newpage()
    grid::grid.rect(
      x = grid::unit(0.025, "npc"),
      y = grid::unit(0.5, "npc"),
      width = grid::unit(0.008, "npc"),
      height = grid::unit(0.92, "npc"),
      just = "left",
      gp = grid::gpar(fill = "#2E75B6", col = NA)
    )
    grid::grid.text(
      title,
      x = grid::unit(0.055, "npc"),
      y = grid::unit(0.95, "npc"),
      just = c("left", "top"),
      gp = grid::gpar(
        col = "#163A5F",
        fontface = "bold",
        fontsize = 18
      )
    )

    grid::pushViewport(grid::viewport(
      x = grid::unit(0.055, "npc"),
      y = grid::unit(0.86, "npc"),
      width = grid::unit(0.90, "npc"),
      height = sum(table_grob$heights),
      just = c("left", "top")
    ))
    grid::grid.draw(table_grob)
    grid::popViewport()

    if (total_pages > 1) {
      grid::grid.text(
        paste("Page", page_index, "of", total_pages),
        x = grid::unit(0.95, "npc"),
        y = grid::unit(0.035, "npc"),
        just = c("right", "bottom"),
        gp = grid::gpar(col = "#60758A", fontsize = 8)
      )
    }
  }
}

# Internal units (ED, fixed at 999 beds) are not user-configured, so they are
# left out of the result tables and plots shown in the app and PDF report.
hide_internal_units <- function(table, column) {
  if (!column %in% names(table)) return(table)
  table[!table[[column]] %in% names(internal_hospital_units), , drop = FALSE]
}

# Result tables shared by the dashboard and the PDF report.
utilization_results_table <- function(simulation_data) {
  hide_internal_units(summary_utilization(simulation_data$resources), "Resource")
}

queue_results_table <- function(simulation_data) {
  hide_internal_units(summary_queue(simulation_data$resources), "Resource")
}

bed_wait_results_table <- function(simulation_data) {
  table <- hide_internal_units(bed_wait_table(simulation_data), "Unit")
  if (nrow(table)) table else data.frame(Status = "No bed requests with a resolved wait in the observation period.")
}

boarding_results_table <- function(simulation_data) {
  table <- hide_internal_units(boarding_time_table(simulation_data), "Unit")
  if (nrow(table)) table else data.frame(Status = "No boarding episodes recorded in the observation period.")
}

add_resource_plot_page <- function(resources, var = "server") {
  # Same daily-mean definition as the dashboard plot and the manuscript
  # figures: mean of daily means across replications, 10th-90th
  # percentile band. See make_daily_peak_summary in simulation_metrics.R.
  resources <- hide_internal_units(resources, "resource")
  plot_data <- make_daily_peak_summary(resources, var = var)
  y_label <- switch(
    var,
    "server" = "Number of Beds Occupied",
    "queue" = "Number of Patients Waiting",
    paste("Value of", var)
  )
  title <- switch(
    var,
    "server" = "Daily Mean Occupied Beds",
    "queue" = "Daily Mean Queue Length",
    paste("Plot of", var)
  )

  plot_obj <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = time1, y = median_val, color = resource, fill = resource)
  ) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper),
                         alpha = 0.15, color = NA, na.rm = TRUE) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::labs(title = title, x = "Time (days)", y = y_label, color = "Resource", fill = "Resource",
      caption = "Mean of daily means across replications; shaded band shows the 10th-90th percentiles.") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")

  print(plot_obj)
}

build_report_params <- function(input, profile_config) {
  civilian_only <- identical(input$scenario_mode, "civilian_only")
  capacities <- profile_config$capacities
  capacity_params <- as.list(capacities[!names(capacities) %in% names(internal_hospital_units)])
  names(capacity_params) <- paste(names(capacity_params), "Total Beds")
  c(
    list(
      `Scenario` = if (civilian_only) "Routine civilian operation only" else "Surge event",
      `Surge arrival process` = profile_config$arrival_process,
      `Surge Patients per Day` = if (civilian_only) 0 else input$n_patients,
      `Surge Arrival Period (days)` = if (civilian_only) 0 else input$duration,
      `Simulation Duration (days)` = input$sim_days,
      `Number of simulations` = input$num_sims
    ),
    list(`Routine civilian flow enabled` = isTRUE(profile_config$baseline$enabled),
         `Civilian arrival process` = profile_config$baseline$arrival_process,
         `Warm-up duration (days)` = profile_config$baseline$warmup_days,
         `Simulation seed` = input$simulation_seed),
    capacity_params,
    list(
      `Maximum Allowed Med/Surg Mean Wait (days)` = input$congestion_index,
      `Maximum Allowed ICU Mean Wait (days)` = input$congestion_index_icu,
      `HxS Med/Surg Additional Beds Entered` = input$genmed_msf,
      `HxS ICU Additional Beds Entered` = input$icu_msf
    )
  )
}

make_profile_configuration_table <- function(profile_config) {
  profiles <- profile_config$patient_profiles
  data.frame(
    Profile = names(profiles),
    Trajectory = vapply(
      profiles,
      function(profile) paste(profile$unit, collapse = " -> "),
      character(1)
    ),
    Mean_stay_days = vapply(profiles, function(profile) format_steps(profile$los), character(1)),
    SD_days = vapply(profiles, function(profile) format_steps(profile_step_sd(profile)), character(1)),
    Arrival_percent = round(100 * profile_config$profile_prob[names(profiles)], 3),
    check.names = FALSE
  )
}

make_fallback_configuration_table <- function(profile_config) {
  fallbacks <- profile_config$fallbacks
  if (length(fallbacks) == 0) {
    return(data.frame(Primary_unit = "None configured", Fallback_units = ""))
  }
  data.frame(
    Primary_unit = names(fallbacks),
    Fallback_units = vapply(
      fallbacks,
      function(units) if (length(units) == 0) "None" else paste(units, collapse = " -> "),
      character(1)
    ),
    check.names = FALSE
  )
}

generate_simulation_pdf_report <- function(file, params, simulation_data, profile_config, n_result = NULL) {
  stopifnot(!is.null(simulation_data$resources), !is.null(simulation_data$arrivals))

  grDevices::pdf(file, width = 11, height = 8.5, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)

  add_report_text_page(
    "Patient Surge Model Report",
    c(
      paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      "",
      "This report summarizes the current model configuration, bed expansion estimate,",
      "resource utilization, queue bottlenecks, and bed waiting time results."
    ),
    cex = 1.1
  )

  add_report_table_page("Model Parameter Configuration", make_parameter_table(params))
  civilian_only <- identical(simulation_data$scenario_mode, "civilian_only")
  if (!civilian_only) {
    add_report_table_page("Surge Patient Profiles", make_profile_configuration_table(profile_config))
  }
  if (isTRUE(profile_config$baseline$enabled)) {
    baseline <- profile_config$baseline
    civilian_profiles <- dplyr::bind_rows(lapply(names(baseline$profiles), function(name) {
      profile <- baseline$profiles[[name]]
      data.frame(Profile = name, Patients_per_day = baseline$arrival_rates[[name]],
                 Pathway = paste(profile$unit, collapse = " -> "),
                 Mean_stays_days = format_steps(profile$los),
                 SD_days = format_steps(civilian_step_sd(profile)))
    }))
    add_report_table_page("Routine Civilian Profiles", civilian_profiles)
    add_report_table_page("Civilian Warm-up Settings", make_parameter_table(
      baseline[setdiff(names(baseline), c("profiles", "arrival_rates"))]))
    add_report_table_page("Warm-up Duration by Replication", simulation_data$runs)
    add_report_text_page("Civilian Flow Interpretation", c(
      if (civilian_only) "Civilian-only scenario: day 0 starts observation after warm-up; no surge patients arrive."
      else "Resource results include both populations after surge onset (day 0).",
      if (civilian_only) "Bed wait tables include civilian bed requests and identify pending requests."
      else "Bed wait tables distinguish civilian and surge bed requests.",
      "Civilian arrivals continue throughout warm-up and follow-up without resetting beds or patients.",
      "The warm-up is a fixed duration chosen by the user; no automated stability test is run, and equilibrium is not established by this report.",
      "Warm-up uses existing beds; added capacity is activated at the start of observation."))
  }
  add_report_table_page("Fallback Configuration", make_fallback_configuration_table(profile_config))
  if (!civilian_only) add_report_table_page("Recommended Expansion", make_expansion_table(n_result))
  add_report_table_page("Average Utilization of Hospital Resources", utilization_results_table(simulation_data))
  add_report_table_page("Bottlenecks in Hospital Resource Usage", queue_results_table(simulation_data))

  add_resource_plot_page(simulation_data$resources, var = "server")
  add_resource_plot_page(simulation_data$resources, var = "queue")
  add_report_table_page("Bed waiting times (days; requests with a resolved wait)", bed_wait_results_table(simulation_data))
  add_report_table_page("Boarding times (days; time holding a bed while awaiting the next unit)",
                        boarding_results_table(simulation_data))

  invisible(file)
}
