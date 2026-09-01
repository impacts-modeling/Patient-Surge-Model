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
      "Search evaluations",
      "Validation evaluations",
      "Converged"
    ),
    Value = c(
      n_result$N_added %||% NA_integer_,
      n_result$N_added_ICU %||% NA_integer_,
      n_result$GenMed_N %||% NA_integer_,
      n_result$ICU_N %||% NA_integer_,
      n_result$search_evaluations %||% NA_integer_,
      n_result$validation_evaluations %||% NA_integer_,
      isTRUE(n_result$converged)
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

add_resource_plot_page <- function(resources, var = "server") {
  plot_data <- make_resource_data(resources, var = var)
  y_label <- switch(
    var,
    "server" = "Number of Beds Occupied",
    "queue" = "Number of Patients Waiting",
    paste("Value of", var)
  )
  title <- switch(
    var,
    "server" = "Average Resource Utilization Over Time",
    "queue" = "Queue Lengths Over Time",
    paste("Plot of", var)
  )

  plot_obj <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = time1, y = median_val, color = resource)
  ) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::labs(title = title, x = "Time (days)", y = y_label, color = "Resource") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")

  print(plot_obj)
}

make_patient_summary <- function(arrivals) {
  arrivals |>
    dplyr::group_by(replication) |>
    dplyr::summarise(
      total_patients = dplyr::n(),
      completion_rate = safe_mean(as.numeric(finished)),
      avg_treatment_time = safe_mean(activity_time[finished], default = NA_real_),
      avg_wait_time = safe_mean(end_time - start_time - activity_time, default = NA_real_),
      .groups = "drop"
    ) |>
    dplyr::filter(is.finite(avg_treatment_time), is.finite(avg_wait_time))
}

add_patient_time_plot_page <- function(arrivals) {
  patient_summary <- make_patient_summary(arrivals)
  if (nrow(patient_summary) == 0) {
    add_report_text_page(
      "Distribution of Average Treatment/Wait Time",
      "No completed patient records were available to plot."
    )
    return(invisible(NULL))
  }

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))

  graphics::hist(
    patient_summary$avg_treatment_time,
    breaks = 20,
    col = "steelblue",
    border = "white",
    main = "Average Treatment Time",
    xlab = "Days",
    ylab = "Number of Simulations"
  )
  graphics::abline(v = mean(patient_summary$avg_treatment_time), col = "red", lty = 2, lwd = 2)
  graphics::mtext(
    paste("Mean:", round(mean(patient_summary$avg_treatment_time), 2), "days"),
    side = 3,
    line = 0.2,
    cex = 0.8
  )

  graphics::hist(
    patient_summary$avg_wait_time,
    breaks = 20,
    col = "tomato",
    border = "white",
    main = "Average Wait Time",
    xlab = "Days",
    ylab = "Number of Simulations"
  )
  graphics::abline(v = mean(patient_summary$avg_wait_time), col = "blue", lty = 2, lwd = 2)
  graphics::mtext(
    paste("Mean:", round(mean(patient_summary$avg_wait_time), 2), "days"),
    side = 3,
    line = 0.2,
    cex = 0.8
  )
}

build_report_params <- function(input, profile_config) {
  capacity_params <- as.list(profile_config$capacities)
  names(capacity_params) <- paste(names(capacity_params), "Available Beds")
  c(
    list(
      `Patients per Day` = input$n_patients,
      `Arrival Period (days)` = input$duration,
      `Simulation Duration (days)` = input$sim_days,
      `Number of simulations` = input$num_sims
    ),
    capacity_params,
    list(
      `Maximum Allowed Med/Surg Queue Length` = input$congestion_index,
      `Maximum Allowed ICU Queue Length` = input$congestion_index_icu,
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
    LOS_days = vapply(
      profiles,
      function(profile) paste(profile$los, collapse = " -> "),
      character(1)
    ),
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
      "resource utilization, queue bottlenecks, and patient treatment/wait time results."
    ),
    cex = 1.1
  )

  add_report_table_page("Model Parameter Configuration", make_parameter_table(params))
  add_report_table_page("Patient Profiles", make_profile_configuration_table(profile_config))
  add_report_table_page("Fallback Configuration", make_fallback_configuration_table(profile_config))
  add_report_table_page("Recommended Expansion", make_expansion_table(n_result))
  add_report_table_page("Average Utilization of Hospital Resources", summary_utilization(simulation_data$resources))
  add_report_table_page("Bottlenecks in Hospital Resource Usage", summary_queue(simulation_data$resources))

  add_resource_plot_page(simulation_data$resources, var = "server")
  add_resource_plot_page(simulation_data$resources, var = "queue")
  add_patient_time_plot_page(simulation_data$arrivals)

  invisible(file)
}