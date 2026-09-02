# Application server ----------------------------------------------------
app_server <- function(input, output, session) {
    # ####
    # res_auth <- secure_server(check_credentials = check_credentials(credentials))
    # output$auth_output <- shiny::renderPrint({ reactiveValuesToList(res_auth) })
    
    simulation_data <- shiny::reactiveVal(NULL)
    profile_config <- hospital_profiles_server("profiles")
  
    shiny::observeEvent(input$guided_tour, {
      return_to_simulation_results <- base::I(paste(
        "var modelLink = document.querySelector(\"a[data-value='InitCondition']\");",
        "if (modelLink) modelLink.click();",
        "var resultTab = Array.from(document.querySelectorAll('.nav-tabs a'))",
        "  .find(function(tab) { return tab.textContent.trim() === 'Simulation Results'; });",
        "if (resultTab) resultTab.click();",
        "window.scrollTo(0, 0);"
      ))
  
      rintrojs::introjs(
        session,
        options = list(
          nextLabel = "Next",
          prevLabel = "Back",
          skipLabel = "Skip",
          doneLabel = "Done",
          showProgress = TRUE,
          showBullets = FALSE,
          showStepNumbers = FALSE,
          exitOnEsc = TRUE,
          exitOnOverlayClick = FALSE,
          scrollToElement = TRUE,
          overlayOpacity = 0.55
        ),
        events = list(
          onbeforechange = base::I(paste(
            "if (targetElement && targetElement.id === 'tour_simulation_parameters') {",
            "  var modelLink = document.querySelector(\"a[data-value='InitCondition']\");",
            "  if (modelLink) modelLink.click();",
            "}",
            "rintrojs.callback.switchTabs(targetElement);"
          )),
          oncomplete = return_to_simulation_results,
          onexit = return_to_simulation_results
        )
      )
    })
    bed_result_signature <- shiny::reactiveVal(NULL)
    applied_recommendation <- shiny::reactiveVal(NULL)
    bed_search_running <- shiny::reactiveVal(FALSE)
    simulation_running <- shiny::reactiveVal(FALSE)
    bed_search_trigger <- shiny::reactiveVal(0L)
  
    update_calculation_buttons <- function() {
      session$sendCustomMessage(
        "calculation-state",
        list(
          simulation_running = isTRUE(simulation_running()),
          bed_search_running = isTRUE(bed_search_running())
        )
      )
    }
  
    current_bed_search_signature <- shiny::reactive({
      config <- profile_config()
      if (is.null(config)) return(NULL)
      evaluated_capacities <- config$capacities
      if ("ICU" %in% names(evaluated_capacities)) {
        evaluated_capacities[["ICU"]] <- evaluated_capacities[["ICU"]] + input$icu_msf
      }
      if ("GenMed" %in% names(evaluated_capacities)) {
        evaluated_capacities[["GenMed"]] <- evaluated_capacities[["GenMed"]] + input$genmed_msf
      }
      list(
        profile_config = config,
        evaluated_capacities = evaluated_capacities,
        n_patients = input$n_patients,
        duration = input$duration,
        sim_days = input$sim_days,
        num_sims = input$num_sims,
        congestion_index = input$congestion_index,
        congestion_index_icu = input$congestion_index_icu,
        genmed_msf = input$genmed_msf,
        icu_msf = input$icu_msf
      )
    })
    
    shiny::observeEvent(input$run_sim, {
      if (isTRUE(bed_search_running())) {
        shiny::showNotification(
          "A bed-expansion estimate is already running. Wait for it to finish before running a simulation.",
          type = "warning",
          duration = 8
        )
        return(invisible(NULL))
      }
      if (isTRUE(simulation_running())) {
        shiny::showNotification(
          "A simulation is already running. Please wait for it to finish.",
          type = "warning",
          duration = 8
        )
        return(invisible(NULL))
      }
  
      simulation_running(TRUE)
      update_calculation_buttons()
      on.exit({
        simulation_running(FALSE)
        update_calculation_buttons()
      }, add = TRUE)
  
      config <- profile_config()
      shiny::validate(shiny::need(
        !is.null(config),
        "Complete a valid Hospital Setup before running the simulation."
      ))
      # Drop the previous simulation before allocating the next replication bank.
      simulation_data(NULL)
      invisible(base::gc(full = TRUE))
  
      params <- reactiveValuesToList(input)
      capacities <- config$capacities
      if ("ICU" %in% names(capacities)) capacities[["ICU"]] <- capacities[["ICU"]] + params$icu_msf
      if ("GenMed" %in% names(capacities)) capacities[["GenMed"]] <- capacities[["GenMed"]] + params$genmed_msf
  
      shiny::withProgress(message = "Running simulations...", value = 0, {
        results <- future_lapply(seq_len(params$num_sims), function(i) {
          sim <- run_simulation(
            capacities = capacities,
            duration = params$duration,
            n_patients = params$n_patients,
            sim_days = params$sim_days,
            patient_profiles = config$patient_profiles,
            profile_prob = config$profile_prob,
            fallbacks = config$fallbacks
          )
          list(
            resources = dplyr::mutate(get_hospital_mon_resources(sim), replication = i),
            arrivals = dplyr::mutate(get_hospital_mon_arrivals(sim), replication = i)
          )
        },
          future.seed = TRUE,
          future.chunk.size = max(1L, ceiling(params$num_sims / max(1L, workers)))
        )
  
        simulation_data(list(
          resources = dplyr::bind_rows(lapply(results, `[[`, "resources")),
          arrivals = dplyr::bind_rows(lapply(results, `[[`, "arrivals")),
          profile_config = config,
          capacities = capacities
        ))
      })
    })
  
  
    shiny::observeEvent(input$run_N, {
      if (isTRUE(simulation_running())) {
        shiny::showNotification(
          "A simulation is already running. Wait for it to finish before estimating bed expansion.",
          type = "warning",
          duration = 8
        )
        return(invisible(NULL))
      }
      if (isTRUE(bed_search_running())) {
        shiny::showNotification(
          "A bed-expansion estimate is already running. Please wait for it to finish.",
          type = "warning",
          duration = 8
        )
        return(invisible(NULL))
      }
  
      shiny::showModal(shiny::modalDialog(
        title = "Confirm bed-expansion estimate",
        shiny::p(
          paste(
            "This calculation may take several minutes and only one",
            "bed-expansion estimate can run at a time."
          )
        ),
        shiny::p("Do you want to start the calculation now?"),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(
            "confirm_run_N",
            "Start calculation",
            class = "btn-primary",
            onclick = paste0(
              "$('#run_sim, #run_N').prop('disabled', true);",
              "$('#run_sim').text('Bed expansion is already running...');",
              "$('#run_N').text('Bed expansion is already running...');"
            )
          )
        ),
        easyClose = FALSE
      ))
    })
  
    find_result <- shiny::reactiveVal(NULL)
    last_patient_profile_signature <- shiny::reactiveVal(NULL)
  
    shiny::observeEvent(profile_config(), {
      config <- profile_config()
      if (is.null(config)) return(invisible(NULL))
  
      profile_signature <- serialize(
        list(
          patient_profiles = config$patient_profiles,
          profile_prob = config$profile_prob,
          fallbacks = config$fallbacks
        ),
        connection = NULL,
        version = 2
      )
      previous_signature <- shiny::isolate(last_patient_profile_signature())
      last_patient_profile_signature(profile_signature)
  
      # The first valid configuration initializes the signature without clearing
      # inputs. Later trajectory or arrival-percentage changes invalidate every
      # result that depended on the previous patient mix.
      if (is.null(previous_signature) ||
          identical(previous_signature, profile_signature)) {
        return(invisible(NULL))
      }
  
      shiny::updateNumericInput(session, "genmed_msf", value = 0)
      shiny::updateNumericInput(session, "icu_msf", value = 0)
      simulation_data(NULL)
      find_result(NULL)
      bed_result_signature(NULL)
      applied_recommendation(NULL)
      invisible(base::gc(full = TRUE))
  
      shiny::showNotification(
        paste(
          "Patient profiles changed. Additional bed capacity was reset to zero",
          "and previous simulation results were cleared from memory."
        ),
        type = "message",
        duration = 8
      )
    }, ignoreInit = FALSE)
  
    shiny::observeEvent(input$confirm_run_N, {
      if (isTRUE(simulation_running())) {
        shiny::removeModal()
        shiny::showNotification(
          "A simulation is already running. Wait for it to finish before estimating bed expansion.",
          type = "warning",
          duration = 8
        )
        update_calculation_buttons()
        return(invisible(NULL))
      }
      if (isTRUE(bed_search_running())) {
        shiny::removeModal()
        shiny::showNotification(
          "A bed-expansion estimate is already running. Please wait for it to finish.",
          type = "warning",
          duration = 8
        )
        return(invisible(NULL))
      }
  
      shiny::removeModal()
      bed_search_running(TRUE)
      find_result(NULL)
      bed_result_signature(NULL)
      applied_recommendation(NULL)
      invisible(base::gc(full = TRUE))
      update_calculation_buttons()
      bed_search_trigger(shiny::isolate(bed_search_trigger()) + 1L)
    }, ignoreInit = TRUE)
  
    shiny::observeEvent(bed_search_trigger(), {
      shiny::req(bed_search_trigger() > 0)
      on.exit({
        bed_search_running(FALSE)
        update_calculation_buttons()
      }, add = TRUE)
  
      applied_recommendation(NULL)
      result <- shiny::withProgress(
        message = "Calculation in progress",
        detail = "This may take a while...",
        value = 0,
        {
          evaluation_signature <- current_bed_search_signature()
          config <- evaluation_signature$profile_config
          shiny::validate(shiny::need(
            !is.null(config),
            "Complete a valid Hospital Setup before estimating bed expansion."
          ))
          shiny::validate(shiny::need(
            all(c("GenMed", "ICU") %in% names(config$capacities)),
            "Bed expansion requires both GenMed and ICU in Hospital Setup."
          ))
          search_mode <- "precise"
          bed_search_config <- bed_search_configs[[search_mode]]
  
          capacities <- evaluation_signature$evaluated_capacities
          result <- find_n_needed(
            capacities = capacities,
            duration = evaluation_signature$duration,
            n_patients = evaluation_signature$n_patients,
            sim_days = evaluation_signature$sim_days,
            patient_profiles = config$patient_profiles,
            profile_prob = config$profile_prob,
            fallbacks = config$fallbacks,
            num_sims = evaluation_signature$num_sims,
            search_num_sims = min(bed_search_config$search_num_sims, evaluation_signature$num_sims),
            max_evaluations = bed_search_config$max_evaluations,
            max_validation_evaluations = bed_search_config$max_validation_evaluations,
            minimum_step = bed_search_config$minimum_step,
            search_queue_tolerance = bed_search_config$search_queue_tolerance,
            demand_safety_factor = bed_search_config$demand_safety_factor,
            reliability_level = bed_search_config$reliability_level,
            search_seed = bed_search_config$search_seed,
            congestion_index_opt = shiny::req(evaluation_signature$congestion_index),
            congestion_index_opt_ICU = shiny::req(evaluation_signature$congestion_index_icu),
            workers = workers,
            verbose = TRUE
          )
          result$search_mode <- search_mode
          result$search_mode_label <- "70% reliability efficient search"
          bed_result_signature(evaluation_signature)
          result
        }
      )
      find_result(result)
      shiny::showNotification(
        "Bed-expansion estimate completed.",
        type = "message",
        duration = 6
      )
    }, ignoreInit = TRUE)
    bed_result_is_current <- shiny::reactive({
      result <- find_result()
      !is.null(result) && identical(
        bed_result_signature(),
        current_bed_search_signature()
      )
    })
  
    current_find_result <- shiny::reactive({
      if (!isTRUE(bed_result_is_current())) return(NULL)
      find_result()
    })
    output$formula_output <- shiny::renderUI({
      shiny::withMathJax(shiny::HTML(my_formula()))
    })
    
    output$N_tex <- shiny::renderUI({
      applied <- applied_recommendation()
      if (!is.null(applied) && identical(
        applied$signature,
        current_bed_search_signature()
      )) {
        return(shiny::div(
          class = "alert alert-success",
          shiny::tags$strong("Recommended expansion applied"),
          shiny::tags$br(),
          sprintf(
            "HxS Med/Surg is now %d and HxS ICU is now %d. Click Run Simulation to update the plots and tables. Do not run Estimate Bed Expansion again unless another model input changes.",
            applied$genmed_msf,
            applied$icu_msf
          )
        ))
      }
  
      n_opt <- find_result()
      if (is.null(n_opt)) return(NULL)
      if (!isTRUE(bed_result_is_current())) {
        return(shiny::div(
          class = "alert alert-warning",
          shiny::tags$strong("Outdated bed-expansion recommendation"),
          shiny::tags$br(),
          "Hospital profiles, capacities, demand, queue limits, HxS inputs, or the number of simulations changed after this recommendation was calculated. Click Estimate Bed Expansion again before applying it."
        ))
      }
  
      shiny::validate(
        shiny::need(
          isTRUE(n_opt$converged),
          sprintf("The bed search did not find a validated capacity after %d evaluations.", n_opt$evaluations)
        )
      )
  
      evaluated <- bed_result_signature()
      N_added <- n_opt$N_added
      N_added_ICU <- n_opt$N_added_ICU
      search_mode_label <- if (is.null(n_opt$search_mode_label)) {
        "Selected mode"
      } else {
        n_opt$search_mode_label
      }
      refinement_message <- if (isTRUE(n_opt$refinement_complete)) {
        "Independent validation refinement completed."
      } else {
        "The independently validated recommendation may be conservative because the refinement budget was reached."
      }
  
      color_medsurg <- ifelse(N_added > 0, "#dc3545", "#28a745")
      color_icu <- ifelse(N_added_ICU > 0, "#dc3545", "#28a745")
      border_color <- ifelse(
        N_added > 0 | N_added_ICU > 0,
        "#dc3545",
        "#28a745"
      )
  
      shiny::tagList(
        shiny::HTML(sprintf(
          "
      <div style='
        background-color:#f8f9fa;
        border-left:6px solid %s;
        padding:15px;
        margin-top:10px;
        font-size:20px;
        font-weight:bold;
        color:#333333;
        border-radius:5px;'>
  
        Recommended Expansion:<br>
        <span style='font-size:14px; font-weight:normal;'>Mode: %s. Recommendation is additional beds beyond current capacity and HxS inputs.</span><br>
        <span style='font-size:14px; font-weight:normal;'>Evaluated scenario: GenMed %d beds; ICU %d beds; queue limits %.2f and %.2f; %d patients/day; %d simulations.</span><br>
        <span style='font-size:14px; font-weight:normal;'>Reliability target per unit: %.0f%%. Validated GenMed: %.0f%%; ICU: %.0f%%. Joint diagnostic: %.0f%%.</span><br>
        <span style='font-size:14px; font-weight:normal;'>%s</span><br>
  
        Add <span style='color:%s;'>%d</span> beds to <b>Med/Surg</b> and
        <span style='color:%s;'>%d</span> beds to <b>ICU HxS</b>.
  
      </div>
      ",
          border_color,
          search_mode_label,
          evaluated$evaluated_capacities[["GenMed"]],
          evaluated$evaluated_capacities[["ICU"]],
          evaluated$congestion_index,
          evaluated$congestion_index_icu,
          evaluated$n_patients,
          evaluated$num_sims,
          100 * n_opt$reliability_level,
          100 * n_opt$reliability_GenMed,
          100 * n_opt$reliability_ICU,
          100 * n_opt$joint_reliability,
          refinement_message,
          color_medsurg,
          N_added,
          color_icu,
          N_added_ICU
        )),
        shiny::actionButton(
          "apply_recommended_expansion",
          "Apply Recommended Expansion",
          icon = shiny::icon("check"),
          class = "btn-primary"
        ),
        shiny::helpText(
          "This copies the recommendation to the HxS fields. Then click Run Simulation to update the plots and tables."
        )
      )
    })
  
    shiny::observeEvent(input$apply_recommended_expansion, {
      n_opt <- current_find_result()
      shiny::req(!is.null(n_opt), isTRUE(n_opt$converged), isTRUE(n_opt$search_complete))
      evaluated <- bed_result_signature()
      new_genmed_msf <- evaluated$genmed_msf + n_opt$N_added
      new_icu_msf <- evaluated$icu_msf + n_opt$N_added_ICU
      applied_signature <- evaluated
      applied_signature$genmed_msf <- new_genmed_msf
      applied_signature$icu_msf <- new_icu_msf
      applied_signature$evaluated_capacities[["GenMed"]] <-
        evaluated$evaluated_capacities[["GenMed"]] + n_opt$N_added
      applied_signature$evaluated_capacities[["ICU"]] <-
        evaluated$evaluated_capacities[["ICU"]] + n_opt$N_added_ICU
  
      applied_recommendation(list(
        signature = applied_signature,
        genmed_msf = new_genmed_msf,
        icu_msf = new_icu_msf
      ))
      simulation_data(NULL)
      shiny::updateNumericInput(session, "genmed_msf", value = new_genmed_msf)
      shiny::updateNumericInput(session, "icu_msf", value = new_icu_msf)
      shiny::showNotification(
        "Recommended beds were applied to the HxS fields. Click Run Simulation to update the results; no second optimization is needed.",
        type = "message",
        duration = 10
      )
    })
    # Plot output
    output$resource_plot <- plotly::renderPlotly({
      shiny::req(simulation_data())
      make_resource_plot(simulation_data()$resources, var = "server")
    })
    ##########
    output$queue_plot <- plotly::renderPlotly({
      shiny::req(simulation_data())
      make_resource_plot(simulation_data()$resources, var = "queue")
    })
    
    ############
    output$utilization_table <- shiny::renderTable({
      # # Extract resource metrics
      shiny::req(simulation_data())
      resource_metrics <- simulation_data()$resources
      summary_utilization(resource_metrics)
    })
    
    
    #############################################################################
    #############################################################################
    
    output$queue_analysis <- shiny::renderTable({
      # resource_metrics <-resource_metrics$resources
      shiny::req(simulation_data())
      resource_metrics <- simulation_data()$resources
      
      summary_queue(resource_metrics)
    })
    
    ###################################################################################################
    ###################################################################################################
    
    output$mean_stay <- plotly::renderPlotly({
      # shiny::req(patient_metrics  <- simulation_result())
      # patient_metrics  <- patient_metrics$arrivals
      shiny::req(simulation_data())
      patient_metrics <- simulation_data()$arrivals
      shiny::validate(
        shiny::need(nrow(patient_metrics) > 0, "No patient arrivals were recorded."),
        shiny::need(any(patient_metrics$finished %in% TRUE), "No patients completed treatment.")
      )
      
      patient_summary <- patient_metrics |>
        dplyr::group_by(replication) |>
        dplyr::summarise(
          total_patients = dplyr::n(),
          completion_rate = safe_mean(as.numeric(finished)),
          avg_treatment_time = safe_mean(activity_time[finished], default = NA_real_),
          avg_wait_time = safe_mean(end_time - start_time - activity_time, default = NA_real_),
          .groups = "drop"
        ) |>
        dplyr::filter(is.finite(avg_treatment_time), is.finite(avg_wait_time))
      
      p <- ggplot2::ggplot(patient_summary, ggplot2::aes(x = avg_treatment_time)) +
        ggplot2::geom_histogram(bins = 20, fill = "steelblue", alpha = 0.8) +
        ggplot2::geom_vline(ggplot2::aes(xintercept = mean(avg_treatment_time)),
                   color = "red", linetype = "dashed", linewidth = 1
        ) + # Mean line
        ggplot2::labs(
          title = "Distribution of Average Treatment Time",
          subtitle = paste("Across", nrow(patient_summary), "simulations"),
          x = "Average Treatment Time (Days)",
          y = "Number of Simulations"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::annotate("text",
                 x = mean(patient_summary$avg_treatment_time), # Position text at mean
                 y = 5, # Adjust y position to avoid overlapping with the histogram
                 label = paste("Overall Mean:", round(mean(patient_summary$avg_treatment_time), 2), "days"),
                 vjust = -1, color = "darkblue"
        )
      
      p1 <- plotly::plot_ly(patient_summary,
                    x = ~avg_wait_time, type = "histogram", nbinsx = 20,
                    marker = list(color = "red", line = list(color = "black", width = 1))
      ) %>%
        plotly::layout(
          title = "", # "Histogram of Average Wait Time",
          xaxis = list(title = "Average Wait Time (Days)"),
          yaxis = list(title = "Number of Simulations"),
          shapes = list(list(
            type = "line", x0 = mean(patient_summary$avg_wait_time),
            x1 = mean(patient_summary$avg_wait_time), yref = "paper", y0 = 0, y1 = 1,
            line = list(color = "blue", dash = "dash")
          )),
          annotations = list(
            text = paste("Overall Mean:", round(mean(patient_summary$avg_wait_time), 2), "days"),
            x = mean(patient_summary$avg_wait_time), y = 5, showarrow = FALSE, yshift = -10
          )
        )
      plotly::subplot(
        plotly::ggplotly(p),
        plotly::ggplotly(p1),
        nrows = 1, # Arrange the plots side by side
        shareX = TRUE, # Optional: Share the X-axis if appropriate
        titleX = TRUE, titleY = TRUE
      )
    })
  
  
  
    output$download_report <- shiny::downloadHandler(
      filename = function() {
        paste0("patient_surge_model_report_", format(Sys.Date(), "%Y%m%d"), ".pdf")
      },
      content = function(file) {
        shiny::req(simulation_data())
        n_result <- tryCatch(
          current_find_result(),
          error = function(err) NULL
        )
        generate_simulation_pdf_report(
          file = file,
          params = build_report_params(input, simulation_data()$profile_config),
          simulation_data = simulation_data(),
          profile_config = simulation_data()$profile_config,
          n_result = n_result
        )
      }
    )
  }
