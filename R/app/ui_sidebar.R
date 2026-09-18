# Sidebar UI ------------------------------------------------------------
build_sidebar <- function() {
  col1 <- 6
  search_config <- bed_search_configs$development
  compliance_note <- sprintf(
    "In at least %.1f%% of replications, the maximum queues of GenMed and ICU must both stay within their limits during the full observation period.",
    100 * search_config$reliability_level)
  sidebar <- dashboardSidebar(
    width = 305,
    shiny::div(
      class = "guided-tour-launch",
      rintrojs::introBox(
        shiny::actionButton(
          "guided_tour", "Start Guided Tour",
          icon = shiny::icon("compass"),
          class = "btn-info", width = "100%"
        ),
        data.step = 1,
        data.intro = paste(
          "<strong>Welcome to the Patient Surge Model.</strong><br>",
          "This guided tour explains how to configure the hospital,",
          "run a simulation, estimate bed expansion, and interpret the results. Choose routine civilian operation only for the baseline, or Surge event to add military arrivals. Keep the same observation horizon when comparing scenarios."
        ),
        data.position = "right"
      )
    ),
    shiny::div(style = "padding: 0 10px;",
      shiny::selectInput("scenario_mode", "Scenario to run",
        choices = c("Surge event" = "surge", "Routine civilian operation only" = "civilian_only"),
        selected = "surge"),
      shiny::conditionalPanel("input.scenario_mode == 'civilian_only'",
        shiny::helpText("Enable Routine Civilian Flow and save civilian profiles in Hospital Setup. Surge profiles are not required."))
    ),
    sidebarMenu(
      id = "sidebarid",
      shinydashboard::menuItem("Hospital Setup", tabName = "HospitalSetup", icon = shiny::icon("hospital")),
      shinydashboard::menuItem("Model Parameters", tabName = "InitCondition", icon = shiny::icon("sliders-h")),
      shiny::conditionalPanel(
        'input.sidebarid == "InitCondition"',
        rintrojs::introBox(
          shiny::tags$details(
            class = "sidebar-section", open = NA,
            shiny::tags$summary("Simulation Parameters"),
            shiny::fluidRow(
              shiny::column(
                width = col1,
                shiny::conditionalPanel("input.scenario_mode != 'civilian_only'",
                  shiny::numericInput("n_patients", "Surge Patients per Day", min = 1, max = 20, value = 10, step = "any")),
                shiny::numericInput("sim_days", "Observation Duration (days)", min = 1, max = 50, value = app_development_config$sim_days)
              ),
              shiny::column(
                width = col1,
                shiny::conditionalPanel("input.scenario_mode != 'civilian_only'",
                  shiny::numericInput("duration", "Surge Arrival Period (days)", min = 1, max = 30, value = 10)),
                shiny::numericInput("num_sims", "Number of simulations", min = 1, max = 20, value = app_development_config$num_sims)
              )
            ),
            shiny::conditionalPanel("input.scenario_mode != 'civilian_only'",
              shiny::selectInput("surge_arrival_process", "Surge arrival process",
                choices = c("Evenly spaced" = "even", "Poisson (random arrivals)" = "poisson")),
              shiny::helpText("For Poisson arrivals, patients/day is the mean rate; the actual count varies. The observation duration must cover the full arrival period.")),
            shiny::numericInput("simulation_seed", "Simulation seed", value = app_development_config$simulation_seed, min = 1, step = 1),
            shiny::helpText(sprintf("Day 0 starts observation and surge arrivals. With civilian flow enabled, warm-up occurs before day 0. Bed search uses %d replications per candidate and %d independent final replications in development mode.",
              search_config$num_sims, search_config$final_num_sims))
          ),
          id = "tour_simulation_parameters",
          data.step = 8,
          data.intro = paste(
            "<strong>Simulation parameters</strong><br>",
            "Choose evenly spaced or Poisson surge arrivals. For Poisson, patients/day is a mean rate and the total varies. Set the arrival period and a common observation horizon extending beyond it,",
            "then set independent replications and a reproducible seed. Day zero follows warm-up; the hospital is not emptied."
          ),
          data.position = "right"
        ),
        rintrojs::introBox(
          shiny::actionButton(
            "run_sim",
            "Run Simulation",
            class = "btn-primary",
            onclick = paste0(
              "$('#run_sim, #run_N').prop('disabled', true);",
              "$('#run_sim').text('Simulation is already running...');",
              "$('#run_N').text('Simulation is already running...');"
            )
          ),
          data.step = 9,
          data.intro = paste(
            "<strong>Run the selected scenario.</strong><br>",
            "The current hospital configuration is simulated and the plots",
            "and tables are refreshed. While it runs, both calculation buttons",
            "are disabled so another calculation cannot start."
          ),
          data.position = "right"
        ),
        rintrojs::introBox(
          shiny::tags$details(
            class = "sidebar-section", open = NA,
            shiny::tags$summary("Calculate HxS Expansion"),
            shiny::h4("Maximum Queue Limits (Patients):"),
            shiny::fluidRow(
              shiny::column(width = 6, shiny::numericInput("congestion_index", "Med/Surg", min = 1, max = 100, value = 10, step = 5)),
              shiny::column(width = 6, shiny::numericInput("congestion_index_icu", "ICU", min = 1, max = 100, value = 10, step = 5))
            ),
            shiny::helpText(paste(
              compliance_note
            )),
            shiny::conditionalPanel("input.scenario_mode != 'civilian_only'",
              shiny::actionButton("run_N", "Estimate Bed Expansion", class = "btn-primary")),
            shiny::h4("Additional Bed Capacity:"),
            shiny::fluidRow(
              shiny::column(width = 6, shiny::numericInput("genmed_msf", "HxS Med/Surg", min = 0, max = 100, value = 0)),
              shiny::column(width = 6, shiny::numericInput("icu_msf", "HxS ICU", min = 0, max = 100, value = 0))
            ),
            shiny::helpText("HxS additions apply to the selected scenario. Use zero additions to evaluate existing capacity.")
          ),
          id = "tour_bed_expansion",
          data.step = 10,
          data.intro = paste(
            "<strong>Estimate additional capacity.</strong><br>",
            "Enter acceptable GenMed and ICU queue limits. The optimizer first checks current capacity",
            "and, if needed, estimates demand with at least 500 beds in every unit.",
            "It then grows only failing resources exponentially; this can exceed primary",
            "demand when fallbacks route patients into GenMed or ICU.", compliance_note,
            "Confirm before starting; only one",
            "calculation can run at a time."
          ),
          data.position = "right"
        )
      )
    )
  )
}
