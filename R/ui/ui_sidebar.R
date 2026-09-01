# Sidebar UI ------------------------------------------------------------
build_sidebar <- function() {
  col1 <- 6
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
          "run a simulation, estimate bed expansion, and interpret the results."
        ),
        data.position = "right"
      )
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
                shiny::numericInput("n_patients", "Patients per Day", min = 1, max = 100, value = 10),
                shiny::numericInput("sim_days", "Simulation Duration (days)", min = 1, max = 100, value = 30)
              ),
              shiny::column(
                width = col1,
                shiny::numericInput("duration", "Arrival Period (days)", min = 1, max = 100, value = 10),
                shiny::numericInput("num_sims", "Number of simulations", min = 1, max = 100, value = 12)
              )
            )
          ),
          id = "tour_simulation_parameters",
          data.step = 7,
          data.intro = paste(
            "<strong>Simulation parameters</strong><br>",
            "Set daily arrivals, the arrival period, the simulation horizon,",
            "and the number of independent simulation replications."
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
          data.step = 8,
          data.intro = paste(
            "<strong>Run the baseline scenario.</strong><br>",
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
            shiny::h4("Maximum Allowed Queue Lengths:"),
            shiny::fluidRow(
              shiny::column(width = 6, shiny::numericInput("congestion_index", "Med/Surg", min = 1, max = 100, value = 10, step = 5)),
              shiny::column(width = 6, shiny::numericInput("congestion_index_icu", "ICU", min = 1, max = 100, value = 10, step = 5))
            ),
            shiny::helpText(paste(
              "Current capacity is checked first."# An internal run with at least 500 beds",
              # "per unit measures unconstrained primary demand. Fallback demand can exceed",
              # "that reference, so failing units grow exponentially up to a fallback-safe",
              # "ceiling. Each unit must independently meet its limit in 80% of validation simulations."
            )),
            shiny::actionButton("run_N", "Estimate Bed Expansion", class = "btn-primary"),
            shiny::h4("Additional Bed Capacity:"),
            shiny::fluidRow(
              shiny::column(width = 6, shiny::numericInput("genmed_msf", "HxS Med/Surg", min = 0, max = 100, value = 0)),
              shiny::column(width = 6, shiny::numericInput("icu_msf", "HxS ICU", min = 0, max = 100, value = 0))
            )
          ),
          id = "tour_bed_expansion",
          data.step = 9,
          data.intro = paste(
            "<strong>Estimate additional capacity.</strong><br>",
            "Enter acceptable GenMed and ICU queue limits. The optimizer first measures",
            "unconstrained primary demand with at least 500 beds in every unit.",
            "It then grows only failing resources exponentially; this can exceed primary",
            "demand when fallbacks route patients into GenMed or ICU. The 80% target is",
            "validated independently for each unit. Confirm before starting; only one",
            "calculation can run at a time."
          ),
          data.position = "right"
        )
      )
    )
  )
}

