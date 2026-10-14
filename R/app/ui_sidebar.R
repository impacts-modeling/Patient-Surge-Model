# Sidebar UI ------------------------------------------------------------
# Info icon placed right after a label; clicking it shows or hides the matching
# sidebar_info_text() (hidden by default) using the Bootstrap 3 collapse plugin
# bundled with shinydashboard. The icon is a link, so clicking it inside a
# checkbox label does not toggle the checkbox.
with_info <- function(label, info_id) {
  shiny::tagList(
    label,
    shiny::tags$a(
      href = paste0("#", info_id), class = "sidebar-info-icon", role = "button",
      `data-toggle` = "collapse", `aria-expanded` = "false", `aria-controls` = info_id,
      title = "Show information", shiny::icon("circle-info")
    )
  )
}

# A div (not helpText's <p>) so it can also hold conditionalPanel()s.
sidebar_info_text <- function(info_id, ...) {
  shiny::div(id = info_id, class = "collapse sidebar-info-text",
             shiny::div(class = "help-block", ...))
}

build_sidebar <- function() {
  col1 <- 6
  # Surge inputs apply to the surge event and to the unlimited-capacity scenario when
  # surge arrivals are included.
  surge_inputs_condition <- paste(
    "input.scenario_mode == 'surge' ||",
    "(input.scenario_mode == 'unlimited' && input.unlimited_include_surge)")
  search_config <- bed_search_configs$development
  compliance_note <- sprintf(
    "In at least %.1f%% of replications, the mean wait for a first bed (days, while holding no bed) in GenMed and in ICU must both stay within their limits during the observation period.",
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
      shiny::selectInput("scenario_mode", with_info("Scenario to run", "info_scenario_mode"),
        choices = c("Surge event" = "surge", "Routine civilian operation only" = "civilian_only",
                    "Unlimited capacity (analytic)" = "unlimited"),
        selected = "surge"),
      shiny::conditionalPanel("input.scenario_mode == 'unlimited'",
        shiny::checkboxInput("unlimited_include_surge", "Include surge arrivals", value = TRUE)),
      sidebar_info_text("info_scenario_mode",
        shiny::conditionalPanel("input.scenario_mode == 'surge'",
          "Surge arrivals, plus routine civilian flow if it is enabled in Hospital Setup."),
        shiny::conditionalPanel("input.scenario_mode == 'civilian_only'",
          "Enable Routine Civilian Flow and save civilian profiles in Hospital Setup. Surge profiles are not required."),
        shiny::conditionalPanel("input.scenario_mode == 'unlimited'",
          "Occupancy under unlimited capacity is computed analytically (no simulation). Bed counts, replications and seed are not used. Untick Include surge arrivals to compute routine civilian operation only (it requires Routine Civilian Flow to be enabled in Hospital Setup)."))
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
                shiny::conditionalPanel(surge_inputs_condition,
                  shiny::numericInput("n_patients", "Surge Patients per Day", min = 1, max = 20, value = 10, step = "any")),
                # Keep the icon on the label's second line so this label stays two
                # lines tall, aligned with "Number of simulations" beside it.
                shiny::numericInput("sim_days",
                                    shiny::tagList("Observation ", shiny::span(class = "text-nowrap",
                                      with_info("Duration (days)", "info_sim_days"))),
                                    min = 1, max = 50, value = app_development_config$sim_days)
              ),
              shiny::column(
                width = col1,
                shiny::conditionalPanel(surge_inputs_condition,
                  shiny::numericInput("duration", "Surge Arrival Period (days)", min = 1, max = 30, value = 10)),
                shiny::conditionalPanel("input.scenario_mode != 'unlimited'",
                  shiny::numericInput("num_sims",
                                      shiny::tagList("Number of ", shiny::span(class = "text-nowrap",
                                        with_info("simulations", "info_num_sims"))),
                                      min = 1,
                                      max = max_num_sims_for(FALSE), value = app_development_config$num_sims))
              )
            ),
            sidebar_info_text("info_sim_days",
              "Day 0 starts observation and surge arrivals. With civilian flow enabled, warm-up occurs before day 0."),
            sidebar_info_text("info_num_sims", sprintf(
              "At most %d simulations with routine civilian arrivals enabled (every replication includes a warm-up) and %d without, to keep run times manageable.",
              max_num_sims_for(TRUE), max_num_sims_for(FALSE))),
            shiny::fluidRow(
              shiny::column(
                width = col1,
                shiny::conditionalPanel(surge_inputs_condition,
                  shiny::selectInput("surge_arrival_process",
                    shiny::tagList("Surge arrival ", shiny::span(class = "text-nowrap",
                      with_info("process", "info_surge_arrival_process"))),
                    choices = c("Even" = "even", "Poisson" = "poisson"),
                    selected = "even"))
              ),
              shiny::column(
                width = col1,
                shiny::conditionalPanel("input.scenario_mode != 'unlimited'",
                  shiny::numericInput("simulation_seed", "Simulation seed", value = app_development_config$simulation_seed, min = 1, step = 1))
              )
            ),
            shiny::conditionalPanel(surge_inputs_condition,
              sidebar_info_text("info_surge_arrival_process",
                "For Poisson arrivals, patients/day is the mean rate; the actual count varies. The observation duration must cover the full arrival period."))
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
        shiny::conditionalPanel("input.scenario_mode != 'unlimited'",
        rintrojs::introBox(
          shiny::tags$details(
            class = "sidebar-section", open = NA,
            shiny::tags$summary("Calculate HxS Expansion"),
            shiny::h4(with_info("Maximum Mean Wait Limits (Days):", "info_wait_limits")),
            sidebar_info_text("info_wait_limits", paste(compliance_note, sprintf(
              "Bed search uses %d replications per candidate and %d independent final replications in development mode.",
              search_config$num_sims, search_config$final_num_sims))),
            shiny::fluidRow(
              shiny::column(width = 6, shiny::numericInput("congestion_index", "GenMed", min = 0.01, max = 30, value = 1, step = 0.5)),
              shiny::column(width = 6, shiny::numericInput("congestion_index_icu", "ICU", min = 0.01, max = 30, value = 1, step = 0.5))
            ),
            shiny::conditionalPanel("input.scenario_mode != 'civilian_only'",
              shiny::actionButton("run_N", "Estimate Bed Expansion", class = "btn-primary")),
            shiny::h4(with_info("Additional Bed Capacity:", "info_hxs")),
            sidebar_info_text("info_hxs",
              "HxS additions apply to the selected scenario. Use zero additions to evaluate existing capacity."),
            shiny::fluidRow(
              shiny::column(width = 6, shiny::numericInput("genmed_msf", "HxS GenMed", min = 0, max = 100, value = 0)),
              shiny::column(width = 6, shiny::numericInput("icu_msf", "HxS ICU", min = 0, max = 100, value = 0))
            )
          ),
          id = "tour_bed_expansion",
          data.step = 10,
          data.intro = paste(
            "<strong>Estimate additional capacity.</strong><br>",
            "Enter acceptable GenMed and ICU mean-wait limits (days). The optimizer first checks current capacity",
            "and, if needed, starts from the analytic unlimited-capacity occupancy (no simulation).",
            "It then grows only failing resources exponentially; this can exceed primary",
            "demand when fallbacks route patients into GenMed or ICU.", compliance_note,
            "Confirm before starting; only one",
            "calculation can run at a time."
          ),
          data.position = "right"
        ))
      )
    )
  )
}
