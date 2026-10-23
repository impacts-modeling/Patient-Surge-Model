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

# Patients to simulate. Offered only when routine civilian arrivals are
# enabled; otherwise the run is always "surge_only" (see effective scenario in
# app_server.R). Capacity (finite or unlimited) is chosen separately in
# Hospital Information.
scenario_choices <- c(
  "Surge + routine civilian" = "surge_civilian",
  "Routine civilian only (baseline)" = "civilian_only",
  "Surge only (empty hospital)" = "surge_only"
)

scenario_label <- function(scenario) {
  label <- names(scenario_choices)[scenario_choices %in% scenario]
  if (length(label)) label else as.character(scenario)
}

# JavaScript conditions for conditionalPanel(); they mirror the server's
# effective scenario: without civilian arrivals the scenario is surge only.
civilian_enabled_js <- "input['baseline-enabled']"
civilian_only_js <- sprintf("(%s && input.scenario_mode == 'civilian_only')", civilian_enabled_js)
unlimited_js <- "input['profiles-unlimited_capacity']"

build_sidebar <- function() {
  col1 <- 6
  # Surge inputs apply to every scenario that includes surge arrivals.
  surge_inputs_condition <- sprintf("!%s", civilian_only_js)
  simulation_inputs_condition <- sprintf("!%s", unlimited_js)
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
          "run a simulation, estimate bed expansion, and interpret the results.",
          "Use Next and Back to move between steps; the tour opens each page for you.",
          "The i icons explain individual inputs, and User Guide (sidebar) has the full documentation."
        ),
        data.position = "right"
      )
    ),
    sidebarMenu(
      id = "sidebarid",
      shinydashboard::menuItem("Hospital Setup", tabName = "HospitalSetup", icon = shiny::icon("hospital")),
      # Shown only with routine civilian arrivals; without them every run is
      # surge only on an empty hospital.
      shiny::conditionalPanel(
        civilian_enabled_js,
        shiny::div(
          style = "padding: 6px 10px 0;",
          shiny::selectInput("scenario_mode", with_info("Patients to simulate", "info_scenario_mode"),
                             choices = scenario_choices, selected = "surge_civilian"),
          sidebar_info_text("info_scenario_mode",
            shiny::conditionalPanel("input.scenario_mode == 'surge_civilian'",
              "Surge arrivals on top of routine civilian flow; the hospital is filled by the civilian warm-up before day 0."),
            shiny::conditionalPanel("input.scenario_mode == 'civilian_only'",
              "Routine civilian flow only (baseline for comparisons). Surge profiles are not required."),
            shiny::conditionalPanel("input.scenario_mode == 'surge_only'",
              "Surge arrivals only, on an empty hospital with the total beds entered (no civilian warm-up). Compare with Surge + routine civilian to see the effect of civilian occupancy."),
            "Unlimited capacity, if selected in Hospital Information, applies to any of these.")
        )
      ),
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
                shiny::conditionalPanel(simulation_inputs_condition,
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
              "At most %d simulations when the scenario includes routine civilian patients (every replication includes a warm-up) and %d for surge only, to keep run times manageable.",
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
                shiny::conditionalPanel(simulation_inputs_condition,
                  shiny::numericInput("simulation_seed", "Simulation seed", value = app_development_config$simulation_seed, min = 1, step = 1))
              )
            ),
            shiny::conditionalPanel(surge_inputs_condition,
              sidebar_info_text("info_surge_arrival_process",
                "For Poisson arrivals, patients/day is the mean rate; the actual count varies. The observation duration must cover the full arrival period."))
          ),
          id = "tour_simulation_parameters",
          data.step = 11,
          data.intro = paste(
            "<strong>Simulation parameters</strong><br>",
            "Set surge patients per day and the arrival period (Even or Poisson arrivals; for Poisson,",
            "patients/day is a mean rate and the total varies), and an observation horizon extending beyond it.",
            "Then set the number of independent simulations (at most 30 with routine civilian patients, 60 for surge only)",
            "and a reproducible seed. Day 0 follows the civilian warm-up; the hospital is not emptied.",
            "Keep the same observation horizon when comparing scenarios."
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
          data.step = 12,
          data.intro = paste(
            "<strong>Run the selected scenario.</strong><br>",
            "The current hospital configuration is simulated and the plots",
            "and tables on this page are refreshed. With unlimited capacity selected in Hospital",
            "Information, it instead computes the expected occupancy analytically (no simulation).",
            "While it runs, both calculation buttons are disabled so another calculation cannot start."
          ),
          data.position = "right"
        ),
        shiny::conditionalPanel(simulation_inputs_condition,
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
            shiny::conditionalPanel(sprintf("!%s", civilian_only_js),
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
          data.step = 13,
          data.intro = paste(
            "<strong>Estimate additional capacity.</strong><br>",
            "Enter acceptable GenMed and ICU mean-wait limits (days) and click Estimate Bed Expansion.",
            "The optimizer searches for the fewest extra beds that meet both limits jointly in at least",
            sprintf("%.0f%% of replications.", 100 * search_config$reliability_level),
            "Apply Recommended Expansion copies the result to the HxS fields; then run the simulation again.",
            "Not available for routine civilian only or with unlimited capacity. Details: User Guide, section 5."
          ),
          data.position = "right"
        ))
      ),
      shinydashboard::menuItem("User Guide", tabName = "UserGuide", icon = shiny::icon("book"))
    )
  )
}
