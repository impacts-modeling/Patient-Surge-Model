# Main UI ---------------------------------------------------------------
build_header <- function() {
  shinydashboard::dashboardHeader(title = "Patient Surge Model")
}

build_body <- function() {
  body <- shinydashboard::dashboardBody(
    rintrojs::introjsUI(),
    shiny::tags$head(
      shiny::tags$style(
        shiny::HTML(
          "
          .guided-tour-launch {
            padding: 10px 14px 6px 10px;
          }
          .guided-tour-launch > div {
            display: block;
            width: 100%;
          }
          .guided-tour-launch .btn {
            box-sizing: border-box;
            display: block;
            max-width: 100%;
            width: calc(100% - 6px) !important;
          }
          .introjs-tooltip {
            min-width: 300px;
            max-width: 400px;
          }
          .introjs-tooltiptext {
            font-size: 14px;
            line-height: 1.5;
          }
          .introjs-progressbar {
            background-color: #3c8dbc;
          }
          .introjs-button {
            font-size: 12px;
          }
          .main-sidebar .form-group.shiny-input-container {
            margin-bottom: 8px;
          }
          .main-sidebar .control-label {
            margin-bottom: 4px;
          }
          .main-sidebar h3,
          .main-sidebar h4 {
            margin-top: 10px;
            margin-bottom: 6px;
          }
          .main-sidebar hr {
            margin-top: 10px;
            margin-bottom: 10px;
          }
          .main-sidebar details.sidebar-section {
            border-top: 1px solid rgba(255, 255, 255, 0.25);
            padding: 0 10px;
          }
          .main-sidebar details.sidebar-section > summary {
            cursor: pointer;
            font-size: 18px;
            font-weight: 600;
            line-height: 1.3;
            list-style: none;
            padding: 10px 28px 10px 0;
            position: relative;
          }
          .main-sidebar details.sidebar-section > summary::-webkit-details-marker {
            display: none;
          }
          .main-sidebar details.sidebar-section[open] > summary {
            margin-bottom: 6px;
          }
          .main-sidebar details.sidebar-section > summary::after {
            content: '\\25BE';
            position: absolute;
            right: 4px;
            top: 50%;
            transform: translateY(-50%);
            transition: transform 0.2s ease;
          }
          .main-sidebar details.sidebar-section:not([open]) > summary::after {
            transform: translateY(-50%) rotate(-90deg);
          }
          /* AdminLTE sets white-space: nowrap inside the sidebar menu; let help text wrap. */
          .main-sidebar .help-block {
            white-space: normal;
          }
          .main-sidebar .sidebar-info-icon {
            color: #8aa4af;
            font-size: 14px;
            font-weight: normal;
            margin-left: 5px;
          }
          .main-sidebar .sidebar-info-icon:hover,
          .main-sidebar .sidebar-info-icon:focus,
          .main-sidebar .sidebar-info-icon[aria-expanded='true'] {
            color: #ffffff;
            text-decoration: none;
          }
          /* Content of the selected option (e.g. hospital unit checkboxes),
             set apart from the option buttons above it. */
          .option-detail-panel {
            background-color: #f4f6f9;
            border-left: 3px solid #00c0ef;
            border-radius: 3px;
            margin-top: 4px;
            padding: 8px 10px 2px;
          }
          .option-detail-panel .form-group {
            margin-bottom: 6px;
          }
          /* Keep the selected value of narrow sidebar selects on one line. */
          .main-sidebar .selectize-input > .item {
            max-width: calc(100% - 16px);
            overflow: hidden;
            text-overflow: ellipsis;
            white-space: nowrap;
          }
          /* Start below floated input columns; otherwise the help text's negative
             top margin pulls its first line beside the left column. */
          .main-sidebar .sidebar-info-text {
            clear: both;
          }
          .main-sidebar .sidebar-info-text .help-block {
            margin: -4px 0 8px;
          }
          "
        )
      ),
      shiny::tags$script(shiny::HTML(
        "Shiny.addCustomMessageHandler('calculation-state', function(message) {
           var simulationRunning = message.simulation_running === true;
           var bedSearchRunning = message.bed_search_running === true;
           var simulationButton = $('#run_sim');
           var bedSearchButton = $('#run_N');
  
           simulationButton.prop('disabled', simulationRunning || bedSearchRunning);
           bedSearchButton.prop('disabled', simulationRunning || bedSearchRunning);
  
           simulationButton.text(
             simulationRunning ? 'Simulation is running...' :
             bedSearchRunning ? 'Bed expansion calculation is running...' :
             'Run Simulation'
           );
           bedSearchButton.text(
             bedSearchRunning ? 'Bed expansion calculation is running...' :
             simulationRunning ? 'Simulation is running...' :
             'Estimate Bed Expansion'
           );
         });"
      ))
    ),
  shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = "HospitalSetup",
        hospital_profiles_ui("profiles"),
        mod_baseline_ui("baseline")
      ),
      shinydashboard::tabItem(
        tabName = "InitCondition",
          shiny::tagList(
            shiny::conditionalPanel(unlimited_js,
              mod_unlimited_ui("unlimited")),
            shiny::conditionalPanel(sprintf("!%s", unlimited_js),
            shiny::uiOutput("baseline_run_status"),
            shiny::textOutput("patient_cohort_note"),
            shiny::fluidRow(
              shinydashboard::box(
                title = "Daily Mean Occupied Beds", status = "success", solidHeader = TRUE, width = 6,
                collapsible = TRUE,
                rintrojs::introBox(
                  plotly::plotlyOutput("resource_plot"),
                  data.step = 14,
                  data.intro = paste(
                    "<strong>Occupancy and queues over time</strong><br>",
                    "Use the interactive plots to compare occupied beds and",
                    "patients waiting across hospital units and days. Each line shows the mean",
                    "of the replication-specific daily means across replications; the shaded band shows the",
                    "10th–90th percentiles. The ED (fixed at 10,000 beds) is not shown.",
                    "With unlimited capacity, this page shows the analytic expected occupancy instead."
                  ),
                  data.position = "left"
                )
              ),
              shinydashboard::box(
                title = "Daily Mean Queue Length", status = "success", solidHeader = TRUE, width = 6,
                collapsible = TRUE,
                plotly::plotlyOutput("queue_plot")
              )
            ),
            shiny::fluidRow(
              shinydashboard::box(
                title = "Average Utilization of Hospital Resources", status = "success", solidHeader = TRUE, width = 6,
                collapsible = TRUE,
                rintrojs::introBox(
                  shiny::tableOutput("utilization_table"),
                  data.step = 15,
                  data.intro = paste(
                    "<strong>Average Utilization of Hospital Resources.</strong><br>",
                    "One row per unit: average and peak bed utilization (%), average and maximum",
                    "occupied beds, and how long (days and % of the observation period) every bed",
                    "was occupied. Values are averaged over the simulations."
                  ),
                  data.position = "right"
                ),
                style = "overflow-x: auto;"
              ),
              shinydashboard::box(
                title = "Bottlenecks in Hospital Resource Usage", status = "success", solidHeader = TRUE, width = 6,
                collapsible = TRUE,
                rintrojs::introBox(
                  shiny::tableOutput("queue_analysis"),
                  data.step = 16,
                  data.intro = paste(
                    "<strong>Bottlenecks in Hospital Resource Usage.</strong><br>",
                    "Queues by unit: average queue length, mean of each simulation's longest queue,",
                    "share of time with at least one patient waiting, and the queued share of all",
                    "patient-time (a congestion ratio, not a waiting time in days)."
                  ),
                  data.position = "left"
                ),
                style = "overflow-x: auto;"
              )
            ),
            shiny::fluidRow(
              shinydashboard::box(
                title = "Boarding Times", status = "success", solidHeader = TRUE, width = 6,
                collapsible = TRUE,
                collapsed = FALSE,
                rintrojs::introBox(
                  shiny::tableOutput("boarding_time_table"),
                  shiny::helpText(
                    "Time a patient held a bed (from the previous pathway step) while waiting to reach ",
                    "the listed unit. \"Mean of replication maxima\" averages each replication's ",
                    "longest boarding episode; \"Mean boarding time\" averages every episode. Both are in days."
                  ),
                  data.step = 17,
                  data.intro = paste(
                    "<strong>Boarding Times.</strong><br>",
                    "How long patients kept a bed (for example in the ED) while waiting for the next",
                    "unit of their pathway, by unit and population: the mean of each simulation's",
                    "longest episode, the mean episode, the share of patients who boarded and their",
                    "mean boarding time, the number of episodes, and those still waiting at the end",
                    "of the observation period."
                  ),
                  data.position = "right"
                ),
                style = "overflow-x: auto;"
              ),
              
              shinydashboard::box(
                title = "Bed Waiting Times", status = "success", solidHeader = TRUE, width = 6,
                collapsible = TRUE,
                collapsed = FALSE,
                rintrojs::introBox(
                  shiny::tableOutput("bed_wait_table"),
                  data.step = 18,
                  data.intro = paste(
                    "<strong>Bed waiting times.</strong><br>",
                    "Waits for a first bed (no bed held), for every request that found a bed. The mean includes",
                    "zero waits; the 95% CI is the uncertainty of that mean, not the spread of individual waits.",
                    "Observed waiting (%) is the share of requests that waited at all."
                  ),
                  data.position = "top"
                ),
                shiny::helpText("Includes every bed request that itself found a bed (resolved), whether or not the patient had been discharged by the end of the simulation. Means include zero waits and average replication-specific bed-request means; 95% CIs require at least two replications with a resolved request. Day-zero waits include waiting before observation."),
                shiny::helpText("This table only covers bed-less waits at a pathway's first step (no bed held yet); the ED is not shown. Patients whose pathway starts in the ED hold an ED bed while waiting for their next unit, so that wait appears under \"Boarding Times\" instead. This table is most informative for patients who do not pass through the ED first (e.g. surge patients requesting their first bed directly)."),
                style = "overflow-x: auto;"
              )
            )),

            shiny::fluidRow(
              shinydashboard::box(
                title = "Export Report", status = "primary", solidHeader = TRUE, width = 12,
                rintrojs::introBox(
                  shiny::downloadButton("download_report", "Download PDF Report", class = "btn-primary"),
                  shiny::helpText("Run the simulation first. If you also ran Estimate Bed Expansion, the recommendation will be included."),
                  data.step = 19,
                  data.intro = paste(
                    "<strong>Export the scenario.</strong><br>",
                    "Download a PDF report with the configuration, seed, result tables and plots. If bed",
                    "expansion was estimated, the recommendation is also included."
                  ),
                  data.position = "top"
                )
              )
            ),
            shiny::fluidRow(
              shiny::uiOutput("N_tex")
            )
          )
      ),
      # User guide: its own sidebar page (below Model Parameters) so it is easy to find.
      shinydashboard::tabItem(
        tabName = "UserGuide",
        shiny::fluidRow(
          shinydashboard::box(
            width = 12, status = "primary",
            # The tour highlights this short note, not the whole (very tall) guide.
            rintrojs::introBox(
              shiny::div(class = "alert alert-info",
                         "New here? Start with the Quick start below, or use Start Guided Tour in the sidebar."),
              id = "tour_user_guide",
              data.step = 20,
              data.intro = paste(
                "<strong>User Guide</strong><br>",
                "Open User Guide in the sidebar at any time for the quick start,",
                "definitions, model assumptions, result interpretation, and troubleshooting."
              ),
              data.position = "bottom"
            ),
            shiny::includeMarkdown("description.md")
          )
        )
      )
    )
  )
}
