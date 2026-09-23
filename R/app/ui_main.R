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
            min-width: 240px;
            max-width: 280px;
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
        shiny::tabsetPanel(
          shiny::tabPanel(
            "Simulation Results",
            shiny::uiOutput("baseline_run_status"),
            shiny::textOutput("patient_cohort_note"),
            shiny::fluidRow(
              shinydashboard::box(
                title = "Daily Maximum Occupied Beds", status = "success", solidHeader = TRUE, width = 6,
                collapsible = TRUE,
                rintrojs::introBox(
                  plotly::plotlyOutput("resource_plot"),
                  data.step = 11,
                  data.intro = paste(
                    "<strong>Occupancy and queues over time</strong><br>",
                    "Use the interactive plots to compare occupied beds and",
                    "patients waiting across hospital units and days. Each line shows the median",
                    "of daily maxima across replications; the shaded band shows the 10th–90th",
                    "percentiles.",
                    "These are daily peaks, not daily averages."
                  ),
                  data.position = "left"
                )
              ),
              shinydashboard::box(
                title = "Daily Maximum Queue Length", status = "success", solidHeader = TRUE, width = 6,
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
                  data.step = 12,
                  data.intro = paste(
                    "<strong>Interpret the summary tables.</strong><br>",
                    "Review utilization, occupied beds, time at full capacity,",
                    "queue lengths, congestion, and waiting burden by resource."
                  ),
                  data.position = "right"
                ),
                # Add styling to make the table fit within the box
                style = "overflow-x: auto;"
              ),
              shinydashboard::box(
                title = "Bottlenecks in Hospital Resource Usage", status = "success", solidHeader = TRUE, width = 6,
                collapsible = TRUE,
                shiny::tableOutput("queue_analysis"), style = "overflow-x: auto;"
              )
            ),
            shiny::fluidRow(
              shinydashboard::box(
                title = "Boarding Times", status = "success", solidHeader = TRUE, width = 6,
                collapsible = TRUE,
                collapsed = FALSE,
                shiny::tableOutput("boarding_time_table"),
                shiny::helpText(
                  "Time a patient held a bed elsewhere while waiting to reach the listed unit -- ",
                  "either boarding between pathway steps or occupying a fallback while watching ",
                  "to transfer back to it. \"Mean of replication maxima\" averages each replication's ",
                  "longest boarding episode; \"Mean boarding time\" averages every episode. Both are in days."
                ),
                style = "overflow-x: auto;"
              ),
              
              shinydashboard::box(
                title = "Bed Waiting Times", status = "success", solidHeader = TRUE, width = 6,
                collapsible = TRUE,
                collapsed = FALSE,
                rintrojs::introBox(shiny::tableOutput("bed_wait_table"), data.step = 13, data.intro = paste("<strong>Bed waiting times: completed patients only.</strong><br>", "Mean wait includes zero waits and is averaged across replications. The 95% CI describes uncertainty in that mean, not the range containing 95% of patient waits; at least two contributing replications are needed.", "Observed waiting (%) is the percentage of bed requests with a positive wait. Rows distinguish unit, population and cohort. Unfinished patients are excluded, so delays may be understated."), data.position = "top"), shiny::helpText("Only patients who completed their hospital trajectory are included. Means include zero waits and average replication-specific bed-request means; 95% CIs require at least two contributing replications. Results exclude unfinished patients and may understate delays. Day-zero waits include waiting before observation."), shiny::helpText("This table only covers bed-less waits at a pathway's first step (no bed held yet). If civilian profiles start with ED, and ED has enough capacity, civilian rows here will be near zero -- that wait now shows as boarding (see \"Boarding Times\" above) instead, since the patient holds the ED bed while waiting. This table remains meaningful for populations that do not pass through ED first (e.g. surge patients requesting their first bed directly)."), style = "overflow-x: auto;"
              )
            ),

            shiny::fluidRow(
              shinydashboard::box(
                title = "Export Report", status = "primary", solidHeader = TRUE, width = 12,
                rintrojs::introBox(
                  shiny::downloadButton("download_report", "Download PDF Report", class = "btn-primary"),
                  shiny::helpText("Run the simulation first. If you also ran Estimate Bed Expansion, the recommendation will be included."),
                  data.step = 14,
                  data.intro = paste(
                    "<strong>Export the scenario.</strong><br>",
                    "Download raw RDS data for independent figures and analysis, including configuration, seeds and bed-wait summaries. A PDF report is also available. If bed",
                    "expansion was estimated, the recommendation is also included."
                  ),
                  data.position = "top"
                )
              )
            ),
            shiny::fluidRow(
              shiny::uiOutput("N_tex")
            )
          ),
          
          shiny::tabPanel(
            "Documentation",
            rintrojs::introBox(
              shiny::includeMarkdown("description.md"),
              data.step = 15,
              data.intro = paste(
                "<strong>Detailed documentation</strong><br>",
                "Return to this tab for definitions, model assumptions, the",
                "recommended workflow, result interpretation, and troubleshooting."
              ),
              data.position = "top"
            )
          )
        )
      )
    )
  )
}
