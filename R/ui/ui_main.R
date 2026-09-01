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
        hospital_profiles_ui("profiles")
      ),
      shinydashboard::tabItem(
        tabName = "InitCondition",
        shiny::tabsetPanel(
          shiny::tabPanel(
            "Simulation Results",
            shiny::fluidRow(
              shinydashboard::box(
                title = "Average Resource Utilization Over Time", status = "success", solidHeader = TRUE, width = 6,
                collapsible = TRUE,
                rintrojs::introBox(
                  plotly::plotlyOutput("resource_plot"),
                  data.step = 10,
                  data.intro = paste(
                    "<strong>Occupancy and queues over time</strong><br>",
                    "Use the interactive plots to compare occupied beds and",
                    "patients waiting across hospital units and days."
                  ),
                  data.position = "left"
                )
              ),
              shinydashboard::box(
                title = "Queue Lengths Over Time", status = "success", solidHeader = TRUE, width = 6,
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
                  data.step = 11,
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
                title = "Distribution of Average Treatment/Wait Time", status = "success", solidHeader = TRUE, width = 12,
                collapsible = TRUE,
                collapsed = TRUE,
                plotly::plotlyOutput("mean_stay"), style = "overflow-x: auto;"
              )
            ),
            shiny::fluidRow(
              shinydashboard::box(
                title = "Export Report", status = "primary", solidHeader = TRUE, width = 12,
                rintrojs::introBox(
                  shiny::downloadButton("download_report", "Download PDF Report", class = "btn-primary"),
                  shiny::helpText("Run the simulation first. If you also ran Estimate Bed Expansion, the recommendation will be included."),
                  data.step = 12,
                  data.intro = paste(
                    "<strong>Export the scenario.</strong><br>",
                    "Download a PDF report after running the simulation. If bed",
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
              data.step = 13,
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

