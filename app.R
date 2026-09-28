# Hospital Surge Capacity Model --------------------------------------------
# Application entry point. Source files are ordered by their dependencies.

source("R/00_packages.R", local = TRUE)
source("R/01_config.R", local = TRUE)

source("R/shared/profiles_deloitte.R", local = TRUE)
source("R/shared/simulation_metrics.R", local = TRUE)
source("R/app/report_functions.R", local = TRUE)
source("R/core/hospital_trajectory.R", local = TRUE)
source("R/core/baseline_flow.R", local = TRUE)
source("R/core/run_scenarios.R", local = TRUE)
source("R/app/mod_profiles.R", local = TRUE)
source("R/app/mod_baseline.R", local = TRUE)

source("R/app/ui_sidebar.R", local = TRUE)
source("R/app/ui_main.R", local = TRUE)
source("R/app/app_ui.R", local = TRUE)
source("R/app/app_server.R", local = TRUE)

shiny::shinyApp(
  ui = app_ui(),
  server = app_server
)
