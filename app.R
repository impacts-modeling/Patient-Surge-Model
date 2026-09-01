# Hospital Surge Capacity Model --------------------------------------------
# Application entry point. Source files are ordered by their dependencies.

source("R/00_packages.R", local = TRUE)
source("R/01_config.R", local = TRUE)

source("R/data/profiles_deloitte.R", local = TRUE)
source("R/helpers/simulation_metrics.R", local = TRUE)
source("R/helpers/report_functions.R", local = TRUE)
source("R/simulation/hospital_trajectory.R", local = TRUE)
source("R/modules/mod_profiles.R", local = TRUE)

source("R/ui/ui_sidebar.R", local = TRUE)
source("R/ui/ui_main.R", local = TRUE)
source("R/app_ui.R", local = TRUE)
source("R/app_server.R", local = TRUE)

shiny::shinyApp(
  ui = app_ui(),
  server = app_server
)