# Hospital Surge Capacity Model --------------------------------------------
# Application entry point. Source files are ordered by their dependencies.

source("R/00_packages.R", local = TRUE)
source("R/01_config.R", local = TRUE)

source("R/data/profiles_deloitte.R", local = TRUE)
source("R/helpers/simulation_metrics.R", local = TRUE)
source("R/helpers/report_functions.R", local = TRUE)
source("R/simulation/hospital_trajectory.R", local = TRUE)
source("R/simulation/baseline_flow.R", local = TRUE)
source("R/simulation/run_scenarios.R", local = TRUE)
source("R/modules/mod_profiles.R", local = TRUE)
source("R/modules/mod_baseline.R", local = TRUE)

source("R/ui/ui_sidebar.R", local = TRUE)
source("R/ui/ui_main.R", local = TRUE)
source("R/app_ui.R", local = TRUE)
source("R/app_server.R", local = TRUE)

# Set the default theme for ggplot2 plots
#ggplot2::theme_set(ggplot2::theme_minimal())

# Apply the CSS used by the Shiny app to the ggplot2 plots
#thematic::thematic_shiny()

shiny::shinyApp(
  ui = app_ui(),
  server = app_server
)
