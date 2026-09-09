# Application configuration ------------------------------------------------
# Deployment limits and optimization settings are centralized here.
options(future.globals.maxSize = 1000 * 1024^2)
options(shiny.maxRequestSize = 50 * 1024^2)
options(dplyr.summarise.inform = FALSE)

available_cores <- future::availableCores()
workers <- max(1L, min(2L, available_cores))

if (workers > 1L) {
  future::plan(future::multisession, workers = workers)
} else {
  future::plan(future::sequential)
}

# Interactive development defaults; final manuscript runs need a separate pilot.
app_development_config <- list(sim_days = 45L, num_sims = 10L, simulation_seed = 2026L)
# One exact-threshold search bank; final evaluation uses independent seeds.
bed_search_configs <- list(
  development = list(
    num_sims = 15L, max_evaluations = 45L, final_num_sims = 20L,
    minimum_step = 1L, demand_safety_factor = 1.1,
    reliability_level = 0.5, search_seed = 2026L
  ),
  paper = list(
    num_sims = 15L, max_evaluations = 40L, final_num_sims = 20L,
    minimum_step = 1L, demand_safety_factor = 1,
    reliability_level = 0.5, search_seed = 2026L
  )
)