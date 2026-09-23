# Application configuration ------------------------------------------------
# Deployment limits and optimization settings are centralized here.
options(future.globals.maxSize = 1000 * 1024^2)
options(shiny.maxRequestSize = 50 * 1024^2)
options(dplyr.summarise.inform = FALSE)

available_cores <- future::availableCores()
workers <- max(1L, min(3L, available_cores))

if (workers > 1L) {
  future::plan(future::multisession, workers = workers)
} else {
  future::plan(future::sequential)
}

# Interactive development defaults; final manuscript runs need a separate pilot.
app_development_config <- list(sim_days = 50L, num_sims = 10L, simulation_seed = 2026L)
# One exact-threshold search bank; final evaluation uses independent seeds.
# reliability_level = 0.5 with find_n_needed()'s default acceptance_rule =
# "lower_ci" means: require statistical confidence that the TRUE joint
# compliance probability exceeds 50%, not a high observed proportion alone.
bed_search_configs <- list(
  development = list(
    num_sims = 10L, max_evaluations = 50L, final_num_sims = 15L,
    minimum_step = 1L, demand_safety_factor = 1.15,
    reliability_level = 0.5, search_seed = 2026L
  ),
  paper = list(
    num_sims = 40L, max_evaluations = 50L, final_num_sims = 40L,
    minimum_step = 1L, demand_safety_factor = 1.15,
    reliability_level = 0.5, search_seed = 2026L
  )
)