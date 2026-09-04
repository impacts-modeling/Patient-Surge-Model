# Application configuration ------------------------------------------------
# Deployment limits and optimization settings are centralized here.
options(future.globals.maxSize = 1000 * 1024^2)
options(shiny.maxRequestSize = 50 * 1024^2)
options(dplyr.summarise.inform = FALSE)

available_cores <- future::availableCores() - 1L
workers <- max(1L, min(4L, available_cores))

if (workers > 1L) {
  future::plan(future::multisession, workers = workers)
} else {
  future::plan(future::sequential)
}

bed_search_configs <- list(
  fast = list(
    search_num_sims = 7L,
    max_evaluations = 25L,
    max_validation_evaluations = 5L,
    minimum_step = 1L,
    search_queue_tolerance = 1,
    demand_safety_factor = 1.1,
    reliability_level = 0.7,
    search_seed = 2026L
  ),
  precise = list(
    search_num_sims = 10L,
    max_evaluations = 30L,
    max_validation_evaluations = 10L,
    minimum_step = 1L,
    search_queue_tolerance = 0.5,
    demand_safety_factor = 1,
    reliability_level = 0.7,
    search_seed = 2026L
  )
)