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
app_development_config <- list(sim_days = 50L, num_sims = 10L, simulation_seed = 2026L)
# One exact-threshold search bank; final evaluation uses independent seeds.
# Every field here is forwarded as-is to find_n_needed() (see its definition
# in R/core/hospital_trajectory.R for the full explanation of each argument).
# Listing them explicitly -- instead of relying on find_n_needed()'s own
# defaults -- lets both the app and the manuscript pipeline tune/test the
# search from this single file without touching engine code.
# - acceptance_rule = "lower_ci": require statistical confidence (at
#   acceptance_confidence) that the TRUE joint compliance probability exceeds
#   reliability_level, not just a high observed proportion that could be a
#   lucky draw with few replications. "point_estimate" accepts whenever the
#   observed joint proportion alone meets reliability_level -- simpler, but
#   more sensitive to sampling noise when num_sims is small.
# - reliability_level: joint pass threshold (both GenMed and ICU within their
#   wait-time limits in the SAME replication).
# - refinement_margin: growth targets reliability_level + refinement_margin
#   (capped at 1) so growth does not stop right at the pass/fail boundary;
#   the reported/holdout pass criterion always uses reliability_level alone.
# - weight_GenMed / weight_ICU: relative cost per added bed used only to rank
#   GenMed<->ICU trade-offs during refinement (higher weight = costlier to add).
bed_search_configs <- list(
  development = list(
    num_sims = 20L, max_evaluations = 50L, final_num_sims = 20L,
    minimum_step = 1L, demand_safety_factor = 1,
    reliability_level = 0.75, refinement_margin = 0.1,
    acceptance_rule = "point_estimate", acceptance_confidence = 0.95,
    weight_GenMed = 1, weight_ICU = 1,
    initialization = "incremental", search_seed = 2026L
  ),
  paper = list(
    num_sims = 40L, max_evaluations = 50L, final_num_sims = 50L,
    minimum_step = 1L, demand_safety_factor = 1,
    reliability_level = 0.75, refinement_margin = 0.1,
    acceptance_rule = "point_estimate", acceptance_confidence = 0.95,
    weight_GenMed = 1, weight_ICU = 1,
    initialization = "incremental", search_seed = 2026L
  )
)