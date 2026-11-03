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
# max_num_sims_*: largest "Number of simulations" accepted by the dashboard, to
# keep run times (and hosted sessions on Connect Cloud) manageable. Civilian
# flow adds a warm-up to every replication, so its limit is lower.
app_development_config <- list(num_sims = 10L,
                               max_num_sims_civilian = 40L, max_num_sims_surge_only = 60L)

# Dashboard inputs: initial value and accepted range, keyed by input ID. A
# numericInput's min/max only bound its spinner, so input_limit_problem()
# (app_server.R) also rejects typed values outside these ranges before any
# calculation starts. `whole` requires a whole number.
# Sizing (compiled engine, UC Davis civilian flow): 100 surge patients/day for
# 50 days costs about 1 s per replication; the result grows with the
# observation horizon (about 6 MB per replication at 50 days, 18 MB at 200), so
# the horizon limit bounds memory. The ranges stay workable if the app falls
# back to simmer, which is about 10 times slower.
app_input_limits <- list(
  n_patients = list(label = "Surge patients per day", value = 15, min = 1, max = 100, whole = FALSE),
  duration = list(label = "Surge arrival period (days)", value = 10, min = 1, max = 60, whole = TRUE),
  sim_days = list(label = "Observation duration (days)", value = 50, min = 1, max = 120, whole = TRUE),
  simulation_seed = list(label = "Simulation seed", value = 2026, min = 1,
                         max = .Machine$integer.max, whole = TRUE),
  congestion_index = list(label = "GenMed wait limit (days)", value = 1, min = 0.01, max = 30, whole = FALSE),
  congestion_index_icu = list(label = "ICU wait limit (days)", value = 1, min = 0.01, max = 30, whole = FALSE),
  genmed_msf = list(label = "HxS GenMed", value = 0, min = 0, max = 1000, whole = TRUE),
  icu_msf = list(label = "HxS ICU", value = 0, min = 0, max = 1000, whole = TRUE)
)

max_num_sims_for <- function(civilian_enabled) {
  if (isTRUE(civilian_enabled)) {
    app_development_config$max_num_sims_civilian
  } else {
    app_development_config$max_num_sims_surge_only
  }
}
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
# - initialization = "analytic": the search starts from the simulation-free
#   unlimited-capacity occupancy (R/analitic/occupancy_analytic.R, which the caller
#   must source). analytic_start_level is the quantile whose peak over time is the
#   starting capacity of each failing unit. The manuscript preset uses 0.75; the app
#   picks the level from app_analytic_start_level below, so the development preset
#   leaves it to find_n_needed()'s default (0.5).
#   "incremental" remains available (simulated unlimited-capacity demand).
# - app_analytic_start_level (app only): starting quantile with routine civilian flow
#   (0.75: starting somewhat above the answer is cheaper than starting below it) and
#   without it (0.5: the median, close to the expected occupancy).
# Keep bed_search_configs as the LAST expression of this file (read in isolation by
# paper/run_manuscript_scenarios.R).
app_analytic_start_level <- c(with_civilian_flow = 0.75, without_civilian_flow = 0.5)
# Simulations (bed search and dashboard runs) use the compiled engine
# (src/hospital_des.cpp via R/core/fast_engine.R) when it compiles; FALSE, or a
# failed compile, uses simmer.
use_fast_engine <- TRUE
bed_search_configs <- list(
  development = list(
    num_sims = 20L, max_evaluations = 50L, final_num_sims = 20L,
    minimum_step = 1L, demand_safety_factor = 1,
    reliability_level = 0.75, refinement_margin = 0.1,
    acceptance_rule = "point_estimate", acceptance_confidence = 0.95,
    weight_GenMed = 1, weight_ICU = 1,
    initialization = "analytic", search_seed = 2026L
  ),
  paper = list(
    num_sims = 40L, max_evaluations = 50L, final_num_sims = 50L,
    minimum_step = 1L, demand_safety_factor = 1,
    reliability_level = 0.75, refinement_margin = 0.1,
    acceptance_rule = "point_estimate", acceptance_confidence = 0.95,
    weight_GenMed = 1, weight_ICU = 1,
    initialization = "analytic", analytic_start_level = 0.75, search_seed = 2026L
  )
)