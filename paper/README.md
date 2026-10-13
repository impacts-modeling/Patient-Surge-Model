# Manuscript scenarios (no Shiny)

Scripts that reproduce the simulation results reported in the manuscript. They use the same engine as the dashboard (`R/core/`, `R/shared/`, `R/analitic/`) and never load `R/app/` or start Shiny. Run everything from the repository root.

| Script | Purpose |
|---|---|
| `run_manuscript_scenarios.R` | Study definitions and functions: scenario runs, capacity search, figures, CSV export. |
| `run_all_scenarios.R` | Driver that runs the manuscript stages in order (edit paths and settings there). |
| `make_unlimited_comparison.R` | Compares the analytic and simulated unlimited-capacity occupancy and checks the warm-up length. |
| `generate_manuscript_tables.R` | Prints the LaTeX rows of the manuscript tables from the saved outputs. |
| `generate_surge_profile_table.R` | Builds the surge-profile table from `R/shared/profiles_deloitte.R`. |
| `plot_raw_dynamics.R` | Raw-occupancy plots of the civilian flow used to choose the warm-up duration. |
| `Cleaning.Rmd` | Calibration of the civilian and surge profiles from UC Davis data. The source data are not distributed. |

Required packages: simmer, future.apply, dplyr, tidyr, ggplot2 (the scripts do not install packages).

## Quick start

```r
source("paper/run_manuscript_scenarios.R")
load_study_functions()
study <- make_study_config()

main <- run_main_example(study, output_dir = "outputs/main_01")
volume_and_concentration <- run_volume_and_concentration_study(study,
  output_dir = "outputs/volume_concentration_01")
```

Each call runs the civilian baseline and the scenarios of its design and, by default, searches for the bed expansion of every surge scenario. The functions switch the `future` plan to `multisession` with `study$search$workers` processes (sequential when `workers <= 1`) and restore the caller's plan on exit. Studies can be run separately.

## Study configuration

`make_study_config()` defaults:

- **Capacities:** ICU 84, GenMed 405, Surge 15; the ED is a practically unlimited placeholder.
- **Profiles:** civilian profiles from `data/baseline_civilian_profiles.csv` (UC Davis) and the reduced Deloitte surge profiles with their current fallbacks (currently GenMed -> Surge).
- **Arrivals:** evenly spaced surge arrivals and Poisson civilian arrivals. Surge profile assignment and stays remain random.
- **Horizon and replications:** `sim_days = 50`, `num_sims = 40` descriptive replications per scenario, `seed = 2026`.
- **Warm-up:** fixed duration, `baseline_defaults()$warmup_days` (30 days) unless overridden with `warmup = list(warmup_days = ...)`. The manuscript driver sets it explicitly in `run_all_scenarios.R`. No automated stability test is run: choose and justify the duration from raw-occupancy plots (`plot_raw_dynamics.R`) and assess sensitivity to it.
- **Workers:** `workers = 4` is a local setting of this pipeline; it is unrelated to `workers` in `R/01_config.R`, which sizes the deployed app.
- **Boarding credit:** `credit_boarding_time = TRUE` (default) credits boarding time in an inpatient bed toward the next unit's stay (never ED boarding), and the manuscript results use it. `make_study_config(credit_boarding_time = FALSE)` runs the comparison model where waiting never shortens the next stay; use a separate `output_dir` and cache for that run.
Pre-event capacity is identical across comparisons, and expansion beds are activated at day 0 (surge onset). Seeds are shared across scenarios for paired contrasts. The capacity search uses its own seeds (`search_seed = seed + 300000`) and an independent final evaluation. A failed search produces neither an expanded scenario nor a recommendation.

Custom example:

```r
study <- make_study_config(
  sim_days = 120, num_sims = 100, seed = 2026,
  warmup = list(warmup_days = 150),
  search = list(reliability_level = 0.70, final_num_sims = 100)
)
custom <- run_scenario_study(study,
  data.frame(scenario_id = c("short", "long"), rate = c(12, 6), duration = c(10, 20)),
  expand = FALSE, output_dir = "outputs/custom_01")
```

## Scenario designs

- `run_main_example()`: one surge scenario, 10 patients/day for 9 days.
- `run_unlimited_demand_study()`: all scenarios with practically unlimited beds, simulated (DES) and analytic.
- `run_volume_and_concentration_study()`: volume keeps `duration = 10` days and evaluates `rates = c(10, 15, 20)` patients/day; concentration keeps `total = 150` patients and evaluates `durations = c(5, 10, 15)` days (rate = total/duration). Both designs share the rate 15 / duration 10 combination (150 patients), which is simulated once as `scenario_id = "reference"` and serves as an additional comparison for both analyses, besides the civilian-only `baseline`. The combined design has five rows: `volume_10`, `reference`, `volume_20`, `concentration_5`, `concentration_15`. `run_volume_study()` and `run_concentration_study()` are available separately but re-simulate the shared point under two names (`volume_15`, `concentration_10`).
- `optimize_study_scenarios()`: capacity search for selected scenarios of a saved study.
- `combine_optimized_studies()`: merges several optimization output folders into one set of tables and figures.

Ambulatory patients are included in the totals, and every scenario includes continuous civilian operation. Run optimizations one scenario at a time with a modest number of workers to limit memory use, then combine them.

## Capacity search settings

`make_study_config()` reads the search settings from `bed_search_configs` in `R/01_config.R` (through `read_app_search_presets()`), so the dashboard and the manuscript cannot drift apart. Only `search_seed`, `workers` and the wait limits (`wait_time_limit_GenMed`, `wait_time_limit_ICU`, 1 day each) are declared in this pipeline.

```r
study <- make_study_config(mode = "paper")        # bed_search_configs$paper
study <- make_study_config(mode = "development")  # bed_search_configs$development
study <- make_study_config(mode = "paper",
  search = list(num_sims = 20, max_evaluations = 40, final_num_sims = 50, workers = 2))
```

Check `R/01_config.R` for the current values. The criterion is joint: GenMed and ICU mean waits must both be within their limits in the same replication, in at least `reliability_level` of the search replications (observed proportion, `acceptance_rule = "point_estimate"`). The search starts from the simulation-free unlimited-capacity occupancy (`initialization = "analytic"`, starting quantile `analytic_start_level`); the starting capacity is a starting point, not a lower bound or a recommendation, and every candidate is evaluated by simulation. Growth targets `reliability_level + refinement_margin`; the reported pass criterion uses `reliability_level` alone. The selected candidate is evaluated once on an independent holdout bank (`final_num_sims`), and capacities are not tuned with that evaluation. The previous simulation-based start remains available with `search = list(initialization = "incremental")`.

The search does not provide a global optimum or a clinical guarantee. The number of descriptive replications (`num_sims` of `make_study_config()`) does not limit the search replications.

## Outputs

Each study writes to an output directory, which must be empty to avoid overwriting experiments:

- `study.rds`: configuration, tables, figures, R session and paths to raw data and cached searches.
- `resource_summary.csv`: mean, Monte Carlo error and 90% interval (0.05/0.95 quantiles of the t distribution) of each metric across replications. For maximum metrics it summarizes the **mean of per-replication maxima**.
- `resource_replications.csv`: unrounded metrics per replication.
- `daily_replications.csv` and `daily_summary.csv`: daily value per replication (`daily_mean`: the mean of the system states recorded within the day, not weighted by duration), summarized across replications as the mean with a P10-P90 band (`mean_val`, `lower`, `upper`), the same calculation as the dashboard (`make_daily_mean_summary` in `R/shared/simulation_metrics.R`). The band describes dispersion across replications; it is not a confidence interval of a mean. The dashboard and its PDF report use the same daily summary.
- `wait_summary.csv` and `wait_replications.csv`: waits for a first bed by unit, population and cohort, with pending requests reported explicitly. Requests of all patients are included (`completed_only = FALSE`), so requests still waiting at the horizon appear in `pending_requests` instead of being dropped. `mean_observed_wait_days` (with `lower_observed` and `upper_observed`) is the mean over all requests in which a pending request counts the time waited up to the horizon; it is a lower bound of the true mean whenever requests are pending. The full-cohort mean is missing when requests are pending; resolved means can underestimate long waits. `percent_observed_waiting` is the share of requests that waited and `mean_resolved_positive_wait_days` the mean wait among those that did; the cohort mean includes zero waits.
- `boarding_summary.csv` and `boarding_replications.csv`: boarding episodes (time holding a bed while waiting for the next unit), including pending episodes. `percent_boarded` and `mean_positive_boarding_days` give the share of episodes with a positive boarding time and the mean among them; `mean_boarding_days` includes zero-length episodes.
- `paired_comparisons.csv`: surge minus baseline, scenario minus `reference` where applicable, and expansion minus surge, with a 90% interval of the mean. `reference_scenario` and `comparison_scenario` identify the compared pair.
- `expansion.csv`: additional beds, joint compliance and acceptance status. It is not a global optimum or a clinical guarantee.
- `search_final_joint_interval.csv` and `search_final_intervals.csv`: exact binomial intervals of the joint and per-unit compliance in the final evaluation. `search_evaluation_history.csv` and `search_replication_history.csv` hold the search history.
- `design.csv`, `run_metadata.csv`, `cache_usage.csv`: traceability.
- Figures (`trajectories`, `resolved_waits`, `pending_requests` and, when candidates are accepted, `expansion`) as PNG (300 dpi) and PDF. Trajectory lines are the mean across replications of the replication-specific daily means and the band is their P10-P90 range. Wait figures show resolved requests only.

```r
saved <- readRDS("outputs/main_01/study.rds")
saved$figures$trajectories
saved$tables$wait_summary
# Re-export without re-simulating:
save_study_outputs(saved, "outputs/main_reexport_01")
```

Figures do not claim empirical validation. Before presenting them, review stability, pending requests, precision and sensitivity.

## Shared cache and memory

All study functions use `outputs/scenario_cache` by default. The civilian baseline is reused across studies that share configuration and seeds. Cache keys include inputs, horizon, replications, seeds, versions and engine code, but not the `scenario_id`, so identical combinations are reused even when labels differ. A saved study carries the engine signature of the code that produced it: if the engine files change, reloading it is rejected and the study must be re-run.

Raw monitors are stored on disk per scenario; only the scenario being computed and the compact tables are kept in RAM. More replications or a longer horizon raise the memory peak, and the search has its own consumption; no fixed memory limit is claimed. `result$runs` and `result$searches` contain file paths, not the data:

```r
raw <- readRDS(main$runs[["reference"]])
rm(raw); gc()
```

Keep the cache together with the outputs: `study.rds` is not a standalone copy of the raw data, and the stored paths are absolute and must be updated if the cache is moved. The implementation targets sequential runs, not concurrent writers.

## Warm-up and unlimited-capacity checks

`make_unlimited_comparison()` (see `run_all_scenarios.R`) reads the saved unlimited-capacity runs and produces the analytic-versus-DES trajectory figure, the peak comparison table and the first day on which the civilian occupancy reaches 95% and 99% of its equilibrium level. `plot_raw_dynamics.R` produces the raw-occupancy plots per replication used to choose the warm-up duration.
