# Patient Surge Model

Patient Surge Model is an R Shiny application for exploring how hospital bed capacity responds to a surge in patient arrivals. It uses discrete-event simulation with `simmer` to follow patients through hospital units and estimate bed occupancy, utilization, queues, boarding, and waiting times.

The project supports hospital capacity planning and scenario analysis. Users can define a hospital, configure patient care pathways, simulate demand, and estimate additional medical/surgical and intensive care beds needed to keep the mean wait for a first bed within selected limits. Results depend on the scenario inputs and model assumptions; they are not patient-level clinical predictions.

## Main features

- **Hospital configuration:** select units and enter total beds. The ED is always included with a fixed capacity of 999 beds and is not shown in results.
- **Patient profiles:** define arrival percentages and ordered care pathways with a mean length of stay and SD per unit, or load a predefined NDMS-Based Classification set (UC Davis calibrated profiles, Completed, Regional hospital, Tertiary hospital, Community acute-care hospital).
- **Routine civilian flow:** configure civilian profiles and constant arrival rates, with a warm-up before the surge. Predefined civilian profiles match the selected predefined hospital.
- **Fallback placement:** specify ordered alternative units when the preferred unit has no available bed.
- **Repeated simulations:** explore occupancy, queues, bottlenecks, boarding, and bed waits across replications.
- **Bed-expansion estimates:** search for additional GenMed and ICU beds and simulate the expanded scenario.
- **Import and export:** exchange surge configurations through Excel, civilian profiles through CSV, and download simulation reports as PDF.

Selectable units are Surge, GenMed, ICU, BurnBed, Cardiac ICU, Cardiology, PhysicalMed, Psychiatric, and TransitionalCare (plus the always-present ED). Expansion controls display GenMed as Med/Surg.

## Typical workflow

1. Open **Hospital Setup**. In **Hospital Information**, select units and total beds; in **Create or edit surge patient trajectory**, create profiles manually, use predefined profiles, or upload an Excel workbook.
2. Review pathways, mean stays and SDs, arrival percentages, and fallback rules. Manual percentages must total 100% and be confirmed with **Set arrival percentages**.
3. Optionally enable **Routine Civilian Flow**, save civilian profiles, and review the arrival process and warm-up in **Advanced Flow Settings**. Open **Model Parameters** and set the scenario, surge patients per day, arrival process and period, observation duration, replications, and simulation seed.
4. Click **Run Simulation** and inspect the plots and summary tables.
5. To explore expansion, enter the maximum mean-wait limits (days) for Med/Surg and ICU and click **Estimate Bed Expansion**. Both GenMed and ICU must be configured.
6. Review the recommendation, click **Apply Recommended Expansion**, and then click **Run Simulation** to refresh results for the expanded capacity.
7. Download the PDF report or export the hospital configuration for reuse.

The application includes a guided tour and a **Documentation** tab. See [description.md](description.md) for the detailed user guide.

## How the model works

Each patient is randomly assigned a profile using the configured arrival probabilities. The profile determines the sequence of hospital units visited and the mean length of stay at each step.

- **Arrivals:** surge arrivals are evenly spaced or Poisson at the configured daily rate during the arrival period; profile assignment is random. With civilian flow disabled the hospital starts empty and the first surge arrival is at day 0. With civilian flow enabled, day 0 is surge onset after warm-up, and each civilian profile keeps its own constant rate (evenly spaced or Poisson) throughout warm-up and follow-up.
- **Length of stay:** inpatient stays follow a log-normal distribution with the configured mean and SD per step (`sigma = sqrt(log(1 + SD^2 / mean^2))`). A blank SD defaults to 1 x mean for ICU and 0.24 x mean for other units; built-in NDMS-based sets without per-step variability use CV = 0.1.
- **Bed allocation:** patients request the primary unit and its configured fallbacks at once and take the first free bed, preferring the primary. A patient in a fallback moves to the primary as soon as it frees up.
- **Waiting:** bed acquisition is event-driven; there is no periodic recheck. A patient with no bed waits in the primary unit's queue.
- **Boarding:** after a step, patients keep their current bed until the next unit has a bed, then release it; they never hold two beds.
- **Ambulatory profiles:** patients without an inpatient pathway receive a 0.1-day delay and occupy no hospital bed.
- **Observation period:** runs stop at the selected horizon, so patients may still be waiting or receiving care when observation ends.

The server combines resource and patient monitoring data across replications to produce plots and summary tables. Replications can run in parallel, depending on available cores and the runtime configuration.

## Bed-expansion search

For each replication, the search computes the mean wait for a first bed (days, while holding no bed) in GenMed and in ICU during observation. Because civilian pathways start in the ED, these waits come in practice from surge patients. A replication complies when both means are within their limits.

The search first evaluates current capacity, including additional beds already entered in the HxS fields. If expansion is needed, it runs an unconstrained-demand reference, raises failing units to that reference, grows units that still fail, and refines GenMed/ICU combinations within an evaluation budget.

The active configuration (`bed_search_configs$development` in `R/01_config.R`) requires joint compliance in at least **75%** of 20 replications per candidate, using the observed proportion. Candidates are then checked once on an independent 20-replication bank. This empirical criterion is not a statistical confidence level or a guarantee of real-world performance, and recommendations are not proven global minima. Applying a recommendation updates the HxS fields; running the simulation again refreshes the plots and tables.

When civilian flow is enabled, candidates include both populations. Warm-up uses existing beds; added beds are activated at surge onset. This implementation does not estimate emergency activation delays or automatically separate an existing civilian capacity deficit from surge-related expansion.

## Civilian warm-up

Civilian profiles are independent of surge profiles and use the selected hospital units and shared fallback rules. In **Routine Civilian Flow**, enter a profile name and patients/day, and build the ordered pathway unit by unit with a mean stay and optional SD for each unit, or load or import CSV profiles. Saved civilian profile lists are kept separately by hospital source and unit selection within the current session; rates may be fractional.

By default warm-up uses a fixed duration of 110 days with a stability diagnostic; the adaptive mode checks up to 360 days. The screen compares time-weighted mean occupancy and queue across the last three 14-day windows; the range must be within 10% of bed capacity for occupancy (at least one bed as denominator) and 0.5 patients for queues, for every unit. Fixed warm-up continues even if the screen fails; adaptive warm-up stops the run before the surge if it never passes.

These configurable thresholds are a screening rule, not proof of equilibrium or empirically calibrated defaults. Assess sensitivity to window lengths, tolerances, and warm-up duration before reporting scientific results.

The same simulation environment continues after warm-up. Existing patients, bed occupancy, queues, and scheduled civilian arrivals remain intact. Day 0 denotes surge onset; the selected simulation duration excludes warm-up. Patients still in care at the final horizon remain marked as incomplete.

## Run scenarios outside Shiny

The dashboard and research scripts use `run_hospital_scenario()`. No Shiny session is required for the following example:

```r
source("R/shared/simulation_metrics.R")
source("R/core/hospital_trajectory.R")
source("R/core/baseline_flow.R")
source("R/core/run_scenarios.R")

config <- list(
  capacities = c(GenMed = 30, ICU = 10),
  patient_profiles = list(
    military = list(unit = c("GenMed", "ICU"), los = c(3, 2))
  ),
  profile_prob = c(military = 1),
  fallbacks = list(),
  baseline = baseline_defaults()
)
config$baseline$enabled <- TRUE
config$baseline$profiles <- list(
  routine_medical = list(unit = "GenMed", los = 3)
)
config$baseline$arrival_rates <- c(routine_medical = 2)

runs <- run_hospital_scenario(
  config, duration = 10, n_patients = 5, sim_days = 30,
  num_sims = 12, seed = 2026, scenario_id = "civilian_plus_surge"
)

# Use these tables directly to create publication figures.
occupancy <- runs$resources
patients <- runs$arrivals
saveRDS(runs, "civilian_plus_surge.rds")
```

Set `n_patients = 0` to evaluate civilian operations without a surge. Set `config$baseline$enabled = FALSE` to use the existing empty-hospital model. Parallel execution follows the caller's `future::plan()`; sourcing these simulation files does not start Shiny or change that plan.

| Output | Contents |
|---|---|
| `resources` | Post-onset resource events, with a day-0 state and terminal state when civilian flow is enabled |
| `resource_history` | Complete resource history, with negative times for civilian warm-up |
| `arrivals` | Patient records, profile, population (`civilian` or `surge`), and completion status; civilian-enabled runs also include incomplete patients, warm-up flags, and presence at surge onset |
| `patient_resource_activity` | Per-resource patient records, including logical waiting resources and incomplete activities |
| `warmup_diagnostics` | Per-unit results at every warm-up check |
| `runs` | Replication IDs, realized warm-up duration, observation duration, and master seed |
| `configuration`, `parameters` | Inputs needed to reproduce the scenario |

All result tables carry `replication` and `scenario_id`. Patient names are unique within a replication; use scenario, replication, and name together when joining tables. Incomplete records may contain missing end times or activity durations and must not be treated as completed stays. Patient records include civilians discharged before day 0; select the intended analysis cohort explicitly. Per-resource activity times for logical waiting counters represent waiting, not treatment.


The dashboard does not export these raw tables; use `run_hospital_scenario()` (and `saveRDS()` as above) for independent analysis. Dashboard plots and tables omit the ED.

## Run locally

The package setup file identifies R 4.5.2 as the development version. Install the dependencies from R if needed:

```r
install.packages(c(
  "shiny", "shinydashboard", "markdown", "rintrojs",
  "simmer", "future", "future.apply", "dplyr", "tidyr", "ggplot2", "plotly",
  "readxl", "openxlsx", "gridExtra"
))
```

Open the repository as your working directory and run:

```r
shiny::runApp(".")
```

Run from the repository root because source files and documentation use relative paths. `R/01_config.R` configures up to two workers, with sequential execution when only one core is available. It also contains the bed-search settings.

## Configuration files and reports

Download the empty Excel template from **Hospital Setup**, or export a surge configuration. Excel contains hospital capacities, surge profiles, and fallback rules; civilian profiles are saved separately as CSV from **Routine Civilian Flow**. Imported workbooks must contain these sheets:

| Sheet | Contents |
|---|---|
| `Profiles` | Profile names, arrival percentages, and ambulatory flags |
| `Trajectories` | Ordered unit visits, mean lengths of stay, and optional SD (days); older `CV` columns are accepted |
| `Fallbacks` | Primary units and ordered alternatives |
| `Hospital` | Hospital units and total beds (`Total_beds`; older `Available_beds` accepted). The ED is always set to 999 beds |

PDF reports contain scenario parameters, profile and fallback configurations, simulation summaries, and plots. A current bed-expansion result is included when available.

## Repository structure

```text
Patient-Surge-Model/
|-- app.R                              # Entry point and source loading order
|-- README.md                          # Project overview and setup
|-- description.md                     # In-app user documentation
|-- manifest.json                      # Deployment dependency manifest (regenerate after moving files)
|-- data/
|   |-- baseline_civilian_profiles.csv            # Default civilian profiles (UC Davis units)
|   |-- baseline_civilian_profiles_regional.csv   # Civilian profiles for the Regional hospital
|   |-- baseline_civilian_profiles_tertiary.csv   # Civilian profiles for the Tertiary hospital
|   `-- baseline_civilian_profiles_community.csv  # Civilian profiles for the Community hospital
|-- R/
|   |-- 00_packages.R                  # Package loading
|   |-- 01_config.R                    # Runtime and search settings
|   |-- core/                          # Simulation engine, shared by the app and paper/
|   |   |-- hospital_trajectory.R      # Patient flow and capacity search
|   |   |-- baseline_flow.R            # Civilian arrivals and warm-up functions
|   |   `-- run_scenarios.R            # Standalone reproducible simulation runs
|   |-- shared/                        # Metrics and built-in profiles, shared by the app and paper/
|   |   |-- simulation_metrics.R       # Resource metrics and plots
|   |   `-- profiles_deloitte.R        # Built-in NDMS-based surge configurations
|   `-- app/                           # Shiny-only: UI, server, modules, PDF report
|       |-- app_ui.R                   # Dashboard assembly
|       |-- app_server.R               # Reactive orchestration and outputs
|       |-- ui_sidebar.R               # Navigation and model inputs
|       |-- ui_main.R                  # Results, documentation, and styling
|       |-- mod_profiles.R             # Hospital setup, surge profiles and Excel exchange
|       |-- mod_baseline.R             # Civilian configuration UI and server
|       `-- report_functions.R         # Result tables and PDF report generation
|-- paper/                             # Manuscript scenarios and figures, no Shiny (see paper/README.md)
|   |-- run_manuscript_scenarios.R     # Study orchestration; calls R/core and R/shared only
|   |-- run_all_scenarios.R            # Runs the manuscript scenarios
|   |-- generate_manuscript_tables.R   # Manuscript tables
|   |-- Cleaning.Rmd                   # UC Davis data calibration notebook
|   `-- manuscript/                    # LaTeX source and figures
|-- outputs/                           # Generated study results (figures, tables, caches)
`-- rsconnect/                         # Deployment metadata
```

`app.R` loads the configuration, shared data/metrics, simulation engine, profile modules, and interface before launching Shiny. The profile module returns a reactive hospital configuration to `app_server.R`, which coordinates simulation runs, bed searches, and reporting. `paper/run_manuscript_scenarios.R` sources only `R/core/` and `R/shared/`, never `R/app/`, so the same simulation engine drives both the dashboard and the manuscript scenarios without duplication.

## Interpretation and limitations

This is a simplified bed-capacity model. It does not explicitly model staffing, equipment constraints, mortality, or clinical outcomes. Fallback rules are user-specified assumptions rather than evidence that units are clinically interchangeable. Built-in profiles are example configurations and do not establish validation for a particular hospital; rerouted units in the Regional, Tertiary and Community examples keep their original mean stays.

The expansion criterion is a mean wait, so individual patients can wait longer than the limit. The queue summary's queued patient-time fraction is a dimensionless ratio, not a duration in days. Patients remaining at the simulation horizon also affect the interpretation of patient-level summaries.

The project also supports preparation of a scientific manuscript describing the implemented model. Methods and findings should distinguish scenario inputs, assumptions, stochastic mechanisms, search criteria, and derived outputs, and remain consistent with the code used to produce them.
