# Patient Surge Model

Patient Surge Model is an R Shiny application for exploring how hospital bed capacity responds to a surge in patient arrivals. It uses discrete-event simulation with `simmer` to follow patients through hospital units and estimate bed occupancy, utilization, queues, and treatment and waiting times.

The project supports hospital capacity planning and scenario analysis. Users can define a hospital, configure patient care pathways, simulate demand, and estimate additional medical/surgical and intensive care beds needed to meet selected queue limits. Results depend on the scenario inputs and model assumptions; they are not patient-level clinical predictions.

## Main features

- **Hospital configuration:** select units and enter available beds.
- **Patient profiles:** define arrival percentages, ordered care pathways, and mean lengths of stay.
- **Routine civilian flow:** configure independent civilian profiles and constant arrival rates, with optional warm-up before the surge.
- **Fallback placement:** specify ordered alternative units when the preferred unit has no available bed.
- **Repeated simulations:** explore occupancy, queues, bottlenecks, and patient times across replications.
- **Bed-expansion estimates:** search for additional GenMed and ICU beds and simulate the expanded scenario.
- **Import and export:** exchange hospital configurations through Excel and download simulation reports as PDF.

Supported units are Surge, GenMed, ICU, BurnBed, Cardiac ICU, PhysicalMed, Psychiatric, and TransitionalCare. Expansion controls display GenMed as Med/Surg.

## Typical workflow

1. Open **Hospital Setup** and choose manual profiles, a built-in Deloitte example, or an Excel upload.
2. Review units, bed capacities, patient pathways, mean lengths of stay, arrival percentages, and fallback rules. Manual percentages must total 100% and be confirmed with **Set arrival percentages**.
3. Optionally enable **Routine Civilian Flow** in Hospital Setup, save civilian profiles, and review the warm-up settings. Open **Model Parameters** and set surge patients per day, the arrival period, the observation duration, replications, and simulation seed.
4. Click **Run Simulation** and inspect the plots and summary tables.
5. To explore expansion, enter the maximum allowed Med/Surg and ICU queue lengths and click **Estimate Bed Expansion**. Both GenMed and ICU must be configured.
6. Review the recommendation, click **Apply Recommended Expansion**, and then click **Run Simulation** to refresh results for the expanded capacity.
7. Download the PDF report or export the hospital configuration for reuse.

The application includes a guided tour and a **Documentation** tab. See [description.md](description.md) for the detailed user guide.

## How the model works

Each patient is randomly assigned a profile using the configured arrival probabilities. The profile determines the sequence of hospital units visited and the mean length of stay at each step.

- **Arrivals:** surge counts and spacing are deterministic; surge profile assignment is random. With civilian flow disabled, the existing empty-hospital model starts arrivals on day 1. With civilian flow enabled, surge arrivals start at event day 0 after warm-up. Each civilian profile has its own constant, evenly spaced arrival rate throughout warm-up and follow-up.
- **Length of stay:** inpatient stays follow a lognormal distribution with the configured mean and a default coefficient of variation of 0.2.
- **Bed allocation:** patients use the primary unit if a bed is available, otherwise the first available configured fallback.
- **Waiting:** when no candidate bed is available, patients enter a logical queue for the primary unit and recheck availability at a default interval of one day.
- **Transfers:** patients release the bed from the completed step before seeking the next bed. They do not retain the previous bed while waiting for transfer.
- **Ambulatory profiles:** patients without an inpatient pathway receive a 0.1-day delay and occupy no hospital bed.
- **Observation period:** runs stop at the selected simulation horizon, so patients may still be waiting or receiving care when observation ends.

The server combines resource and patient monitoring data across replications to produce plots and summary tables. Replications can run in parallel, depending on available cores and the runtime configuration.

## Bed-expansion search

The search first evaluates current capacity, including additional beds already entered in the HxS fields. If expansion is needed, it uses an unconstrained-demand reference, increases beds in units that fail the queue criteria, and refines GenMed/ICU capacity combinations within an evaluation budget.

The active application configuration uses a **70% reliability target for each unit separately**. A candidate must keep its maximum queue within the selected limit in at least that fraction of validation replications for GenMed and for ICU. Joint reliability is also reported as a diagnostic. This empirical simulation criterion is not a statistical confidence level or a guarantee of real-world performance.

Candidates are checked with a separate validation replication bank. Recommendations depend on the implemented search and validation budgets and are not proven global minima. Applying a recommendation updates the HxS fields; running the simulation again refreshes the plots and tables.

When civilian flow is enabled, search candidates include both populations and evaluate queues after surge onset. Additional beds are available during warm-up, representing planned expansion. Warm-up failure stops the estimate rather than returning a recommendation from an uninitialized scenario. This implementation does not estimate emergency activation delays or automatically separate an existing civilian capacity deficit from surge-related expansion.

## Civilian warm-up

Civilian profiles are independent of surge profiles and use the selected hospital units and shared fallback rules. In **Routine Civilian Flow**, enter a profile name, patients/day, ordered unit IDs (for example `GenMed, ICU`), and corresponding mean stays (for example `3, 2`). Save each profile. Saved civilian profile lists are kept separately by hospital source and unit selection within the current session; rates may be fractional.

The default minimum warm-up is 90 days and the maximum is 365 days. At each check, the model compares the time-weighted mean occupancy and queue in the last three 14-day windows for each unit. The range of these window means must be within 5% of bed capacity for occupancy (using at least one bed as the denominator) and 0.5 patients for queues. All units must pass. The warm-up extends by one window after a failed check, with a final check at the maximum.

These configurable thresholds are a screening rule, not proof of equilibrium or empirically calibrated defaults. Assess sensitivity to window lengths, tolerances, and warm-up duration before reporting scientific results. If the screen does not pass, the run stops before the surge; review demand and capacity or increase the warm-up limit. A permanently overloaded hospital may not stabilize.

The same simulation environment continues after warm-up. Existing patients, bed occupancy, queues, and scheduled civilian arrivals remain intact. Day 0 denotes surge onset; the selected simulation duration excludes warm-up. Patients still in care at the final horizon remain marked as incomplete.

## Run scenarios outside Shiny

The dashboard and research scripts use `run_hospital_scenario()`. No Shiny session is required for the following example:

```r
source("R/helpers/simulation_metrics.R")
source("R/simulation/hospital_trajectory.R")
source("R/simulation/baseline_flow.R")
source("R/simulation/run_scenarios.R")

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

**Download raw run data (.rds)** in the dashboard exports the same result structure with report metadata. Resource plots include both populations; when civilian flow is enabled, patient-time plots use completed surge patients. Research figures can use the raw tables without reproducing dashboard summaries.

## Run locally

The package setup file identifies R 4.5.2 as the development version. Install the dependencies from R if needed:

```r
install.packages(c(
  "shiny", "shinydashboard", "markdown", "thematic", "rintrojs",
  "simmer", "future", "future.apply", "dplyr", "ggplot2", "plotly",
  "readxl", "openxlsx", "gridExtra"
))
```

Open the repository as your working directory and run:

```r
shiny::runApp(".")
```

Run from the repository root because source files and documentation use relative paths. `R/01_config.R` configures up to four workers, with sequential execution when only one worker is available. It also contains the bed-search settings.

## Configuration files and reports

Download the empty Excel template from **Hospital Setup**, or export a surge configuration. Excel contains hospital capacities, surge profiles, and fallback rules; it does not include civilian settings. The raw RDS download preserves the full configuration, which can be reused in R. Imported workbooks must contain these sheets:

| Sheet | Contents |
|---|---|
| `Profiles` | Profile names, arrival percentages, and ambulatory flags |
| `Trajectories` | Ordered unit visits and mean lengths of stay |
| `Fallbacks` | Primary units and ordered alternatives |
| `Hospital` | Hospital units and available beds |

PDF reports contain scenario parameters, profile and fallback configurations, simulation summaries, and plots. A current bed-expansion result is included when available.

## Repository structure

```text
Patient-Surge-Model/
|-- app.R                              # Entry point and source loading order
|-- README.md                          # Project overview and setup
|-- description.md                     # In-app user documentation
|-- manifest.json                      # Deployment dependency manifest
|-- R/
|   |-- 00_packages.R                  # Package loading
|   |-- 01_config.R                    # Runtime and search settings
|   |-- app_ui.R                       # Dashboard assembly
|   |-- app_server.R                   # Reactive orchestration and outputs
|   |-- data/
|   |   `-- profiles_deloitte.R        # Built-in patient configurations
|   |-- modules/
|   |   |-- mod_profiles.R             # Hospital setup and surge Excel exchange
|   |   `-- mod_baseline.R             # Civilian configuration UI and server
|   |-- simulation/
|   |   |-- hospital_trajectory.R      # Patient flow and capacity search
|   |   |-- baseline_flow.R            # Civilian arrivals and warm-up functions
|   |   `-- run_scenarios.R            # Standalone reproducible simulation runs
|   |-- helpers/
|   |   |-- simulation_metrics.R       # Resource metrics and plots
|   |   `-- report_functions.R         # PDF report generation
|   `-- ui/
|       |-- ui_sidebar.R               # Navigation and model inputs
|       `-- ui_main.R                  # Results, documentation, and styling
`-- rsconnect/                         # Deployment metadata
```

`app.R` loads the configuration, data, helpers, simulation functions, profile module, and interface before launching Shiny. The profile module returns a reactive hospital configuration to `app_server.R`, which coordinates simulation runs, bed searches, and reporting.

## Interpretation and limitations

This is a simplified bed-capacity model. It does not explicitly model staffing, equipment constraints, mortality, or clinical outcomes. Fallback rules are user-specified assumptions rather than evidence that units are clinically interchangeable. Built-in profiles are example configurations and do not establish validation for a particular hospital.

Queue behavior depends on the discrete recheck interval, and transfer waiting does not represent boarding while retaining an upstream bed. Several displayed averages are calculated over recorded resource states rather than weighted by elapsed time. The queue summary's waiting-time fraction is a state-based ratio, not a duration in days. Patients remaining at the simulation horizon also affect the interpretation of patient-time summaries.

The project also supports preparation of a scientific manuscript describing the implemented model. Methods and findings should distinguish scenario inputs, assumptions, stochastic mechanisms, search criteria, and derived outputs, and remain consistent with the code used to produce them.
