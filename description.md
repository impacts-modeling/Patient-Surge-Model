# Patient Surge Model Documentation

> This application is a decision-support tool for exploring hospital bed demand
> during surge events. It uses discrete-event simulation (DES) to represent
> patient arrivals, care trajectories, bed occupancy, fallback placement, and
> queues across multiple hospital units.

## Quick start

1. Open **Hospital Setup** and select a patient profile source.
2. Confirm the hospital units, available beds, patient profiles, arrival
   percentages, and fallback rules.
3. Open **Model Parameters** and enter the demand and simulation settings.
4. Click **Run Simulation** to evaluate the current hospital configuration.
5. If needed, enter the maximum allowed Med/Surg and ICU queue lengths, click
   **Estimate Bed Expansion**, and confirm that you want to start the calculation.
6. Review the recommendation and click **Apply Recommended Expansion**.
7. Click **Run Simulation** once more to evaluate the expanded configuration.

> After applying a recommendation, do not run the optimizer a second time unless
> the hospital configuration, demand, simulation settings, or queue limits have
> changed.

## 1. Model purpose

The model represents the movement of patients through a hospital with multiple
bed types. Each arriving patient is assigned to a profile according to the
configured arrival percentages. The profile determines the ordered sequence of
units the patient must visit and the mean length of stay (LOS) at each step.

The application can be used to:

- estimate bed occupancy and utilization;
- identify queues and operational bottlenecks;
- examine patient treatment and waiting times;
- compare baseline and expanded-capacity scenarios; and
- estimate additional GenMed and ICU beds needed to satisfy user-defined queue
  limits with a specified level of reliability.

This is a scenario-analysis model, not a clinical prediction or patient-level
decision tool.

## 2. Hospital Setup

### Patient profile sources

| Option | Intended use |
|---|---|
| **Create profiles manually** | Build a custom hospital configuration and custom patient trajectories. |
| **Use Deloitte profiles (completed)** | Load the complete built-in example configuration. |
| **Use Deloitte profiles (reduced)** | Load the smaller built-in example configuration. |
| **Upload profiles from Excel** | Complete the empty Excel template or restore a configuration previously downloaded from the application. |

Selecting a built-in or uploaded example automatically populates the hospital
units, capacities, profiles, probabilities, and fallback rules associated with
that configuration.

### Hospital units and available beds

Select the units that exist in the scenario and enter the number of beds
available in each selected unit. Supported units are:

- **Surge**
- **GenMed**
- **ICU**
- **BurnBed**
- **Cardiac ICU**
- **PhysicalMed**
- **Psychiatric**
- **TransitionalCare**

Only selected hospital units can be used in patient trajectories or fallback
rules. The bed-expansion optimizer requires both **GenMed** and **ICU**.

### Creating patient trajectories

A patient profile contains:

- a unique profile name;
- one or more hospital units, in the order visited; and
- a mean LOS in days for every unit.

The form begins with **Unit 1**. Use the **+** button when the trajectory needs
another unit. There is no fixed five-unit limit. Saving a profile resets the form
to one unit. If the profile name already exists, the application asks whether
the existing profile should be replaced.

An ambulatory profile has no inpatient trajectory. It receives a short model
delay and does not occupy a hospital bed.

### Arrival percentages

Each profile receives a percentage of total patient arrivals. Percentages must
be non-negative and must sum to **100%** before the configuration can be used.
The application no longer requires a separate WIA proportion: the full patient
mix is represented directly by the profile percentages.

### Fallback rules

Fallbacks define ordered substitute units for a primary unit. When a patient
reaches a trajectory step, the model:

1. checks the primary unit;
2. checks each configured fallback in priority order if the primary unit is
   full; and
3. counts the patient in the queue of the primary unit if no fallback bed is
   available;
4. waits one day; and
5. checks the primary unit and all ordered fallbacks again.

The daily wait-and-recheck cycle continues until a bed becomes available. The
patient remains one logical patient, is counted in the queue of the requested
primary unit, and can occupy no more than one bed.

The fallback list shown in the application always corresponds to the currently
selected profile source. A unit cannot be its own fallback.

### Saving and loading configurations

When **Upload profiles from Excel** is selected, use **Download empty Excel
template** to obtain a blank workbook with the required sheets and column names.
Complete that workbook without renaming the sheets or headers, then upload it
through **Profile configuration (.xlsx)**.

Use **Download surge profile configuration** to save a completed setup as an `.xlsx`
workbook for future simulations. Both files use four required sheets:

| Sheet | Required columns | Purpose |
|---|---|---|
| **Profiles** | `Profile`, `Arrival_percent`, `Ambulatory` | Profile names, patient mix, and ambulatory status. |
| **Trajectories** | `Profile`, `Step`, `Unit`, `LOS_days` | Ordered care steps and mean LOS values. |
| **Fallbacks** | `Primary_unit`, `Priority`, `Fallback_unit` | Ordered substitute-bed rules. |
| **Hospital** | `Unit`, `Available_beds` | Selected units and baseline bed capacity. |

To reuse the file, select **Upload profiles from Excel** and upload the workbook.
Profile names must start with a letter and may contain letters, numbers,
underscores, or hyphens.

## 3. Simulation parameters

### Routine civilian operations

The **Routine Civilian Flow** panel in Hospital Setup optionally adds continuous
civilian demand. Save a separate list of civilian profiles with constant arrival
rates in patients/day, ordered unit IDs separated by commas, and one positive
mean stay per step. These profiles share hospital beds and fallback rules with
surge patients. Fractional rates are supported. Profile lists are retained for
each hospital source and unit selection during the current session.

Before the surge, the model runs civilian arrivals alone. The default warm-up
starts checking after 90 days and can extend to 365 days. It compares time-weighted
mean occupancy and queue lengths across three consecutive 14-day windows. Every
unit must have an occupancy range no greater than 5% of its capacity (minimum
denominator one bed) and a queue range no greater than 0.5 patients. Checks repeat
one window later until the screen passes or the maximum duration is reached.
Settings are editable; they require sensitivity analysis for scientific use.
Passing this screen does not establish statistical equilibrium.

If warm-up fails, the run stops before the surge. Review civilian demand and
capacity or increase the warm-up limit. Overloaded configurations may never
stabilize. If warm-up passes, the same simulation continues: no beds, queues, or
patients are reset, and civilian arrivals continue throughout the event and
follow-up. Day 0 is surge onset; simulation duration excludes warm-up. With this
option disabled, the original empty-hospital behavior is retained, including
the first surge arrivals on day 1.

Bed-expansion searches include the civilian flow and its warm-up. Added beds are
available from warm-up onward, representing planned expansion rather than a
delayed response. Queue criteria are evaluated after surge onset.

### Reproducible outputs for analysis

Use **Simulation seed** to repeat a scenario. **Download raw run data (.rds)**
exports resource events, complete resource history including warm-up, patient
records by population, per-resource patient activity, warm-up diagnostics,
replication metadata, and the configuration. Civilian-enabled runs retain
incomplete patients and civilians discharged before surge onset; select the
appropriate cohort before analysis. Missing end times do not indicate discharge.
Resource plots include both populations, while patient-time plots use completed
surge patients when civilian flow is enabled.

The same runs can be generated outside Shiny with `run_hospital_scenario()`;
see the runnable example and output dictionary in `README.md`. A zero surge
arrival rate permits civilian-only comparisons in R. Existing Excel templates
store hospital and surge settings only; the RDS export includes civilian settings.

### Event settings

| Parameter | Meaning |
|---|---|
| **Patients per Day** | Number of surge patient arrivals generated each day. |
| **Arrival Period (days)** | Number of consecutive days during which new patients arrive. |
| **Simulation Duration (days)** | Observation horizon; excludes warm-up when civilian flow is enabled. |
| **Number of simulations** | Number of independent replications used to summarize stochastic variation. |
| **Med/Surg queue limit** | Maximum acceptable GenMed queue length used by the expansion optimizer. |
| **ICU queue limit** | Maximum acceptable ICU queue length used by the expansion optimizer. |
| **HxS Med/Surg** | Additional GenMed beds added to the baseline capacity. |
| **HxS ICU** | Additional ICU beds added to the baseline capacity. |

The simulation duration should be long enough to observe the consequences of the
full arrival period. Patients may still be in treatment or waiting when the
simulation horizon ends.

## 4. Model mechanics

### Patient arrivals and profile assignment

Patients arrive at the configured daily rate during the arrival period. Each
patient is randomly assigned to a profile using the configured arrival
percentages, then follows the profile's ordered trajectory.

### Length of stay

LOS at each inpatient step follows a log-normal distribution with:

- mean equal to the LOS entered for that trajectory step; and
- coefficient of variation (CV) fixed at **0.20**.

The model converts the arithmetic mean and CV to log-normal parameters:

`sigma = sqrt(log(1 + CV^2))`

`meanlog = log(mean LOS) - sigma^2 / 2`

This preserves the requested mean LOS while allowing realistic right-skewed
variation. Consequently, two patients with the same profile can have different
realized treatment times.

### Beds, queues, and fallback placement

A patient occupies one bed at a time. After completing a trajectory step, the
patient releases the current bed before moving to the next step. Queues have no
fixed capacity and the model does not include patient abandonment.

Fallback placement uses an available substitute bed but retains the LOS assigned
to the original trajectory step.

## 5. Estimating bed expansion

The optimizer estimates additional **GenMed** and **ICU** beds. It evaluates the
current capacity first. If the current capacity satisfies the exact queue limits
at the required reliability, it returns zero additional beds without running the
unlimited-capacity demand scenario.

When expansion is needed, the optimizer runs an internal demand scenario with at
least **500 beds in every configured hospital unit**. If the scenario has more
than 500 total arrivals, that capacity is increased to the number of arrivals so
that the demand run remains unconstrained. Patients therefore use their primary
trajectory units and queues do not determine placement. For every resource, the
optimizer records the largest number of beds occupied simultaneously across the
full validation replication bank. These values are printed in the R console as
`Unlimited-capacity demand`.

Unlimited-capacity demand is a **primary-demand reference**, not a hard safety
ceiling. A constrained upstream unit can route additional patients through a
fallback to GenMed or ICU, which is not observed when every unit has ample beds.
The optimizer therefore starts at current capacity, grows only units that fail
using doubling increments (for example `7, 8, 10, 14, 22, 35`), and permits that
growth up to the total number of arrivals in the scenario. It never jumps directly
to this fallback-safe ceiling. A unit that already passes remains fixed until a
capacity interaction causes it to fail later. The internal demand scenario is not
displayed in the dashboard and is not a bed recommendation.

The recommendation applies the reliability requirement independently to each
target unit:

- the maximum GenMed queue must be at or below the Med/Surg limit in at least
  **70% of validation simulations**; and
- the maximum ICU queue must be at or below the ICU limit in at least **70% of
  validation simulations**.

For example, with 20 validation simulations, GenMed must pass in at least 16 and
ICU must pass in at least 16. They do not have to be the same 16 simulations.
Joint reliability is still reported as a diagnostic, but it does not determine
whether a candidate passes.

All candidate capacities use common seeds and replications, and previously
evaluated combinations are read from a cache. After exponential growth finds a
passing combination, coordinate-wise binary searches reduce ICU and GenMed. A
small ordered neighborhood search then checks nearby trade-offs without testing
every possible combination. The precise search permits up to 30 candidate
evaluations. If that optional minimum-bed refinement reaches its budget after a
validated solution has already been found, the app still returns the solution and
labels it as potentially conservative instead of reporting non-convergence.

The search stage uses a small queue tolerance to locate promising capacities
efficiently. Final validation uses a separate common seed bank and exact queue
limits with no tolerance. If the first validation candidate passes, validation
checks local one-bed reductions. If validation must increase capacity, the last
failing and first passing values become a binary-search interval. No candidate
exceeds the fallback-safe ceiling equal to the total number of arrivals, while
unlimited-capacity demand remains available as a primary-demand reference.

To reduce memory during optimization, each replica returns only the maximum
GenMed and ICU queue and occupancy metrics needed by the search. The full resource
time series is retained only for the user-requested simulation results dashboard.
Because the model is stochastic, the result is a reliability-based recommendation,
not a guarantee that every future simulation will remain below both limits.
The recommendation reports additional beds beyond the currently configured
baseline and HxS values. If any input used by the optimizer changes, the previous
recommendation becomes outdated and must be recalculated.

### Correct workflow

1. Enter the two queue limits.
2. Click **Estimate Bed Expansion**.
3. Confirm that you want to start the calculation.
4. Review the evaluated scenario, each unit's validated reliability, and the
   joint diagnostic.
5. Click **Apply Recommended Expansion**.
6. Confirm that the recommendation was copied to the two HxS fields.
7. Click **Run Simulation** to refresh all plots and tables.

Only one calculation can run at a time. While **Run Simulation** is active, both
calculation buttons are disabled. While **Estimate Bed Expansion** is active,
both buttons are also disabled. The button text identifies the calculation
currently in progress, and the controls are restored when it finishes or stops
with an error.

## 6. Understanding the results

### Average Resource Utilization Over Time

This plot shows occupied beds by hospital unit over time. Within each simulation,
resource observations are grouped by day and summarized by their daily median.
Those daily values are then averaged across simulations.

### Queue Lengths Over Time

This plot uses the same daily aggregation but displays patients waiting for each
unit. A value of zero means no observed queue for that unit on that day.

### Average Utilization of Hospital Resources

| Column | Interpretation |
|---|---|
| **Average Bed Utilization (%)** | Mean occupied capacity across time and simulations. |
| **Peak Bed Utilization (%)** | Highest utilization observed among the simulations. |
| **Average Occupied Beds** | Mean number of occupied beds. |
| **Maximum Occupied Beds** | Highest occupied-bed count observed among the simulations. |
| **Average Time at Full Capacity (days)** | Mean count of recorded time points at which the unit was full, reported as days. |
| **Average Percent of Time at Full Capacity (%)** | Mean percentage of recorded time points at full capacity. |

### Bottlenecks in Hospital Resource Usage

| Column | Interpretation |
|---|---|
| **Average Queue Length (Patients)** | Mean queue length across recorded time points and simulations. |
| **Average Maximum Queue Length (Patients)** | Median of the maximum queue length observed in each simulation. |
| **Average Congestion Index** | Mean fraction of recorded time points with a queue greater than zero. |
| **Average Waiting Time Fraction** | Mean ratio of queued demand to total queued plus occupied demand. |

The waiting-time fraction is a dimensionless congestion measure; it is not the
average number of days a patient waited.

> **Why can the plot and table show different maxima?** The time-series plot
> averages daily summaries across simulations, while the queue table first finds
> a maximum within each simulation and then summarizes those maxima. A peak can
> therefore be visible in the table even when averaging makes the plotted curve
> appear lower.

### Treatment and wait time distributions

The treatment-time histogram shows the distribution of average treatment time
among completed patients in each simulation. The wait-time histogram shows the
distribution of average time spent outside active treatment. The dashed line in
each chart marks the overall mean across the displayed simulations.

## 7. Interpreting stochastic results

Every simulation run contains random profile assignments and random LOS values.
Results can therefore change slightly even when inputs do not change. For more
stable summaries:

- use a sufficient number of simulations;
- compare scenarios using the same model assumptions;
- focus on patterns across metrics rather than one isolated value; and
- rerun important scenarios to assess sensitivity.

An 70% reliability target means that, for each unit separately, up to 30% of
validation simulations may exceed that unit's queue limit. Because failures can
occur in different replications, the joint diagnostic can be below 70% even
when the recommendation passes.

## 8. Assumptions and limitations

- Patient arrivals use a fixed daily rate during the arrival period.
- Profile probabilities remain constant throughout a scenario.
- LOS variability uses a fixed CV of 0.20.
- Queues are unlimited and patients do not leave while waiting.
- Bed capacity is constant during a simulation.
- Staffing, equipment, acuity changes, transfers outside the modeled hospital,
  and clinical prioritization are not modeled separately.
- Patients without an available primary or fallback bed wait one day, then
  recheck the primary unit and every fallback in priority order.
- Outputs depend on the quality and realism of the entered profiles,
  probabilities, capacities, and fallback rules.

## 9. Troubleshooting

**The results disappeared after applying the recommendation.**  
This is expected. Click **Run Simulation** to generate results for the expanded
configuration.

**The recommendation is marked as outdated.**  
One or more inputs changed after optimization. Run **Estimate Bed Expansion**
again.

**The Excel file is rejected.**  
Confirm that all four sheets and their required columns are present, profile
percentages sum to 100%, trajectory and fallback units appear in the Hospital
sheet, and numeric fields contain valid non-negative values.

**Why are Run Simulation and Estimate Bed Expansion disabled?**  
One calculation is already running. Wait for it to finish; the buttons will be
enabled automatically. Only one simulation or bed-expansion calculation can run
at a time.

**A queue still exceeds its limit in some simulations.**  
The optimizer requires each unit to comply in at least 70% of validation
simulations, not 100%. Increase the number of simulations for a more stable
assessment or manually test a larger expansion if a more conservative scenario
is required. The reported joint diagnostic can be lower because GenMed and ICU
may fail in different replications.
