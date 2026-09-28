# Patient Surge Model Documentation

> This application is a decision-support tool for exploring hospital bed demand
> during surge events. It uses discrete-event simulation (DES) to represent
> patient arrivals, care trajectories, bed occupancy, fallback placement, and
> queues across multiple hospital units.

## Quick start

1. Open **Hospital Setup** and select a patient profile source.
2. Confirm the hospital units, total beds, patient profiles, arrival
   percentages, and fallback rules.
3. Open **Model Parameters** and enter the demand and simulation settings.
4. Click **Run Simulation** to evaluate the current hospital configuration.
5. If needed, enter the maximum mean-wait limits (days) for Med/Surg and ICU,
   click **Estimate Bed Expansion**, and confirm that you want to start the
   calculation.
6. Review the recommendation and click **Apply Recommended Expansion**.
7. Click **Run Simulation** once more to evaluate the expanded configuration.

> After applying a recommendation, do not run the optimizer a second time unless
> the hospital configuration, demand, simulation settings, or mean-wait limits
> have changed.

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
- estimate additional GenMed and ICU beds needed to keep the mean wait for a
  first bed within user-defined limits with a specified level of reliability.

This is a scenario-analysis model, not a clinical prediction or patient-level
decision tool.

## 2. Hospital Setup

Hospital Setup is organized into four collapsible sections: **Hospital
Information**, **Create or edit surge patient trajectory**, **Routine Civilian
Flow**, and **Advanced Flow Settings** (collapsed by default).

### Patient profile sources

| Option | Intended use |
|---|---|
| **Create profiles manually** | Build a custom hospital configuration and custom patient trajectories. |
| **Use predefined profiles** | Load a built-in NDMS-Based Classification profile set: UC Davis calibrated profiles, Completed, Regional hospital, Tertiary hospital, or Community acute-care hospital. |
| **Upload profiles from Excel** | Complete the empty Excel template or restore a configuration previously downloaded from the application. |

All predefined sets use the NDMS-Based Classification:

| Set | Units and beds |
|---|---|
| **UC Davis calibrated profiles** | Surge, GenMed and ICU (the configuration used in the manuscript). |
| **Completed** | All original units (BurnBed, CardiacICU, GenMed, ICU, PhysicalMed, Psychiatric, TransitionalCare). |
| **Regional hospital** | GenMed 240, ICU 20, Cardiology 12, PhysicalMed 12, TransitionalCare 20; BurnBed pathways rerouted to ICU and Psychiatric to GenMed. |
| **Tertiary hospital** | GenMed 400, ICU 72, BurnBed 12, Cardiology 18, PhysicalMed 24, Psychiatric 20, TransitionalCare 24. |
| **Community acute-care hospital** | GenMed 80, ICU 12, TransitionalCare 12; BurnBed and CardiacICU pathways rerouted to ICU, PhysicalMed to TransitionalCare, and Psychiatric to GenMed. Fallbacks: GenMed and TransitionalCare back each other up; ICU has none. |

Rerouting keeps each step's mean LOS unchanged; it is an illustrative
adaptation of the NDMS proportions to a smaller unit set, not a calibration.

Selecting a built-in or uploaded example automatically populates the hospital
units, capacities, profiles, probabilities, and fallback rules associated with
that configuration.

### Hospital units and total beds

Select the units that exist in the scenario and enter the total number of beds
in each selected unit. The **ED** is always part of the hospital with a fixed
capacity of 999 beds (practically unlimited), so it is not listed or editable;
it remains available to trajectories and fallback rules. Selectable units are:

- **Surge**
- **GenMed**
- **ICU**
- **BurnBed**
- **Cardiac ICU**
- **Cardiology**
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

The form begins with **Unit 1**. Use **Add unit** when the trajectory needs
another unit and **Remove last unit** to drop one. Each unit has a **Mean stay**
and an optional **SD** (days). Saving a profile resets the form to one unit. If
the profile name already exists, the application asks whether the existing
profile should be replaced.

An ambulatory profile has no inpatient trajectory. It receives a short model
delay and does not occupy a hospital bed.

### Arrival percentages

Each profile receives a percentage of total patient arrivals. Percentages must
be non-negative and must sum to **100%** before the configuration can be used.

### Fallback rules

Fallbacks define ordered substitute units for a primary unit. When a patient
reaches a trajectory step, the model:

1. requests a bed from the primary unit and every configured fallback for it,
   in priority order, at the same time;
2. is placed in whichever of those beds is free first -- the primary unit if
   it has one, otherwise the highest-priority free fallback; and
3. is counted in the queue of the primary unit if none of the primary or
   fallback beds are free.

Bed acquisition is **event-driven, not a periodic recheck**: a waiting patient
is placed the instant any requested bed (primary or fallback) becomes free,
not on a fixed daily cycle. If the patient is placed in a fallback, it keeps
watching the primary unit at the same time. Should the primary free up before
this step's length of stay is over, the patient transfers there immediately --
with priority over a fresh request for that same bed -- and continues with only
the remaining stay, so the step's total duration is unchanged by the transfer.
The patient remains one logical patient throughout, is counted in the queue of
the requested primary unit while waiting, and can occupy no more than one bed
at a time. A patient who already holds a bed (boarding between pathway steps,
or occupying a fallback while watching for the primary) is dispatched ahead of
a patient with no bed making a fresh request for that same unit.

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
| **Trajectories** | `Profile`, `Step`, `Unit`, `LOS_days` (`SD` optional) | Ordered care steps, mean LOS, and optional per-step LOS standard deviation in days (defaults to 1 x LOS for ICU steps and 0.24 x LOS for every other unit when omitted). Older workbooks with a `CV` column are still accepted. |
| **Fallbacks** | `Primary_unit`, `Priority`, `Fallback_unit` | Ordered substitute-bed rules. |
| **Hospital** | `Unit`, `Total_beds` | Selected units and baseline bed capacity. The ED is always set to 999 beds. Older workbooks with `Available_beds` are still accepted. |

To reuse the file, select **Upload profiles from Excel** and upload the workbook.
Profile names must start with a letter and may contain letters, numbers,
underscores, or hyphens.

## 3. Simulation parameters

### Routine civilian operations

The **Routine Civilian Flow** panel in Hospital Setup optionally adds continuous
civilian demand. The civilian editor is shown only when **Enable routine
civilian arrivals** is checked. Save a separate list of civilian profiles with
constant arrival rates in patients/day and an ordered pathway built unit by
unit, as in the surge trajectory editor, with one positive mean stay and,
optionally, one standard deviation (SD, days) per unit. Leaving an SD blank
defaults to SD = 1 x mean stay for ICU steps and 0.24 x mean stay for every other
unit. The civilian arrival process and warm-up settings are in **Advanced Flow
Settings**. These profiles share hospital beds and fallback
rules with surge patients. Fractional rates are supported. Profile lists are
retained for each hospital source and unit selection during the current session.

**Use predefined civilian profiles** loads the UC Davis-based civilian file that
matches the selected predefined hospital:
`data/baseline_civilian_profiles_regional.csv`, `_tertiary.csv`, or
`_community.csv`. Every other source uses `data/baseline_civilian_profiles.csv`
(ED, GenMed, Surge and ICU pathways). In the Regional, Tertiary and Community
files every pathway is ED -> unit, and arrival rates are set so that expected
occupancy (rate x 6-day mean stay) is 85% of that unit's beds.

Before the surge, the model runs civilian arrivals alone (warm-up). With the
default settings (**Advanced Flow Settings**), the minimum warm-up is 110 days
and the maximum 360 days. The stability screen compares time-weighted mean
occupancy and queue lengths across three consecutive 14-day windows: every unit
must have an occupancy range no greater than 10% of its capacity (minimum
denominator one bed) and a queue range no greater than 0.5 patients.

- **Fixed duration with diagnostics** (default): warm-up lasts the minimum
  duration; the screen is recorded as a diagnostic and the run continues even
  if it fails.
- **Adaptive stability screen**: checks repeat one window later until the
  screen passes or the maximum duration is reached; if it never passes, the
  run stops before the surge.

Settings are editable and require sensitivity analysis for scientific use.
Passing the screen does not establish statistical equilibrium, and overloaded
configurations may never stabilize. After warm-up the same simulation
continues: no beds, queues, or patients are reset, and civilian arrivals
continue throughout the event and follow-up. Day 0 is surge onset; simulation
duration excludes warm-up. With civilian flow disabled, the hospital starts
empty and the first surge arrival occurs at day 0.

Warm-up uses the existing beds. Additional (HxS) beds, including candidates
evaluated by the bed-expansion search, are activated at surge onset. The
search's wait criterion is evaluated only during the observation period.

### Reproducible runs

Use **Simulation seed** to repeat a scenario. The **Download PDF Report**
button exports the configuration, seed, result tables and plots. The same runs,
with all raw tables (resource history including warm-up, patient records by
population, per-resource activity, warm-up diagnostics and replication
metadata), can be generated outside Shiny with `run_hospital_scenario()`; see
the runnable example and output dictionary in `README.md`. Excel workbooks store
hospital and surge settings only; save civilian profiles with **Download saved
profiles** in Routine Civilian Flow.

### Event settings

| Parameter | Meaning |
|---|---|
| **Scenario to run** | **Surge event** (surge arrivals, plus civilian flow if enabled) or **Routine civilian operation only**. |
| **Surge Patients per Day** | Surge arrival rate. With Poisson arrivals it is the mean rate and the realized count varies. |
| **Surge Arrival Period (days)** | Number of consecutive days during which surge patients arrive. |
| **Surge arrival process** | **Evenly spaced** or **Poisson (random arrivals)**. |
| **Observation Duration (days)** | Observation horizon; excludes warm-up when civilian flow is enabled. |
| **Number of simulations** | Number of independent replications used to summarize stochastic variation. |
| **Simulation seed** | Master seed that makes a run reproducible. |
| **Med/Surg (days)**, **ICU (days)** | Maximum mean wait for a first bed used by the expansion optimizer. |
| **HxS Med/Surg** | Additional GenMed beds added to the baseline capacity. |
| **HxS ICU** | Additional ICU beds added to the baseline capacity. |

The simulation duration should be long enough to observe the consequences of the
full arrival period. Patients may still be in treatment or waiting when the
simulation horizon ends.

## 4. Model mechanics

### Patient arrivals and profile assignment

Surge patients arrive at the configured daily rate during the arrival period,
either evenly spaced or as a Poisson process (random exponential interarrival
times with the configured mean rate). Each patient is randomly assigned to a
profile using the configured arrival percentages, then follows the profile's
ordered trajectory. Civilian profiles use their own rates and the civilian
arrival process chosen in **Advanced Flow Settings**.

### Length of stay

LOS at each inpatient step follows a log-normal distribution with:

- mean equal to the LOS entered for that trajectory step; and
- standard deviation (SD, days) entered for that step. A blank SD defaults to
  **1 x LOS for ICU steps and 0.24 x LOS for every other unit** (CV 1 and 0.24).

The entered SD is stored as the coefficient of variation `CV = SD / mean LOS`,
which identifies exactly the same log-normal distribution. The model converts
the arithmetic mean and CV to log-normal parameters:

`sigma = sqrt(log(1 + CV^2)) = sqrt(log(1 + SD^2 / mean LOS^2))`

`meanlog = log(mean LOS) - sigma^2 / 2`

Built-in surge profiles that carry no per-step variability (the Completed,
Regional hospital, Tertiary hospital, and Community acute-care hospital
NDMS-based sets) use the engine
default **CV = 0.1** at every step, not the 1/0.24 defaults above; the
trajectory editor and Excel download show the corresponding SD (0.1 x LOS).

This preserves the requested mean LOS while allowing right-skewed
variation. Consequently, two patients with the same profile can have different
realized treatment times.

### Beds, queues, and fallback placement

After completing a trajectory step, a patient keeps the current bed until a bed
for the next step is obtained (**boarding**) and then releases it, so a patient
never holds more than one bed. Time spent holding a bed while waiting for
another unit -- between steps, or in a fallback while watching for the primary
unit -- is reported under **Boarding Times**. A wait with no bed held at all
(only possible at a pathway's first step) is reported under **Bed Waiting
Times**. Queues have no fixed capacity and the model does not include patient
abandonment.

Fallback placement uses an available substitute bed but retains the LOS assigned
to the original trajectory step.

## 5. Estimating bed expansion

The optimizer estimates additional **GenMed** and **ICU** beds. Its acceptance
metric is, for each replication and each of GenMed and ICU, the **mean wait for
a first bed** during the observation period: the average duration of episodes in
which a patient requested a bed in that unit while holding no bed at all. Because
civilian pathways start in the ED (a patient waiting in the ED holds an ED bed),
these waits come in practice from surge patients whose pathway starts in GenMed
or ICU. Boarding time is reported but is not part of the criterion.

The optimizer evaluates the current capacity first. If the current capacity
satisfies both limits at the required reliability, it returns zero additional
beds without running the unlimited-capacity demand scenario.

When expansion is needed, the optimizer runs an internal demand scenario with at
least **500 beds in every configured hospital unit**. If the scenario has more
than 500 total arrivals, that capacity is increased to the number of arrivals so
that the demand run remains unconstrained. Patients therefore use their primary
trajectory units and queues do not determine placement. For every resource, the
optimizer records the largest number of beds occupied simultaneously across the
replications of that demand scenario (a separate seed bank). These values are printed in the R console as
`Unlimited-capacity demand`.

Unlimited-capacity demand is a **primary-demand reference**, not a hard safety
ceiling. A constrained upstream unit can route additional patients through a
fallback to GenMed or ICU, which is not observed when every unit has ample beds.
With the app's `incremental` initialization, only the units that fail at current
capacity are first raised to their unlimited-capacity peak (never lowered); units
that still fail then grow using doubling increments (for example
`7, 8, 10, 14, 22, 35`), up to a fallback-safe ceiling equal to the total number
of arrivals in the scenario. A unit that already passes remains fixed until a
capacity interaction causes it to fail later. The internal demand scenario is not
displayed in the dashboard and is not a bed recommendation.

The recommendation applies the reliability requirement **jointly** to both
target units: a replication only counts as compliant when the GenMed mean wait
and the ICU mean wait are *both* at or below their limits in that same
replication. With the dashboard's current search settings
(`bed_search_configs$development` in `R/01_config.R`), a candidate must clear
this joint check in at least **75%** of **20** replications per candidate (15
of 20) to pass. Each unit's own compliance rate is still reported next to the
joint result, but it is a diagnostic only: a unit can clear 100% of
replications on its own while the candidate still fails overall, because its
failures and the other unit's failures do not have to land on the same
replications for the joint check. The acceptance check itself is a plain
observed proportion (not a statistical confidence bound), so it can be sensitive
to sampling noise when the number of replications is small.

All candidate capacities use common seeds and replications, and previously
evaluated combinations are read from a cache. Growth first targets a looser
margin above the reliability target (so growth does not stop right at the
boundary); once a candidate clears the stricter joint criterion above,
coordinate-wise binary searches reduce ICU and GenMed, and a trade-off search
then checks whether shifting beds between GenMed and ICU lowers the total
weighted bed count further, without testing every possible combination. The
search stops after at most **50 candidate evaluations** (`max_evaluations`). If
that optional minimum-bed refinement reaches its budget after a validated
solution has already been found, the app still returns the solution and labels
it as potentially conservative instead of reporting non-convergence.

Once a candidate clears the joint criterion during the search, it is evaluated
exactly once more on an independent replication bank that was never used to
tune the capacity (**20** replications by default, `final_num_sims`). This
holdout result -- not the search-stage estimate -- is what the dashboard
reports and what "Apply Recommended Expansion" copies into the HxS fields.
Because the holdout bank is independent, a candidate that looked strong while
search was actively growing or shrinking beds can still fail holdout; when
that happens, the dashboard reports that the search did not find a passing
capacity even though some intermediate search evaluations looked promising. No
candidate exceeds the fallback-safe ceiling equal to the total number of
arrivals, while unlimited-capacity demand remains available as a
primary-demand reference.

To reduce memory during optimization, each replica returns only the GenMed and
ICU mean-wait and maximum-occupancy metrics needed by the search. The full resource
time series is retained only for the user-requested simulation results dashboard.
Because the model is stochastic, the result is a reliability-based recommendation,
not a guarantee that every future simulation will remain below both limits.
The recommendation reports additional beds beyond the currently configured
baseline and HxS values. If any input used by the optimizer changes, the previous
recommendation becomes outdated and must be recalculated.

### Correct workflow

1. Enter the two mean-wait limits (days).
2. Click **Estimate Bed Expansion**.
3. Confirm that you want to start the calculation.
4. Review the evaluated scenario, the joint reliability that determines
   pass/fail, and each unit's own compliance rate as a diagnostic.
5. Click **Apply Recommended Expansion**.
6. Confirm that the recommendation was copied to the two HxS fields.
7. Click **Run Simulation** to refresh all plots and tables.

Only one calculation can run at a time. While **Run Simulation** is active, both
calculation buttons are disabled. While **Estimate Bed Expansion** is active,
both buttons are also disabled. The button text identifies the calculation
currently in progress, and the controls are restored when it finishes or stops
with an error.

## 6. Understanding the results

Result plots and tables leave out the ED, whose capacity is fixed at 999 beds.

### Daily Mean Occupied Beds

This plot shows occupied beds by hospital unit over time. Within each simulation,
the time-weighted mean occupancy of each day is computed; the line is the mean
of those daily means across simulations, and the shaded band spans their 10th
to 90th percentiles.

### Daily Mean Queue Length

This plot uses the same daily aggregation but displays patients waiting for each
unit. A value of zero means no queue for that unit on that day.

### Average Utilization of Hospital Resources

| Column | Interpretation |
|---|---|
| **Average Bed Utilization (%)** | Time-weighted mean occupied share of capacity, averaged across simulations. |
| **Peak Bed Utilization (%)** | Highest utilization observed in any simulation. |
| **Average Occupied Beds** | Time-weighted mean number of occupied beds, averaged across simulations. |
| **Maximum Occupied Beds** | Highest occupied-bed count observed in any simulation. |
| **Time at Full Capacity (days)** | Mean time (days) during which every bed was occupied. |
| **Percent of Time at Full Capacity (%)** | Mean percentage of the observation period at full capacity. |

### Bottlenecks in Hospital Resource Usage

| Column | Interpretation |
|---|---|
| **Average Queue Length (Patients)** | Time-weighted mean queue length, averaged across simulations. |
| **Mean Maximum Queue Length (Patients)** | Mean, across replications, of each replication's maximum queue length. |
| **Fraction of Time with a Queue** | Mean share of time with at least one patient waiting. |
| **Queued Patient-Time Fraction** | Queued patient-time divided by queued plus occupied patient-time. |

The queued patient-time fraction is a dimensionless congestion measure; it is not
the average number of days a patient waited.

> **Why can the plot and table show different maxima?** The time-series plot
> averages daily means across simulations, while the queue table first finds a
> maximum within each simulation and then summarizes those maxima. A peak can
> therefore be visible in the table even when averaging makes the plotted curve
> appear lower.

### Boarding Times and Bed Waiting Times

**Boarding Times** reports the time patients held a bed elsewhere while waiting
for the listed unit (mean of each replication's longest episode, and mean
episode duration). **Bed Waiting Times** reports waits with no bed held,
including zero waits, for every request that obtained a bed during observation;
the 95% CI describes Monte Carlo uncertainty in the mean.

## 7. Interpreting stochastic results

Every simulation run contains random profile assignments and random LOS values.
Results can therefore change slightly even when inputs do not change. For more
stable summaries:

- use a sufficient number of simulations;
- compare scenarios using the same model assumptions;
- focus on patterns across metrics rather than one isolated value; and
- rerun important scenarios to assess sensitivity.

A 75% reliability target means that, in at least 75% of validation replications,
the GenMed and ICU mean waits must *both* stay within their limits in that same
replication. A single unit's own compliance rate can look higher or lower than
75% in isolation -- it is diagnostic only. What determines whether a candidate
passes is the joint rate across both units together.

## 8. Assumptions and limitations

- Patient arrivals use a constant daily rate (evenly spaced or Poisson) during
  the arrival period.
- The bed-expansion criterion is the mean wait for a first bed, not the longest
  wait or boarding time; individual patients can wait longer than the limit.
- Profile probabilities remain constant throughout a scenario.
- LOS variability is entered as a per-step SD; a blank SD defaults to a CV of 1
  for ICU steps and 0.24 for every other unit. Built-in NDMS-based profile sets
  without per-step variability use CV = 0.1.
- Queues are unlimited and patients do not leave while waiting.
- Bed capacity is constant during a simulation.
- Staffing, equipment, acuity changes, transfers outside the modeled hospital,
  and clinical prioritization are not modeled separately.
- Patients without an available primary or fallback bed wait, event-driven,
  until any of the requested beds (primary or fallback) frees; there is no
  periodic recheck delay.
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

**A mean wait still exceeds its limit in some simulations.**  
The optimizer requires GenMed and ICU to comply jointly in at least 75% of
validation simulations, not 100%. Increase the number of simulations for a more
stable assessment or manually test a larger expansion if a more conservative
scenario is required.
