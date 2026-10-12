# Quick, ad-hoc script: run the hospital DES from an empty state and plot raw
# occupancy over time, to check by eye that it converges to a stable level
# under a manually chosen warm-up length. No automated pass/fail warm-up test
# is used anywhere in this file; the warm-up length is a modeling choice made
# from these plots. Not part of the Shiny app or the calibration pipeline.
# Run via `Rscript paper/plot_raw_dynamics.R` from anywhere (working
# directory is derived from the script's own path below); if sourced
# interactively instead, first setwd() to the project root yourself.
script_args <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_args[grepl("^--file=", script_args)])
if (length(script_file) == 1L) setwd(dirname(dirname(normalizePath(script_file))))

library(simmer)
library(dplyr)
library(ggplot2)

# ---------------------------------------------------------------------------
# Manuscript scenarios: civilian-only convergence check
# ---------------------------------------------------------------------------
# Reuses the paper's own study configuration (paper/run_manuscript_scenarios.R
# -> make_study_config()) instead of hand-picked capacities/profiles, so this
# matches 2026_Assessing_the_hospital_occupancy_under_a_surge_event: the same
# civilian profiles/arrival rates (data/baseline_civilian_profiles.csv) and the
# same capacities used for every volume/concentration/reference scenario in
# manuscript_scenario_design().
#
# Every manuscript scenario shares the same civilian (routine) flow -- only
# the surge differs -- so there is one civilian-only trajectory per *capacity
# regime*, not one per scenario_id. The two fixed capacity inputs the paper's
# pipeline starts from are covered:
#   - "paper": make_study_config()'s capacities (ED is a 999 placeholder).
#   - "unlimited_demand": every inpatient unit at 500 beds and ED at 100000,
#     as in run_unlimited_demand_study().
# (Per-scenario *expanded* capacities from find_n_needed() are search outputs,
# not fixed inputs, so they are not reproduced here.)
#
# The simulation is a plain run_simulation() with baseline = NULL: civilian
# arrivals run from an empty hospital at t = 0, and the plots are what you
# inspect to choose (and justify in the paper) a warm-up length.
source("paper/run_manuscript_scenarios.R")
load_study_functions(".")

paper_study <- make_study_config(project_dir = ".", mode = "paper")

unlimited_capacities <- stats::setNames(
  rep(500L, length(paper_study$config$capacities)),
  names(paper_study$config$capacities)
)
unlimited_capacities[["Surge"]] <- 15L
# accumulates patients, so it gets a far larger value than the inpatient units.
if ("ED" %in% names(unlimited_capacities)) unlimited_capacities[["ED"]] <- 100000L

capacity_regimes <- list(
  paper = paper_study$config$capacities,
  unlimited_demand = unlimited_capacities
)

civilian_profiles <- paper_study$config$baseline$profiles
civilian_rates <- paper_study$config$baseline$arrival_rates  # patients/day per profile
civilian_profile_prob <- civilian_rates / sum(civilian_rates)

civilian_sim_days <- 50  # total days simulated from an empty hospital

# Independent replications per capacity regime, so the plots can show
# between-replication uncertainty. Each replication is a full run_simulation()
# call with its own seed (the same seeds are reused across regimes, so
# replication k sees the same random draws under both capacity settings).
# run_simulation()'s own "replication" column is always 1 for a single call,
# so it is overwritten with the loop index before combining. Cost grows
# linearly with this number; use a small value (e.g. 3) for a quick check.
n_civilian_replications <- 40

civilian_trajectories <- dplyr::bind_rows(lapply(names(capacity_regimes), function(regime_name) {
  capacities <- capacity_regimes[[regime_name]]

  dplyr::bind_rows(lapply(seq_len(n_civilian_replications), function(replication_id) {
    set.seed(2026L * 1000L + replication_id)
    result <- run_simulation(
      capacities = capacities,
      duration = civilian_sim_days,  # civilian arrivals spread over the whole window
      # n_patients is a DAILY RATE (patients/day), not a total: run_simulation()
      # generates n_patients * duration arrivals. "even" requires an integer
      # rate and the combined civilian rate is fractional, so use "poisson".
      n_patients = sum(civilian_rates),
      sim_days = civilian_sim_days,
      patient_profiles = civilian_profiles,
      profile_prob = civilian_profile_prob,
      fallbacks = paper_study$config$fallbacks,
      baseline = NULL,
      arrival_process = "poisson",
      # Only aggregate resource state is read below, never per-patient arrival
      # records (the same reasoning find_n_needed()'s replications use).
      monitor_patients = FALSE,
      # ED's state history is not plotted or used here, and it is ~46% of the
      # resource history, so it is not recorded (the ED panel is therefore
      # absent from the figures below).
      monitor_ed = FALSE
    )

    collect_hospital_resources(result, include_resources = names(capacities)) |>
      dplyr::mutate(scenario_id = regime_name, replication = replication_id)
  }))
}))

# ---------------------------------------------------------------------------
# Figure 1: median + P10-P90 band across replications
# ---------------------------------------------------------------------------
# Same daily-mean + between-replication band already used for the manuscript's
# own figures (make_daily_peak_summary() in R/shared/simulation_metrics.R).
#civilian_daily_summary <- make_daily_peak_summary(civilian_trajectories, var = "server")

# p_manuscript <- ggplot(civilian_daily_summary,
#                        aes(x = time1, y = median_val, color = scenario_id, fill = scenario_id)) +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
#   geom_line() +
#   facet_wrap(~resource, ncol = 1, scales = "free_y") +
#   labs(title = "Manuscript scenarios: civilian-only occupancy from an empty hospital",
#        subtitle = sprintf("%d replications per regime; band = P10-P90 across replications",
#                           n_civilian_replications),
#        x = "Simulation day", y = "Occupied beds (daily mean)",
#        color = "Capacity regime", fill = "Capacity regime") +
#   theme_minimal() + theme(legend.position = "top")
# 
# print(p_manuscript)
# ggsave("paper/manuscript_civilian_dynamics.png", p_manuscript, width = 10, height = 8)

# ---------------------------------------------------------------------------
# Figure 2: every replication's own daily-mean trajectory
# ---------------------------------------------------------------------------
civilian_daily_replications <- daily_peak_by_replication(civilian_trajectories, var = "server")
civilian_daily_replications <- civilian_trajectories |>
  dplyr::filter(scenario_id == "paper") |>
  dplyr::filter(resource != "ED", resource != "Surge")

# Ejemplo: reemplaza los nombres y capacidades por los de tu escenario.
capacity_lines <- data.frame(
  resource = names(capacity_regimes$paper),
  capacity = unname(capacity_regimes$paper),
  Occupied = c(79, 394,8, 999)
  
) |> dplyr::filter(resource != "ED", resource != "Surge")


p_replications <- ggplot2::ggplot(
  civilian_daily_replications,
  ggplot2::aes(x = time, y = server, group = replication)
) +
  ggplot2::geom_line(
    alpha = 0.5,
    linewidth = 0.4,
    color = "gray"
  ) +
  ggplot2::geom_hline(
    data = capacity_lines,
    ggplot2::aes(yintercept = capacity),
    color = "black",
    inherit.aes = FALSE,
    linetype = "dashed",
    linewidth = 0.7
  ) +
  # ggplot2::geom_hline(
  #   data = capacity_lines,
  #   ggplot2::aes(yintercept = Occupied, color="Occupied"),
  #   inherit.aes = FALSE,
  #   linetype = "dashed",
  #   linewidth = 0.7
  # ) +
  ggplot2::facet_wrap(
    ~ resource,
    ncol = 1,
    scales = "free_y",
    strip.position = "top"
  )+
  #ggplot2::facet_grid(resource ~ ., scales = "free_y") +
  ggplot2::labs(
    title = "Civilian-only hospital occupancy",
    subtitle = sprintf(
      "%d replications; each line represents one replication, dashed lines indicate bed capacity",#
      n_civilian_replications
    ),
    x = "Time (days)",
    y = "Occupied beds"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "none",
    panel.grid.minor = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold", size = 11),
    strip.background = ggplot2::element_blank()
  )

print(p_replications)
dir.create("outputs", showWarnings = FALSE)
ggsave("outputs/warm_up.png", p_replications, width = 10, height = 8)

p_replications <- ggplot(civilian_daily_replications,
                         aes(x = time, y = server, group = replication)) +
  geom_line(alpha = 0.5, linewidth = 0.4, color= "gray") +
  facet_grid(resource ~"" , scales = "free_y") +
  labs(title = "Civilian-only occupancy, every replication",
       subtitle = sprintf("%d replications per regime; each line is one replication",
                          n_civilian_replications),
       x = "Time", y = "Occupied beds") +
  theme_minimal() + theme(legend.position = "none")
