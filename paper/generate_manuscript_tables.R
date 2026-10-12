# Extracts and formats the numbers used in the manuscript's Results tables
# (Section "Results", paper/manuscript/main.tex) from the study outputs in
# outputs/unlimited_demand_01, outputs/volume_concentration_01, and the
# per-scenario optimized_* directories (see optimized_dirs3 below). Prints
# LaTeX-ready table rows and the inline figures cited in the prose, for manual
# copy into main.tex after re-running those studies. Does not write to
# main.tex itself.
#
# Run from the repository root:
#   Rscript paper/generate_manuscript_tables.R
# or, in R/RStudio from the repository root:
#   source("paper/generate_manuscript_tables.R")

unrestricted_dir <- "outputs/unlimited_demand_01"
constrained_dir  <- "outputs/volume_concentration_01"

fmt <- function(x, digits = 2) formatC(x, format = "f", digits = digits)

scenario_order <- c(baseline = "Baseline (no surge)",
                    volume_10 = "Volume: 10/day", reference = "Reference",
                    volume_20 = "Volume: 20/day", concentration_5 = "Concentration: 5 days",
                    concentration_15 = "Concentration: 15 days")

# ---------------------------------------------------------------------------
cat("\n================ Table: unrestricted-capacity peak occupancy ================\n")
d1 <- read.csv(file.path(unrestricted_dir, "resource_summary.csv"))
design1 <- read.csv(file.path(unrestricted_dir, "design.csv"))
peak <- d1[d1$metric == "max_capacity", c("scenario_id", "resource", "mean")]
peak_wide <- reshape(peak, idvar = "scenario_id", timevar = "resource", direction = "wide")
names(peak_wide) <- sub("^mean\\.", "", names(peak_wide))
peak_wide <- merge(peak_wide, rbind(data.frame(scenario_id = "baseline", rate = NA, duration = NA),
                                    design1[c("scenario_id", "rate", "duration")]), by = "scenario_id", all.x = TRUE)
peak_wide <- peak_wide[match(names(scenario_order), peak_wide$scenario_id), ]
for (i in seq_len(nrow(peak_wide))) {
  r <- peak_wide[i, ]
  rate_txt <- if (is.na(r$rate)) "--" else r$rate
  dur_txt <- if (is.na(r$duration)) "--" else r$duration
  cat(sprintf("%s & %s & %s & %s & %s & %s \\\\\n",
    scenario_order[[r$scenario_id]], rate_txt, dur_txt,
    fmt(r$GenMed), fmt(r$ICU), fmt(r$Surge )))
}
cat("Peak queue check (should all be 0):",
    all(d1$mean[d1$metric == "max_queue_length"] == 0), "\n")

# ---------------------------------------------------------------------------
cat("\n================ Table: capacity-constrained queue burden ================\n")
d2 <- read.csv(file.path(constrained_dir, "resource_summary.csv"))
design2 <- read.csv(file.path(constrained_dir, "design.csv"))
row_order <- c("baseline", "volume_10", "reference", "volume_20", "concentration_5", "concentration_15")
for (id in row_order) {
  design_row <- if (id == "baseline") data.frame(rate = NA, duration = NA, total_surge_arrivals = NA)
                else design2[design2$scenario_id == id, ]
  for (unit in c("GenMed", "ICU", "Surge ")) {
    mq <- d2$mean[d2$scenario_id == id & d2$resource == unit & d2$metric == "avg_queue_length"]
    pq <- d2$mean[d2$scenario_id == id & d2$resource == unit & d2$metric == "max_queue_length"]
    pc <- d2$mean[d2$scenario_id == id & d2$resource == unit & d2$metric == "percent_at_capacity"]
    rate_txt <- if (is.na(design_row$rate)) "--" else design_row$rate
    dur_txt <- if (is.na(design_row$duration)) "--" else design_row$duration
    tot_txt <- if (is.na(design_row$total_surge_arrivals)) "--" else design_row$total_surge_arrivals
    cat(sprintf("%s & %s & %s & %s & %s & %s & %s \\\\\n",
      rate_txt, dur_txt, tot_txt, unit, fmt(mq), fmt(pq), fmt(pc)))
  }
}

# ---------------------------------------------------------------------------
cat("\n================ Table: GenMed/ICU delays (hours, 90%/95% CI) ================\n")
# Civilians board (hold ED or a fallback bed) rather than generate a
# bed-less GenMed/ICU wait request in this model, so wait_summary.csv only
# has surge rows for GenMed/ICU (see boarding_time_summary() in
# R/shared/simulation_metrics.R). Civilian delay is boarding time instead;
# both units are shown for both populations.
w <- read.csv(file.path(constrained_dir, "wait_summary.csv"))
w <- w[w$cohort == "Requested during observation", ]
board_rep <- read.csv(file.path(constrained_dir, "boarding_replications.csv"))
label_for <- c(volume_10 = "10 & 10", reference = "15 & 10 (reference)",
              volume_20 = "20 & 10", concentration_5 = "30 & 5", concentration_15 = "10 & 15")
# A population/resource combination with zero rows (e.g. no civilian request
# ever named ICU as its primary unit here) must still print something --
# sprintf() silently returns character(0) when any argument has zero length,
# which otherwise makes cat() print nothing for that row with no warning.
wait_cell <- function(rows) {
  if (!nrow(rows)) return("-- (no requests)")
  sprintf("%s (%s--%s)", fmt(rows$mean_wait_days * 24), fmt(rows$lower * 24), fmt(rows$upper * 24))
}
# boarding_time_summary()'s summary has no confidence interval (unlike
# bed_wait_summary()); build the same 90% central Student-t interval used
# throughout this pipeline (study_mean_interval() in
# paper/run_manuscript_scenarios.R) directly from the per-replication means.
boarding_cell <- function(id, unit) {
  x <- board_rep$mean_boarding_days[board_rep$scenario_id == id & board_rep$resource == unit &
    board_rep$population == "civilian"]
  x <- x[is.finite(x)]
  n <- length(x)
  if (!n) return("-- (no episodes)")
  m <- mean(x) * 24
  if (n < 2) return(sprintf("%s (n=1, no CI)", fmt(m)))
  half_width <- stats::qt(0.95, n - 1) * stats::sd(x) / sqrt(n) * 24
  sprintf("%s (%s--%s)", fmt(m), fmt(m - half_width), fmt(m + half_width))
}
for (unit in c("GenMed", "ICU")) {
  for (id in names(label_for)) {
    sur <- w[w$scenario_id == id & w$resource == unit & w$population == "surge", ]
    cat(sprintf("%s & %s & %s & %s \\\\\n", unit, label_for[[id]], boarding_cell(id, unit), wait_cell(sur)))
  }
}
cat("Pending requests check, wait (should all be 0):", all(w$pending_requests == 0), "\n")
cat("Pending episodes check, boarding (should all be 0):",
    sum(read.csv(file.path(constrained_dir, "boarding_summary.csv"))$pending_episodes), "\n")

# ---------------------------------------------------------------------------
cat("\n================ Table: capacity search outcomes ================\n")
# Each scenario below was optimized independently (its own optimize_study_scenarios()
# call and output_dir), so the four CSVs are read separately per scenario and
# row-bound rather than read once from a single shared optimized_dir.
optimized_dirs3 <- c(reference = "outputs/optimized_reference",
                     volume_20 = "outputs/optimized_volume20",
                     concentration_5 = "outputs/optimized_concentration5")
read_per_scenario3 <- function(filename) {
  do.call(rbind, lapply(names(optimized_dirs3), function(id) {
    path <- file.path(optimized_dirs3[[id]], filename)
    df <- read.csv(path)
    df[df$scenario_id == id, ]
  }))
}
exp3 <- read_per_scenario3("expansion.csv")
joint3 <- read_per_scenario3("search_final_joint_interval.csv")
marg3 <- read_per_scenario3("search_final_intervals.csv")
hist3 <- read_per_scenario3("search_evaluation_history.csv")
label_for3 <- c(reference = "Reference", volume_20 = "Volume: 20/day",
               concentration_5 = "Concentration: 5 days")
for (id in names(label_for3)) {
  e <- exp3[exp3$scenario_id == id, ]
  n_eval <- sum(hist3$scenario_id == id & hist3$stage == "search")
  j <- joint3[joint3$scenario_id == id, ]
  cat(sprintf("%s & %d & %d & %d & Joint & %d/%d & %s & %s--%s \\\\\n",
    label_for3[[id]], e$GenMed_added, e$ICU_added, n_eval,
    j$successes, j$replications, fmt(100 * j$probability, 1),
    fmt(100 * j$lower_95, 1), fmt(100 * j$upper_95, 1)
    ))
  for (unit in c("GenMed", "ICU")) {
    m <- marg3[marg3$scenario_id == id & marg3$resource == unit, ]
    cat(sprintf("%s & %d & %d & %d & %s & %d/%d & %s & %s--%s \\\\\n",
      label_for3[[id]], e$GenMed_added, e$ICU_added, n_eval, unit,
      m$successes, m$replications, fmt(100 * m$probability, 1),
      fmt(100 * m$lower_95, 1), fmt(100 * m$upper_95, 1)))
  }
}

# ---------------------------------------------------------------------------
# Accepted expanded capacity vs. the peak simultaneous occupancy observed for
# the same scenario under unrestricted capacity (the first table above).
# Accepted capacity = the study's starting capacity (saved in each study.rds,
# not hardcoded here) plus the beds the search added (expansion.csv). Reuses
# d1 (unrestricted-capacity resource_summary), exp3 and label_for3 from above.
cat("\n================ Table: accepted capacity vs. unrestricted peak ================\n")
base_capacity_by_scenario <- lapply(optimized_dirs3, function(d) {
  readRDS(file.path(d, "study.rds"))$study$config$capacities
})
stopifnot(length(unique(lapply(base_capacity_by_scenario, function(x) x[c("GenMed", "ICU")]))) == 1L)
base_capacity <- base_capacity_by_scenario[[1]]

cat(
  "\\begin{table}\n",
  "\\tbl{Accepted expanded capacity compared with the peak simultaneous occupancy ",
  "observed for the same scenario under unrestricted capacity ",
  "(Table~\\ref{tab:unrestricted_demand}). A negative difference means the accepted ",
  "capacity was below the unrestricted-capacity peak; the joint criterion permits ",
  "this because brief queueing is tolerated.}\n",
  "{\\begin{tabular}{llcccc} \\toprule\n",
  "Scenario & Unit & Unrestricted peak & Accepted capacity & Difference & Difference (\\%) \\\\ \\midrule\n",
  sep = ""
)
for (id in names(label_for3)) {
  e <- exp3[exp3$scenario_id == id, ]
  stopifnot(nrow(e) == 1L)
  if (!isTRUE(as.logical(e$accepted))) {
    warning("Scenario '", id, "' has no accepted expansion; its row shows a non-accepted candidate.")
  }
  for (unit in c("GenMed", "ICU")) {
    unrestricted_peak <- d1$mean[d1$scenario_id == id & d1$resource == unit & d1$metric == "max_capacity"]
    stopifnot(length(unrestricted_peak) == 1L)
    accepted_capacity <- base_capacity[[unit]] + e[[paste0(unit, "_added")]]
    difference <- accepted_capacity - unrestricted_peak
    cat(sprintf("%s & %s & %s & %d & %s & %s \\\\\n",
      label_for3[[id]], unit, fmt(unrestricted_peak), as.integer(accepted_capacity),
      fmt(difference), fmt(100 * difference / unrestricted_peak)))
  }
}
cat("\\bottomrule\n\\end{tabular}}\n\\label{tab:optim_vs_unrestricted}\n\\end{table}\n")

# ---------------------------------------------------------------------------
# Wait burden before (current capacity) and after the accepted expansion --
# the outcome the search's acceptance criterion is actually evaluated on
# (Section~sec:optim), analogous to the existing queue-burden table but for
# wait time instead of queue length. Surge only: GenMed/ICU bed-less waits
# are, by this model's civilian-always-starts-at-ED convention, effectively
# specific to surge requests (see waiting_state_summary() in
# R/core/hospital_trajectory.R and the manuscript's Section 3.4).
cat("\n================ Table: wait burden before/after expansion ================\n")

wait3 <- read_per_scenario3("wait_summary.csv")

wait3_expanded <- do.call(
  rbind,
  lapply(names(optimized_dirs3), function(id) {
    path <- file.path(optimized_dirs3[[id]], "wait_summary.csv")
    df <- read.csv(path)
    df[df$scenario_id == paste0(id, "_expanded"), ]
  })
)

cat(
  "\\begin{tabular}{llccr}\n",
  "\\toprule\n",
  "& & Current capacity & \\multicolumn{2}{c}{Expanded capacity} \\\\\n",
  "\\cmidrule(lr){3-3}\n",
  "\\cmidrule(lr){4-5}\n",
  "Scenario & Unit ",
  "& \\shortstack{Mean wait, hours\\\\(95\\% CI)} ",
  "& \\shortstack{Mean wait, hours\\\\(95\\% CI)} ",
  "& Added beds \\\\\n",
  "\\midrule\n",
  sep = ""
)

for (id in names(label_for3)) {
  e <- exp3[exp3$scenario_id == id, ]
  
  stopifnot(nrow(e) == 1L)
  
  if (id != names(label_for3)[1]) {
    cat("\\addlinespace\n")
  }
  
  for (unit in c("GenMed", "ICU")) {
    before <- wait3[
      wait3$scenario_id == id &
        wait3$resource == unit &
        wait3$population == "surge" &
        wait3$cohort == "Requested during observation",
    ]
    
    after <- wait3_expanded[
      wait3_expanded$scenario_id == paste0(id, "_expanded") &
        wait3_expanded$resource == unit &
        wait3_expanded$population == "surge" &
        wait3_expanded$cohort == "Requested during observation",
    ]
    
    stopifnot(nrow(before) == 1L, nrow(after) == 1L)
    
    scenario_label <- if (unit == "GenMed") label_for3[[id]] else ""
    added_beds <- e[[paste0(unit, "_added")]]
    
    cat(sprintf(
      "%s & %s & %s & %s & %d \\\\\n",
      scenario_label,
      unit,
      wait_cell(before),
      wait_cell(after),
      added_beds
    ))
  }
}

cat("\\bottomrule\n\\end{tabular}\n")

cat(
  "\nPending requests check (should all be 0):",
  all(wait3$pending_requests == 0),
  all(wait3_expanded$pending_requests == 0),
  "\n"
)

# ---------------------------------------------------------------------------
# Queue burden before (current capacity) and after expansion
cat("\n================ Table: queue burden before/after expansion ================\n")

queue3 <- read_per_scenario3("resource_summary.csv")

queue3_expanded <- do.call(
  rbind,
  lapply(names(optimized_dirs3), function(id) {
    path <- file.path(optimized_dirs3[[id]], "resource_summary.csv")
    df <- read.csv(path)
    df[df$scenario_id == paste0(id, "_expanded"), ]
  })
)

queue_cell <- function(rows, metric_name) {
  if (!nrow(rows)) return("--")
  rows_metric <- rows[rows$metric == metric_name, ]
  if (!nrow(rows_metric)) return("--")
  fmt(rows_metric$mean)
}

cat(
  "\\begin{tabular}{llccccr}\n",
  "\\toprule\n",
  "& & \\multicolumn{2}{c}{Current capacity} & \\multicolumn{2}{c}{Expanded capacity} \\\\\n",
  "\\cmidrule(lr){3-4}\n",
  "\\cmidrule(lr){5-6}\n",
  "Scenario & Unit ",
  "& Mean & Max ",
  "& Mean & Max ",
  "& Added beds \\\\\n",
  "\\midrule\n",
  sep = ""
)

for (id in names(label_for3)) {
  e <- exp3[exp3$scenario_id == id, ]

  stopifnot(nrow(e) == 1L)

  if (id != names(label_for3)[1]) {
    cat("\\addlinespace\n")
  }

  for (unit in c("GenMed", "ICU")) {
    before <- queue3[
      queue3$scenario_id == id &
        queue3$resource == unit,
    ]

    after <- queue3_expanded[
      queue3_expanded$scenario_id == paste0(id, "_expanded") &
        queue3_expanded$resource == unit,
    ]

    scenario_label <- if (unit == "GenMed") label_for3[[id]] else ""
    added_beds <- e[[paste0(unit, "_added")]]

    cat(sprintf(
      "%s & %s & %s & %s & %s & %s & %d \\\\\n",
      scenario_label,
      unit,
      queue_cell(before, "avg_queue_length"),
      queue_cell(before, "max_queue_length"),
      queue_cell(after, "avg_queue_length"),
      queue_cell(after, "max_queue_length"),
      added_beds
    ))
  }
}

cat("\\bottomrule\n\\end{tabular}\n")

# ---------------------------------------------------------------------------
cat("\n================ Inline figures cited in the prose ================\n")
cat("Capacity-search wall-clock time (fill in the manuscript's performance note; ",
    "hardware/OS are not recorded automatically -- add them manually):\n", sep = "")
# expansion.csv has no timing/workers columns; elapsed_seconds is per
# candidate evaluation in search_evaluation_history.csv (sum for a total),
# and workers is a search setting saved in study.rds, not a CSV column.
elapsed_by_scenario <- stats::setNames(
  vapply(names(optimized_dirs3), function(id) sum(hist3$elapsed_seconds[hist3$scenario_id == id]), numeric(1)),
  names(optimized_dirs3))
for (id in names(label_for3)) {
  workers_used <- readRDS(file.path(optimized_dirs3[[id]], "study.rds"))$study$search$workers
  n_eval <- sum(hist3$scenario_id == id & hist3$stage == "search")
  cat(sprintf("  %s: %.1f s (%.2f min) with %s worker(s), %d search evaluations\n",
    label_for3[[id]], elapsed_by_scenario[[id]], elapsed_by_scenario[[id]] / 60, workers_used, n_eval))
}
cat(sprintf("  Total across the three searches: %.1f s (%.2f min)\n",
  sum(elapsed_by_scenario), sum(elapsed_by_scenario) / 60))
