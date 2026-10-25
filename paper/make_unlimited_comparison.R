# Figure and tables contrasting the analytic (simulation-free) and the simulated
# unlimited-capacity occupancy, plus a check of the civilian warm-up length.
#
# Input: the study saved by run_unlimited_demand_study(study, output_dir = ...)
# (study.rds and the raw runs it references). No simulation is run here.
# Run from the project root:
#   source("paper/make_unlimited_comparison.R")
#   comparison <- make_unlimited_comparison("outputs/unlimited_demand_01",
#                                           output_dir = "outputs/unlimited_comparison_01")
#
# Outputs (list, also written to output_dir as CSV/PNG/PDF):
#   trajectory   analytic and DES occupancy per scenario, unit and time (days from surge onset)
#   peak_table   per scenario and unit: DES mean (sd) of replication peaks vs analytic peaks
#   fit          agreement of the mean trajectory (RMSE, maximum error, peak error)
#   warmup       baseline (no surge) occupancy as a fraction of its equilibrium at checkpoint days
#   warmup_reached  first day each unit reaches 95% / 99% of equilibrium (analytic)
#   figures      trajectories (all surge scenarios) and warmup
#
# Notes for the manuscript:
#  * The DES peak is the mean over replications of the maximum occupancy over the whole run
#    (what the capacity search used to start from); the analytic peaks are the peak over
#    time of the pointwise mean and quantiles, so they are different quantities.
#  * The warm-up check is for unlimited capacity. With finite beds, queues and boarding
#    change the dynamics (see paper/plot_raw_dynamics.R for the capacity-constrained runs).
#  * Equilibrium is the exact Little's-law value; the analytic curve stays slightly below it
#    because stays are discretised on a 0.05-day grid.

make_unlimited_comparison <- function(unlimited_dir = "outputs/unlimited_demand_01",
                                      project_dir = ".", units = c("GenMed", "ICU"),
                                      checkpoint_days = c(10, 20, 30, 40, 50, 60),
                                      output_dir = NULL) {
  for (pkg in c("dplyr", "tidyr", "ggplot2")) {
    if (!requireNamespace(pkg, quietly = TRUE)) stop("Install required package: ", pkg)
  }
  if (!exists("analytic_unlimited_demand", mode = "function")) {
    sys.source(file.path(project_dir, "R/analitic/occupancy_analytic.R"), envir = environment())
  }
  if (!exists("run_unlimited_analytic_study", mode = "function")) {
    source(file.path(project_dir, "paper/run_manuscript_scenarios.R"), local = TRUE)
  }

  resolve <- function(path) if (grepl("^([A-Za-z]:|/|~)", path)) path else file.path(project_dir, path)
  unlimited <- readRDS(file.path(resolve(unlimited_dir), "study.rds"))
  config <- unlimited$study$config
  design <- unlimited$tables$design
  warmup <- config$baseline$warmup_days
  scenarios <- c("baseline", design$scenario_id)
  analytic <- unlimited$analytic
  if (is.null(analytic)) {
    analytic <- run_unlimited_analytic_study(unlimited$study, dplyr::bind_rows(
      data.frame(scenario_id = "baseline", rate = 0, duration = 0),
      design[c("scenario_id", "rate", "duration")]), units = units)
  }
  analytic$trajectory <- dplyr::filter(analytic$trajectory, .data$resource %in% units)
  analytic$peaks <- dplyr::filter(analytic$peaks, .data$resource %in% units)
  times <- sort(unique(analytic$trajectory$time))

  # The study stores absolute paths to the raw runs; if the outputs were moved, look for the
  # same file in a scenario_cache folder next to the study directory.
  raw_path <- function(id) {
    path <- unlimited$runs[[id]]
    if (file.exists(path)) return(path)
    moved <- file.path(dirname(resolve(unlimited_dir)), "scenario_cache", basename(path))
    if (file.exists(moved)) return(moved)
    stop("Raw run for '", id, "' not found at ", path, " or ", moved)
  }
  # Simulated occupancy on the analytic time grid, one raw run at a time (~45 MB each).
  des <- lapply(scenarios, function(id) {
    history <- readRDS(raw_path(id))$resource_history
    history <- history[history$resource %in% units, c("resource", "time", "server", "replication")]
    on_grid <- history |>
      dplyr::group_by(.data$resource, .data$replication) |>
      dplyr::group_modify(function(rows, key) {
        rows <- rows[order(rows$time), ]
        data.frame(time = times, occupied = rows$server[pmax(findInterval(times, rows$time), 1L)])
      }) |>
      dplyr::ungroup()
    trajectory <- on_grid |>
      dplyr::group_by(.data$resource, .data$time) |>
      dplyr::summarise(des_mean = mean(.data$occupied), des_sd = stats::sd(.data$occupied),
        des_q05 = stats::quantile(.data$occupied, 0.05, names = FALSE),
        des_q95 = stats::quantile(.data$occupied, 0.95, names = FALSE),
        replications = dplyr::n_distinct(.data$replication), .groups = "drop")
    peaks <- history |>
      dplyr::group_by(.data$resource, .data$replication) |>
      dplyr::summarise(peak = max(.data$server), .groups = "drop") |>
      dplyr::group_by(.data$resource) |>
      dplyr::summarise(des_mean_of_peaks = mean(.data$peak), des_sd_of_peaks = stats::sd(.data$peak),
                       des_max_of_peaks = max(.data$peak), .groups = "drop")
    rm(history, on_grid)
    invisible(gc())
    list(trajectory = dplyr::mutate(trajectory, scenario_id = id),
         peaks = dplyr::mutate(peaks, scenario_id = id))
  })
  des_trajectory <- dplyr::bind_rows(lapply(des, `[[`, "trajectory"))
  des_peaks <- dplyr::bind_rows(lapply(des, `[[`, "peaks"))

  labels <- stats::setNames(
    c("baseline (no surge)", sprintf("%s: %g/day x %g d", design$scenario_id, design$rate, design$duration)),
    scenarios)
  trajectory <- analytic$trajectory |>
    dplyr::rename_with(~ paste0("analytic_", .x), -c("resource", "time", "scenario_id")) |>
    dplyr::inner_join(des_trajectory, by = c("scenario_id", "resource", "time")) |>
    dplyr::mutate(scenario_label = factor(labels[.data$scenario_id], levels = labels))

  peak_table <- analytic$peaks |>
    dplyr::filter(.data$statistic %in% c("peak_mean", "peak_q0.5", "peak_q0.75", "peak_q0.95")) |>
    tidyr::pivot_wider(id_cols = c("scenario_id", "resource"), names_from = "statistic",
                       values_from = "peak_value", names_prefix = "analytic_") |>
    dplyr::inner_join(des_peaks, by = c("scenario_id", "resource")) |>
    dplyr::filter(.data$scenario_id != "baseline") |>
    dplyr::arrange(factor(.data$scenario_id, levels = scenarios), .data$resource)

  fit <- trajectory |>
    dplyr::group_by(.data$scenario_id, .data$resource) |>
    dplyr::summarise(
      rmse_mean = sqrt(mean((.data$analytic_mean - .data$des_mean)^2)),
      rmse_mean_observation = sqrt(mean((.data$analytic_mean - .data$des_mean)[.data$time >= 0]^2)),
      max_abs_error_mean = max(abs(.data$analytic_mean - .data$des_mean)),
      peak_mean_error = max(.data$analytic_mean) - max(.data$des_mean),
      share_des_mean_within_2se = mean(abs(.data$analytic_mean - .data$des_mean) <=
        2 * .data$analytic_sd / sqrt(.data$replications)),
      .groups = "drop")

  # Warm-up: civilian-only baseline against the exact equilibrium occupancy.
  equilibrium <- analytic_equilibrium_occupancy(config$baseline, units)
  baseline_curve <- trajectory |>
    dplyr::filter(.data$scenario_id == "baseline") |>
    dplyr::mutate(day = .data$time + warmup, equilibrium = unname(equilibrium[.data$resource]),
                  analytic_fraction = .data$analytic_mean / .data$equilibrium,
                  des_fraction = .data$des_mean / .data$equilibrium)
  warmup_table <- baseline_curve |>
    dplyr::filter(.data$day %in% checkpoint_days) |>
    dplyr::transmute(.data$resource, .data$day, .data$equilibrium,
                     analytic_expected = .data$analytic_mean, .data$analytic_fraction,
                     des_expected = .data$des_mean, .data$des_fraction) |>
    dplyr::arrange(.data$resource, .data$day)
  warmup_reached <- baseline_curve |>
    dplyr::group_by(.data$resource) |>
    dplyr::summarise(
      warmup_days = warmup,
      first_day_95 = suppressWarnings(min(.data$day[.data$analytic_fraction >= 0.975])),
      first_day_99 = suppressWarnings(min(.data$day[.data$analytic_fraction >= 0.99])),
      analytic_fraction_at_warmup_end = .data$analytic_fraction[which.min(abs(.data$day - warmup))],
      des_fraction_at_warmup_end = .data$des_fraction[which.min(abs(.data$day - warmup))],
      .groups = "drop")

  figures <- list(
    trajectories = ggplot2::ggplot(dplyr::filter(trajectory, .data$scenario_id != "baseline"),
        ggplot2::aes(x = .data$time)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$analytic_q0.05, ymax = .data$analytic_q0.95,
                                        fill = "Analytic 5-95%"), alpha = 0.25) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$des_q05, ymax = .data$des_q95, fill = "DES 5-95%"),
                           alpha = 0.25) +
      ggplot2::geom_line(ggplot2::aes(y = .data$analytic_mean, color = "Analytic mean")) +
      ggplot2::geom_line(ggplot2::aes(y = .data$des_mean, color = "DES mean"), linetype = "dashed") +
      ggplot2::geom_vline(xintercept = 0, linetype = "dotted") +
      ggplot2::facet_grid(rows = ggplot2::vars(.data$resource), cols = ggplot2::vars(.data$scenario_label),
                          scales = "free_y",
                          labeller = ggplot2::labeller(resource = figure_unit_label)) +
      ggplot2::labs(x = "Days from surge onset (negative = civilian warm-up)", y = "Occupied beds",
        color = NULL, fill = NULL#,
        #caption = "Unlimited capacity. DES band: empirical 5-95% across replications; analytic band: 5-95% of the exact distribution."
        ) +
      ggplot2::theme_bw() + ggplot2::theme(
        text = ggplot2::element_text(size = 16),
        legend.position = "top"),
    warmup = ggplot2::ggplot(baseline_curve, ggplot2::aes(x = .data$day)) +
      ggplot2::geom_line(ggplot2::aes(y = .data$analytic_fraction, color = "Analytic")) +
      ggplot2::geom_line(ggplot2::aes(y = .data$des_fraction, color = "DES"), linetype = "dashed") +
      ggplot2::geom_hline(yintercept = c(0.95, 0.99), linetype = "dotted") +
      ggplot2::geom_vline(xintercept = warmup, linetype = "dotdash") +
      ggplot2::facet_wrap(ggplot2::vars(.data$resource),
                          labeller = ggplot2::labeller(resource = figure_unit_label)) +
      ggplot2::labs(x = "Days since the start of the civilian warm-up",
        y = "Expected occupancy / equilibrium occupancy", color = NULL,
        caption = sprintf("Civilian flow only, unlimited capacity. Dotted: 95%% and 99%%; dash-dot: warm-up length (%g days).", warmup)) +
      ggplot2::theme_bw() + ggplot2::theme(legend.position = "top"))

  result <- list(trajectory = trajectory, peak_table = peak_table, fit = fit, warmup = warmup_table,
                 warmup_reached = warmup_reached, figures = figures)
  if (!is.null(output_dir)) {
    save_unlimited_comparison(result, resolve(output_dir))
  }
  result
}

save_unlimited_comparison <- function(result, output_dir) {
  if (dir.exists(output_dir) && length(list.files(output_dir, all.files = TRUE, no.. = TRUE))) {
    stop("Output directory must be empty.")
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  for (name in c("trajectory", "peak_table", "fit", "warmup", "warmup_reached")) {
    utils::write.csv(result[[name]], file.path(output_dir, paste0(name, ".csv")), row.names = FALSE)
  }
  sizes <- list(trajectories = c(14, 6), warmup = c(9, 4.5))
  for (name in names(result$figures)) {
    for (extension in c("png", "pdf")) {
      ggplot2::ggsave(file.path(output_dir, paste0(name, ".", extension)), result$figures[[name]],
                      width = sizes[[name]][[1]], height = sizes[[name]][[2]], dpi = 300)
    }
  }
  invisible(output_dir)
}
