# Standalone research workflow. Source this file, then call load_study_functions().
# No app startup or package installation. Runners temporarily use sequential execution.
load_study_functions <- function(project_dir = ".", envir = parent.frame()) {
  required <- c("simmer", "future.apply", "dplyr", "tidyr", "ggplot2")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Install required packages: ", paste(missing, collapse = ", "))
  for (file in c("R/shared/simulation_metrics.R", "R/core/hospital_trajectory.R",
                 "R/core/baseline_flow.R", "R/core/run_scenarios.R",
                 "R/shared/profiles_deloitte.R")) {
    sys.source(file.path(project_dir, file), envir = envir)
  }
  invisible(TRUE)
}

# Reads the app's own search-budget presets from R/01_config.R without its
# side effects (no options(), no future::plan() change): evaluate only the
# last top-level expression, which is the bed_search_configs <- list(...)
# assignment. This is the same isolation trick tests/test_unified_search.R
# uses, so the manuscript pipeline and the deployed app declare these numbers
# exactly once and cannot silently drift apart.
read_app_search_presets <- function(project_dir = ".") {
  config_file <- file.path(project_dir, "R/01_config.R")
  config_expressions <- parse(config_file)
  presets <- new.env(parent = baseenv())
  eval(config_expressions[[length(config_expressions)]], envir = presets)
  if (!exists("bed_search_configs", envir = presets, inherits = FALSE)) {
    stop("Could not find bed_search_configs as the last expression in ", config_file, ".")
  }
  presets$bed_search_configs
}

make_study_config <- function(project_dir = ".",
                              # ED = 999 is a practically-unlimited placeholder (hallway/chair
                              # capacity is elastic in practice, not a hard bed count); boarding
                              # time, not this capacity, is what constrains an ED patient.
                              capacities = c(ICU = 84, GenMed = 405, Surge = 15, ED = 999),
                              civilian_file = file.path(project_dir, "data/baseline_civilian_profiles.csv"),
                              sim_days = 50, num_sims = 40L, seed = 2026L,
                              warmup = list(), search = list(), mode = c("paper", "development"),
                              workers = 3L) {
  mode <- match.arg(mode)
  profiles <- deloitte_test_profile_config()
  civilian <- utils::read.csv(civilian_file, stringsAsFactors = FALSE)
  stopifnot(all(c("Profile", "Patients_per_day", "Pathway", "Mean_stays_days") %in% names(civilian)),
            nrow(civilian) > 0, !anyDuplicated(civilian$Profile))
  # CV_values is optional, mirroring the app's read_baseline_profiles_csv():
  # a blank or absent entry defaults to default_cv_for_unit() (1 for ICU,
  # 0.24 for every other unit), not the engine's own flat 0.1 fallback.
  has_civilian_cv <- "CV_values" %in% names(civilian)
  baseline <- utils::modifyList(baseline_defaults(), warmup)
  baseline$enabled <- TRUE
  baseline$arrival_process <- "even"
  baseline$profiles <- stats::setNames(lapply(seq_len(nrow(civilian)), function(i) {
    units <- trimws(strsplit(civilian$Pathway[i], ",", fixed = TRUE)[[1]])
    los <- as.numeric(trimws(strsplit(civilian$Mean_stays_days[i], ",", fixed = TRUE)[[1]]))
    cv_entry <- if (has_civilian_cv) trimws(civilian$CV_values[i]) else NA_character_
    cv <- if (is.na(cv_entry) || !nzchar(cv_entry)) {
      default_cv_for_unit(units)
    } else {
      as.numeric(trimws(strsplit(cv_entry, ",", fixed = TRUE)[[1]]))
    }
    if (length(cv) == 1L && length(units) > 1L) cv <- rep(cv, length(units))
    list(unit = units, los = los, cv = cv)
  }), civilian$Profile)
  baseline$arrival_rates <- stats::setNames(civilian$Patients_per_day, civilian$Profile)
  config <- list(capacities = capacities, warmup_capacities = capacities,
                 patient_profiles = profiles$patient_profiles, profile_prob = profiles$profile_prob,
                 fallbacks = profiles$fallbacks, baseline = baseline, arrival_process = "even")
  validate_patient_configuration(capacities, config$patient_profiles, config$profile_prob, config$fallbacks)
  validate_baseline_config(baseline, capacities, config$fallbacks)
  app_presets <- read_app_search_presets(project_dir)
  if (!mode %in% names(app_presets)) {
    stop("R/01_config.R has no bed_search_configs entry named '", mode, "'.")
  }
  # num_sims, max_evaluations, final_num_sims, minimum_step, demand_safety_factor
  # and reliability_level come straight from R/01_config.R's bed_search_configs.
  # Only two things are specific to this offline pipeline, not the deployed app:
  # - search_seed is offset from the descriptive seed so search candidates use
  #   an independent random-number bank (see the manuscript's methodology).
  # - workers sizes local/offline parallel execution for this script; it is
  #   unrelated to R/01_config.R's own `workers`, which sizes the deployed
  #   app's future plan for its hosting platform.
  # boarding_time_limit_GenMed/ICU (days) have no app-side default (they are
  # interactive UI inputs in the dashboard), so they stay declared here.
  defaults <- utils::modifyList(app_presets[[mode]],
    list(search_seed = seed + 300000L, workers = workers,
         boarding_time_limit_GenMed = 1, boarding_time_limit_ICU = 1))
  retired <- intersect(names(search), c("search_num_sims", "max_validation_evaluations", "search_queue_tolerance"))
  if (length(retired)) stop("Retired search parameters: ", paste(retired, collapse = ", "),
    ". Use num_sims and max_evaluations for the unified search.")
  settings <- list(config = config, sim_days = sim_days, num_sims = num_sims, seed = seed,
       project_dir = normalizePath(project_dir, winslash = "/"), mode = mode,
       search = utils::modifyList(defaults, search), civilian_source = civilian_file,
       surge_source = profiles$source, service_cv = eval(formals(make_service_times)$cv))
  settings$engine_signature <- study_engine_signature(settings)
  settings
}

# Cache keys include inputs and engine versions, not the display label or study family.
study_fingerprint <- function(object) {
  path <- tempfile("study-key-")
  on.exit(unlink(path), add = TRUE)
  saveRDS(object, path, compress = FALSE, version = 2)
  unname(tools::md5sum(path))
}

study_engine_signature <- function(study) {
  files <- file.path(study$project_dir, c("R/shared/simulation_metrics.R",
    "R/core/hospital_trajectory.R", "R/core/baseline_flow.R",
    "R/core/run_scenarios.R"))
  # Fingerprint loaded function definitions, not files that another editor can
  # change while this R session is still executing the previously loaded code.
  names <- unique(unlist(lapply(files, function(file) {
    lines <- readLines(file, warn = FALSE)
    lines <- grep("^[A-Za-z_][A-Za-z_0-9.]* <- function", lines, value = TRUE)
    sub(" <- function.*", "", lines)
  })))
  definitions <- stats::setNames(lapply(names, function(name) {
    fun <- get(name, envir = environment(study_engine_signature), inherits = TRUE)
    # Plain text excludes mutable source-reference environments/JIT metadata.
    list(formals = paste(deparse(formals(fun)), collapse = "\n"),
         body = paste(deparse(body(fun)), collapse = "\n"))
  }), names)
  list(schema = 2L, definitions = definitions, R = R.version.string,
       packages = vapply(c("simmer", "future.apply", "dplyr", "tidyr"),
                         function(p) as.character(utils::packageVersion(p)), character(1)))
}

study_initial_signature <- function(run) {
  rows <- run$resource_history
  rows <- rows[rows$time < 0, setdiff(names(rows), c("scenario_id", "scenario_mode")), drop = FALSE]
  rows <- rows[order(rows$replication, rows$resource, rows$time), , drop = FALSE]
  rownames(rows) <- NULL
  study_fingerprint(list(rows, run$configuration$baseline,
    run$runs[c("replication", "seed", "sim_days", "warmup_days")]))
}

# Cached summaries retain neutral scenario labels so identical combinations can
# be reused across studies. Relabel only the single summary currently needed.
read_study_summary <- function(reference) {
  compact <- readRDS(reference$summary_path)
  for (name in names(compact$tables)) {
    if ("scenario_id" %in% names(compact$tables[[name]])) {
      compact$tables[[name]]$scenario_id <- reference$scenario_id
    }
  }
  compact
}
compare_study_summaries <- function(reference, comparison) {
  if (!identical(reference$initial_signature, comparison$initial_signature)) {
    stop("Paired comparisons require identical warm-up histories and replication settings.")
  }
  a <- reference$tables$resource_replications
  b <- comparison$tables$resource_replications
  keys <- c("replication", "resource", "metric")
  stopifnot(!anyDuplicated(a[keys]), !anyDuplicated(b[keys]),
            nrow(dplyr::anti_join(a, b, by = keys)) == 0,
            nrow(dplyr::anti_join(b, a, by = keys)) == 0)
  pairs <- dplyr::inner_join(a, b, by = keys, suffix = c("_reference", "_comparison")) |>
    dplyr::mutate(difference = .data$value_comparison - .data$value_reference)
  pairs |>
    dplyr::group_by(.data$resource, .data$metric) |>
    dplyr::group_modify(function(rows, key) {
      ci <- study_mean_interval(rows$difference)
      data.frame(reference_mean = mean(rows$value_reference), comparison_mean = mean(rows$value_comparison),
        n_pairs = ci$n, mean_difference = ci$mean, mcse = ci$mcse, lower = ci$lower, upper = ci$upper)
    }) |>
    dplyr::ungroup() |>
    dplyr::mutate(reference_scenario = a$scenario_id[1], comparison_scenario = b$scenario_id[1],
                  confidence_level = 0.90)
}

study_mean_interval <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  avg <- if (n) mean(x) else NA_real_
  se <- if (n > 1) stats::sd(x) / sqrt(n) else NA_real_
  width <- if (n > 1) stats::qt(0.95, n - 1) * se else NA_real_
  data.frame(n = n, mean = avg, mcse = se, lower = avg - width, upper = avg + width)
}

# Only requests resolved by the horizon contribute to resolved-only statistics.
# Full-cohort wait means remain NA when requests are pending (existing helper).
summarize_study_runs <- function(runs) {
  resources <- dplyr::bind_rows(lapply(runs, `[[`, "resources"))
  metrics <- resource_replication_metrics(resources)
  summary <- metrics |>
    dplyr::group_by(.data$scenario_id, .data$resource, .data$metric) |>
    dplyr::group_modify(function(rows, key) study_mean_interval(rows$value)) |>
    dplyr::ungroup()
  waits <- lapply(runs, bed_wait_summary)
  # Same daily-peak definition as the dashboard plot (make_resource_plot):
  # one maximum per scenario/resource/replication/day, then the median and
  # 10th-90th percentile band across replications. These are daily peaks,
  # not daily means, and the band is between-replication spread, not a CI.
  daily <- dplyr::bind_rows(lapply(c("server", "queue"), function(variable) {
    daily_peak_by_replication(resources, variable) |>
      dplyr::mutate(metric = variable)
  }))
  daily_summary <- dplyr::bind_rows(lapply(c("server", "queue"), function(variable) {
    make_daily_peak_summary(resources, var = variable) |>
      dplyr::mutate(metric = variable)
  }))
  list(resource_replications = metrics, resource_summary = summary,
       daily_replications = daily, daily_summary = daily_summary,
       wait_summary = dplyr::bind_rows(lapply(waits, `[[`, "summary")),
       wait_replications = dplyr::bind_rows(lapply(waits, `[[`, "replications")),
       warmup_diagnostics = dplyr::bind_rows(lapply(runs, `[[`, "warmup_diagnostics")),
       run_metadata = dplyr::bind_rows(lapply(runs, `[[`, "runs")))
}

# Turns "concentration_5" into "concentration_rate30_duration5" (and
# "reference" into "reference_rate15_duration10"), using the design table's
# rate/duration for that scenario, so the rate and duration behind a scenario
# are readable directly from a legend or axis instead of a bare numeric
# suffix that means "duration" for concentration_* but "rate" for volume_*.
# "baseline" (no surge) and "_expanded" suffixes are preserved as-is.
scenario_labels <- function(scenario_id, design) {
  design_lookup <- rbind(
    data.frame(scenario_id = "baseline", rate = 0L, duration = 0L),
    design[c("scenario_id", "rate", "duration")]
  )
  expanded <- grepl("_expanded$", scenario_id)
  base_id <- sub("_expanded$", "", scenario_id)
  match_index <- match(base_id, design_lookup$scenario_id)
  if (anyNA(match_index)) {
    stop("No rate/duration found in the design table for: ",
         paste(unique(base_id[is.na(match_index)]), collapse = ", "))
  }
  family <- ifelse(base_id == "baseline", "baseline", sub("_[0-9]+$", "", base_id))
  label <- ifelse(base_id == "baseline", "baseline",
    sprintf("%s_rate%d_duration%d", family,
            design_lookup$rate[match_index], design_lookup$duration[match_index]))
  ifelse(expanded, paste0(label, "_expanded"), label)
}

make_study_figures <- function(tables) {
  daily <- tables$daily_summary
  daily$scenario_id <- scenario_labels(daily$scenario_id, tables$design)
  daily$measure <- factor(ifelse(daily$metric == "server", "Occupied beds", "Queue (patients)"),
                          levels = c("Occupied beds", "Queue (patients)"))
  unit_order <- c(intersect(c("GenMed", "ICU", "Surge"), unique(daily$resource)),
                  setdiff(unique(daily$resource), c("GenMed", "ICU", "Surge")))
  daily$resource <- factor(daily$resource, levels = unit_order)
  figures <- list(trajectories = ggplot2::ggplot(daily,
    ggplot2::aes(x = .data$time1 - 0.5, y = .data$median_val, color = .data$scenario_id,
                 fill = .data$scenario_id)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
                         alpha = 0.12, color = NA, na.rm = TRUE) +
    ggplot2::geom_line() + ggplot2::facet_wrap(ggplot2::vars(measure, resource),
                                             ncol = length(unit_order), scales = "free_y") +
    ggplot2::labs(x = "Days after surge onset (daily interval midpoint)", y = NULL,
      color = "Scenario", fill = "Scenario",
      caption = paste("Median of daily maxima across replications; shaded band shows",
                       "the 10th-90th percentiles. Daily peaks, not daily means.")) +
    ggplot2::theme_bw())
  waits <- tables$wait_summary
  if (nrow(waits)) {
    waits$scenario_id <- scenario_labels(waits$scenario_id, tables$design)
    figures$resolved_waits <- ggplot2::ggplot(waits,
      ggplot2::aes(x = .data$scenario_id, y = .data$mean_resolved_wait_days, fill = .data$population)) +
      ggplot2::geom_col(position = "dodge", na.rm = TRUE) +
      ggplot2::facet_wrap(ggplot2::vars(resource, cohort), scales = "free_y") +
      ggplot2::labs(x = NULL, y = "Mean resolved-request wait (days)", fill = "Population",
        caption = "Resolved requests only; consult pending-request counts in the accompanying table.") +
      ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
    figures$pending_requests <- ggplot2::ggplot(waits,
      ggplot2::aes(x = .data$scenario_id, y = .data$pending_requests, fill = .data$population)) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::facet_wrap(ggplot2::vars(resource, cohort), scales = "free_y") +
      ggplot2::labs(x = NULL, y = "Pending requests (sum across replications)", fill = "Population") +
      ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
  }
  if (nrow(tables$expansion)) {
    additions <- tables$expansion |>
      dplyr::filter(.data$accepted) |>
      tidyr::pivot_longer(c("GenMed_added", "ICU_added"), names_to = "unit", values_to = "beds")
    if (nrow(additions)) {
      additions$scenario_id <- scenario_labels(additions$scenario_id, tables$design)
      figures$expansion <- ggplot2::ggplot(additions,
        ggplot2::aes(x = .data$scenario_id, y = .data$beds, fill = .data$unit)) +
        ggplot2::geom_col(position = "dodge") + ggplot2::theme_bw() +
        ggplot2::labs(x = NULL, y = "Additional beds", fill = "Unit",
          caption = "Candidates passing independent final evaluation; global optimality is not established.")
    }
  }
  figures
}

# General runner: one routine baseline plus arbitrary rate/duration combinations.
# Expansion is activated only at day zero; warm-up capacity remains unchanged.
run_scenario_study <- function(study, design, expand = FALSE, output_dir = NULL,
                               reference_id = NULL,
                               cache_dir = file.path(study$project_dir, "outputs/scenario_cache")) {
  stopifnot(all(c("scenario_id", "rate", "duration") %in% names(design)), nrow(design) > 0,
    !anyNA(design), !anyDuplicated(design$scenario_id),
    all(grepl("^[A-Za-z][A-Za-z0-9_]*$", design$scenario_id)),
    !any(design$scenario_id == "baseline"),
    !any(paste0(design$scenario_id, "_expanded") %in% design$scenario_id),
    all(design$rate > 0 & design$rate == floor(design$rate)),
    all(design$duration > 0 & design$duration == floor(design$duration)),
    study$sim_days >= max(design$duration), study$config$arrival_process == "even",
    study$config$baseline$arrival_process == "even",
    is.null(reference_id) || (length(reference_id) == 1L && !is.na(reference_id) &&
      reference_id %in% design$scenario_id))
  if (!is.null(output_dir) && dir.exists(output_dir) && length(list.files(output_dir, all.files = TRUE,
                                                                            no.. = TRUE))) {
    stop("Output directory is not empty; choose a new directory to preserve earlier results.")
  }
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  # study$search$workers sizes local/offline parallel execution for this
  # pipeline (see make_study_config). Sequential is kept as the safe default
  # for workers <= 1, matching the deployed app's own fallback in 01_config.R.
  requested_workers <- study$search$workers
  if (is.null(requested_workers)) requested_workers <- 1L
  if (requested_workers > 1L) {
    future::plan(future::multisession, workers = requested_workers)
  } else {
    future::plan(future::sequential)
  }
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_dir <- normalizePath(cache_dir, winslash = "/", mustWork = TRUE)
  engine <- study_engine_signature(study)
  if (!is.null(study$engine_signature) && !identical(engine, study$engine_signature)) {
    stop("Loaded simulation functions changed after configuration. Recreate the study before running it.")
  }
  cache_events <- list()
  run_one <- function(config, rate, duration, id) {
    key <- study_fingerprint(list(engine = engine, config = config, rate = rate, duration = duration,
                                  horizon = study$sim_days, replications = study$num_sims, seed = study$seed))
    raw_path <- file.path(cache_dir, paste0(key, "_raw.rds"))
    summary_path <- file.path(cache_dir, paste0(key, "_summary.rds"))
    hit <- file.exists(summary_path) && file.exists(raw_path)
    if (hit) {
      message("Reusing ", id, " (", rate, " x ", duration, ")")
    } else {
      message("Simulating ", id, " (", rate, " x ", duration, ")")
      # Only this scenario's raw data are resident; other scenarios remain on disk.
      run <- run_hospital_scenario(config, duration, rate, study$sim_days, study$num_sims, study$seed,
                                   paste0("rate_", rate, "_days_", duration))
      saveRDS(run, raw_path)
      compact <- list(tables = summarize_study_runs(list(run)), raw_path = raw_path,
                       initial_signature = study_initial_signature(run))
      saveRDS(compact, summary_path)
      rm(run, compact)
      invisible(gc())
    }
    cache_events[[length(cache_events) + 1L]] <<- data.frame(scenario_id = id, kind = "simulation",
      key = key, reused = hit, path = raw_path)
    list(raw_path = raw_path, summary_path = summary_path, scenario_id = id)
  }
  runs <- list(baseline = run_one(study$config, 0, 0, "baseline"))
  diagnostics <- read_study_summary(runs$baseline)$tables$warmup_diagnostics |>
    dplyr::group_by(.data$replication, .data$resource) |>
    dplyr::filter(.data$check_time == max(.data$check_time)) |>
    dplyr::ungroup()
  if (any(!diagnostics$passed)) {
    warning("Baseline warm-up diagnostic failed in some units/replications. See warmup_diagnostics; do not assume equilibrium.")
  }
  # Diagnostics are exported; fixed warm-up is not described as equilibrium.
  searches <- list()
  expansion <- list()
  comparisons <- list()
  for (i in seq_len(nrow(design))) {
    row <- design[i, ]
    id <- as.character(row$scenario_id)
    runs[[id]] <- run_one(study$config, row$rate, row$duration, id)
    comparisons[[id]] <- compare_study_summaries(read_study_summary(runs$baseline), read_study_summary(runs[[id]]))
    if (!expand) {
      invisible(gc())
      next
    }
    message("Searching capacity for ", id)
    args <- c(list(capacities = study$config$capacities, duration = row$duration,
      n_patients = row$rate, sim_days = study$sim_days,
      patient_profiles = study$config$patient_profiles, profile_prob = study$config$profile_prob,
      fallbacks = study$config$fallbacks, baseline = study$config$baseline,
      warmup_capacities = study$config$warmup_capacities, arrival_process = "even"), study$search)
    search_key <- study_fingerprint(list(engine = engine, args = args))
    search_path <- file.path(cache_dir, paste0(search_key, "_search.rds"))
    search_hit <- file.exists(search_path)
    if (search_hit) {
      message("Reusing capacity search for ", id)
      fit <- readRDS(search_path)
    } else {
      fit <- do.call(find_n_needed, args)
      saveRDS(fit, search_path)
    }
    cache_events[[length(cache_events) + 1L]] <- data.frame(scenario_id = id, kind = "search",
      key = search_key, reused = search_hit, path = search_path)
    searches[[id]] <- search_path
    expansion[[id]] <- data.frame(scenario_id = id, rate = row$rate, duration = row$duration,
      GenMed_added = fit$N_added, ICU_added = fit$N_added_ICU,
      joint_compliance = fit$joint_reliability, joint_lower_ci = fit$joint_lower_ci,
      acceptance_rule = fit$search_configuration$acceptance_rule, accepted = isTRUE(fit$converged),
      refinement_complete = fit$refinement_complete, final_evaluations = fit$final_evaluations,
      final_replications = fit$final_num_sims, search_evaluations = fit$search_evaluations,
      workers = fit$search_configuration$workers,
      optimization_elapsed_seconds = fit$optimization_elapsed_seconds)
    if (!isTRUE(fit$converged)) {
      warning("No accepted expansion for ", id, "; candidate retained as failed, not applied.")
      rm(fit)
      invisible(gc())
      next
    }
    config <- study$config
    config$capacities[c("GenMed", "ICU")] <- c(fit$GenMed_N, fit$ICU_N)
    rm(fit)
    invisible(gc())
    expanded_id <- paste0(id, "_expanded")
    runs[[expanded_id]] <- run_one(config, row$rate, row$duration, expanded_id)
    comparisons[[expanded_id]] <- compare_study_summaries(read_study_summary(runs[[id]]), read_study_summary(runs[[expanded_id]]))
    invisible(gc())
  }
  # A second, optional reference (e.g. a rate/duration combination shared by
  # two designs) adds cross-scenario comparisons alongside the always-present
  # baseline (no-surge) comparisons already stored above. Both share the same
  # paired_comparisons schema; reference_scenario identifies which was used.
  if (!is.null(reference_id)) {
    reference_summary <- read_study_summary(runs[[reference_id]])
    for (id in setdiff(names(runs), c("baseline", reference_id))) {
      comparisons[[paste0(id, "_vs_", reference_id)]] <-
        compare_study_summaries(reference_summary, read_study_summary(runs[[id]]))
    }
  }
  # Assemble one table at a time from disk after all combinations finish.
  table_names <- names(read_study_summary(runs$baseline)$tables)
  invisible(gc())
  tables <- stats::setNames(lapply(table_names, function(name) {
    dplyr::bind_rows(lapply(runs, function(run) read_study_summary(run)$tables[[name]]))
  }), table_names)
  tables$design <- dplyr::mutate(design, total_surge_arrivals = .data$rate * .data$duration)
  tables$expansion <- dplyr::bind_rows(expansion)
  tables$paired_comparisons <- dplyr::bind_rows(comparisons)
  for (field in c("evaluation_history", "replication_history", "final_joint_interval", "final_intervals")) {
    tables[[paste0("search_", field)]] <- dplyr::bind_rows(lapply(names(searches), function(id) {
      rows <- readRDS(searches[[id]])[[field]]
      if (!nrow(rows)) return(data.frame())
      dplyr::mutate(rows, scenario_id = id)
    }))
  }
  tables$cache_usage <- dplyr::bind_rows(cache_events)
  # Raw monitors are referenced, never duplicated inside the exported study.
  raw_files <- vapply(runs, `[[`, character(1), "raw_path")
  result <- list(study = study, tables = tables, runs = raw_files, summaries = runs, searches = searches,
                 session_info = utils::sessionInfo())
  # Keep the public study view focused on trajectories and metric tables.
  result$cache_dir <- cache_dir
  result$figures <- make_study_figures(tables)["trajectories"]
  if (!is.null(output_dir)) save_study_outputs(result, output_dir)
  result
}

save_study_outputs <- function(result, output_dir) {
  if (dir.exists(output_dir) && length(list.files(output_dir, all.files = TRUE, no.. = TRUE))) {
    stop("Output directory must be empty.")
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(result, file.path(output_dir, "study.rds"))
  for (name in names(result$tables)) {
    rows <- result$tables[[name]]
    # Search histories include RNG-state list columns. Preserve the exact
    # objects in RDS and serialize their R representation for CSV inspection.
    for (column in names(rows)[vapply(rows, is.list, logical(1))]) {
      rows[[column]] <- vapply(rows[[column]], function(value) paste(deparse(value), collapse = " "), character(1))
    }
    utils::write.csv(rows, file.path(output_dir, paste0(name, ".csv")), row.names = FALSE)
  }
  for (name in names(result$figures)) {
    for (extension in c("pdf", "png")) {
      ggplot2::ggsave(file.path(output_dir, paste0(name, ".", extension)),
        result$figures[[name]], width = 12, height = 8, units = "in", dpi = 300, limitsize = FALSE)
    }
  }
  invisible(output_dir)
}

run_main_example <- function(study, rate = 10, duration = 9, ...) {
  run_scenario_study(study, data.frame(scenario_id = "reference", rate = rate, duration = duration), ...)
}

run_volume_study <- function(study, rates = c(10, 15, 20), duration = 10, ...) {
  run_scenario_study(study, data.frame(scenario_id = paste0("volume_", rates), rate = rates,
                                      duration = duration), ...)
}

run_concentration_study <- function(study, total = 150, durations = c(5, 10, 15), ...) {
  rates <- total / durations
  if (any(!is.finite(rates)) || any(rates != floor(rates))) {
    stop("For deterministic arrivals, total/duration must be an integer rate.")
  }
  run_scenario_study(study, data.frame(scenario_id = paste0("concentration_", durations),
                                      rate = rates, duration = durations), ...)
}

# The manuscript's surge scenario set: volume analysis (fixed duration,
# varying rate) and concentration analysis (fixed total patients, varying
# duration). The rate/duration combination the two designs share (by default
# rate = 15, duration = 10) is included once, under scenario_id =
# reference_id, instead of twice under two different labels (e.g. "volume_15"
# and "concentration_10"). Declared once here so run_volume_and_concentration_study
# and run_unlimited_demand_study simulate exactly the same scenario set.
manuscript_scenario_design <- function(volume_duration = 10, volume_rates = c(10, 15, 20),
                                       concentration_total = 150,
                                       concentration_durations = c(5, 10, 15),
                                       reference_id = "reference") {
  concentration_rates <- concentration_total / concentration_durations
  if (any(!is.finite(concentration_rates)) || any(concentration_rates != floor(concentration_rates))) {
    stop("For deterministic arrivals, total/duration must be an integer rate.")
  }
  volume <- data.frame(rate = volume_rates, duration = volume_duration)
  concentration <- data.frame(rate = concentration_rates, duration = concentration_durations)
  shared <- merge(volume, concentration)
  if (nrow(shared) != 1L) {
    stop("Expected exactly one rate/duration combination shared between the volume ",
         "design (duration = ", volume_duration, ") and the concentration design ",
         "(total = ", concentration_total, "); found ", nrow(shared), ". Adjust ",
         "volume_rates/volume_duration or concentration_total/concentration_durations ",
         "so exactly one combination coincides.")
  }
  design <- unique(rbind(volume, concentration))
  is_shared <- design$rate == shared$rate & design$duration == shared$duration
  design$scenario_id <- ifelse(is_shared, reference_id,
    ifelse(design$duration == volume_duration, paste0("volume_", design$rate),
           paste0("concentration_", design$duration)))
  design <- design[order(design$duration, design$rate), c("scenario_id", "rate", "duration")]
  rownames(design) <- NULL
  design
}

# Combines the volume and concentration manuscript designs into a single run,
# avoiding duplicate simulation and duplicate rows for their shared scenario
# (see manuscript_scenario_design). The reference scenario is also used as the
# comparison baseline for every other volume/concentration scenario (via
# reference_id), on top of the existing baseline (no-surge) comparisons that
# run_scenario_study always computes. The routine civilian baseline keeps
# running in every scenario; "reference" here names a surge scenario, not a
# no-surge run.
run_volume_and_concentration_study <- function(study, volume_duration = 10,
                                               volume_rates = c(10, 15, 20),
                                               concentration_total = 150,
                                               concentration_durations = c(5, 10, 15),
                                               reference_id = "reference", ...) {
  design <- manuscript_scenario_design(volume_duration, volume_rates,
    concentration_total, concentration_durations, reference_id)
  run_scenario_study(study, design, reference_id = reference_id, ...)
}

# Runs the same manuscript scenario set (see manuscript_scenario_design) with
# every configured unit's bed capacity set to `capacity` beds, both during
# warm-up and observation, so no patient -- civilian or surge -- ever queues.
# Occupancy under this run is unconstrained demand: what would be admitted if
# capacity were never a limiting factor, contrasted against the
# capacity-constrained runs from run_volume_and_concentration_study. No
# capacity search is run (there is nothing to search for when capacity
# already exceeds any plausible need); expand is not exposed as a parameter.
# `capacity` must comfortably exceed peak simultaneous demand in every unit,
# or this stops being effectively unconstrained -- inspect resource_summary's
# peak utilization/time-at-capacity to confirm no unit ever saturates.
run_unlimited_demand_study <- function(study, capacity = 500L, volume_duration = 10,
                                       volume_rates = c(10, 15, 20),
                                       concentration_total = 150,
                                       concentration_durations = c(5, 10, 15),
                                       reference_id = "reference", output_dir = NULL,
                                       cache_dir = file.path(study$project_dir, "outputs/scenario_cache")) {
  stopifnot(length(capacity) == 1L, is.finite(capacity), capacity > 0,
            capacity == floor(capacity))
  design <- manuscript_scenario_design(volume_duration, volume_rates,
    concentration_total, concentration_durations, reference_id)
  unlimited <- stats::setNames(rep(as.integer(capacity), length(study$config$capacities)),
    names(study$config$capacities))
  study$config$capacities <- unlimited
  study$config$warmup_capacities <- unlimited
  run_scenario_study(study, design, expand = FALSE, output_dir = output_dir,
                     reference_id = reference_id, cache_dir = cache_dir)
}

# Step 2: optimize only explicitly selected IDs from a completed first-stage study.
# Example: optimize_study_scenarios(volume, c("volume_10", "volume_15"))
# A saved study.rds path can be supplied instead of retaining the first-stage object.
optimize_study_scenarios <- function(scenarios, scenario_ids, search = list(),
                                     output_dir = NULL) {
  if (is.character(scenarios) && length(scenarios) == 1L) {
    scenarios <- readRDS(scenarios)
  }
  design <- scenarios$tables$design
  if (!is.character(scenario_ids) || !length(scenario_ids) || anyNA(scenario_ids) ||
      anyDuplicated(scenario_ids) || !all(scenario_ids %in% design$scenario_id)) {
    stop("Choose one or more unique scenario IDs from scenarios$tables$design$scenario_id.")
  }
  selected <- design[match(scenario_ids, design$scenario_id),
                     c("scenario_id", "rate", "duration"), drop = FALSE]
  references <- scenarios$summaries[c("baseline", scenario_ids)]
  if (length(references) != length(scenario_ids) + 1L ||
      any(vapply(references, is.null, logical(1))) ||
      !all(vapply(references, function(x) file.exists(x$raw_path) &&
                    file.exists(x$summary_path), logical(1)))) {
    stop("Selected scenario cache files are missing. Restore them before optimization.")
  }
  study <- scenarios$study
  study$search <- utils::modifyList(study$search, search)
  cache_dir <- scenarios$cache_dir
  if (is.null(cache_dir)) cache_dir <- dirname(references[[1]]$raw_path)
  rm(scenarios, references)
  invisible(gc())
  # The runner reuses the baseline and selected originals from disk. Only accepted
  # expansions are simulated; failed candidates remain recorded in the search cache.
  run_scenario_study(study, selected, expand = TRUE, output_dir = output_dir,
                     cache_dir = cache_dir)
}

# Combines the tables from several optimize_study_scenarios() output
# directories -- each run separately, e.g. one scenario at a time to bound
# memory use -- into one set of tables and figures, without re-simulating
# anything. Every run must share the same routine-civilian baseline
# (identical config and seed): its scenario_id ("baseline") is deduplicated
# rather than plotted or averaged once per run. output_dirs order only
# affects which run's copy of shared rows (e.g. baseline) is kept; their
# values must already agree, or dplyr::distinct() will retain more than one.
combine_optimized_studies <- function(output_dirs, out_file = NULL) {
  if (!is.character(output_dirs) || !length(output_dirs)) {
    stop("output_dirs must be one or more paths to optimize_study_scenarios() output directories.")
  }
  table_names <- c("daily_summary", "design", "expansion", "wait_summary",
                   "resource_summary", "run_metadata", "warmup_diagnostics")
  studies <- lapply(output_dirs, function(d) readRDS(file.path(d, "study.rds")))
  tables <- stats::setNames(lapply(table_names, function(name) {
    dplyr::distinct(dplyr::bind_rows(lapply(studies, function(s) s$tables[[name]])))
  }), table_names)
  key_cols <- list(daily_summary = c("scenario_id", "resource", "time1", "metric"),
                   design = "scenario_id")
  for (name in names(key_cols)) {
    dup <- tables[[name]][key_cols[[name]]]
    if (anyDuplicated(dup)) {
      stop("Conflicting rows for the same ", paste(key_cols[[name]], collapse = "/"),
           " across output_dirs in table '", name, "'. Runs must share an identical baseline.")
    }
  }
  figures <- make_study_figures(tables)
  if (!is.null(out_file)) {
    ggplot2::ggsave(out_file, figures$trajectories, width = 12, height = 8,
      units = "in", dpi = 300, limitsize = FALSE)
  }
  list(tables = tables, figures = figures)
}