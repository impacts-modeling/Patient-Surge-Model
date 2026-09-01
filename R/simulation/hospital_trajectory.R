validate_patient_configuration <- function(capacities, patient_profiles, profile_prob, fallbacks) {
  stopifnot(
    is.numeric(capacities),
    length(capacities) > 0,
    !is.null(names(capacities)),
    all(is.finite(capacities)),
    all(capacities >= 0),
    is.list(patient_profiles),
    length(patient_profiles) > 0,
    is.numeric(profile_prob),
    setequal(names(patient_profiles), names(profile_prob)),
    all(is.finite(profile_prob)),
    all(profile_prob >= 0),
    abs(sum(profile_prob) - 1) < 1e-6,
    is.list(fallbacks)
  )

  profile_units <- unique(unlist(lapply(patient_profiles, `[[`, "unit"), use.names = FALSE))
  fallback_units <- unique(c(names(fallbacks), unlist(fallbacks, use.names = FALSE)))
  configured_units <- names(capacities)
  if (length(setdiff(profile_units, configured_units)) > 0) {
    stop("Every patient trajectory unit must have a configured capacity.")
  }
  if (length(setdiff(fallback_units, configured_units)) > 0) {
    stop("Every fallback unit must have a configured capacity.")
  }
  invisible(TRUE)
}

random_stay <- function(mean_days, cv = 0.2) {
  sigma <- sqrt(log(1 + cv^2))
  stats::rlnorm(1, meanlog = log(mean_days) - sigma^2 / 2, sdlog = sigma)
}



logical_queue_prefix <- ".waiting_for__"

logical_queue_resource <- function(primary) {
  paste0(logical_queue_prefix, primary)
}


attempt_bed <- function(env, traj, primary, mean_stay, fallbacks,
                        trajectory_step_id, recheck_interval_days = 1) {
  fallback_units <- fallbacks[[primary]]
  if (is.null(fallback_units)) fallback_units <- character()
  candidates <- unique(c(primary, fallback_units))
  waiting_resource <- logical_queue_resource(primary)
  retry_tag <- paste0("retry_bed_", trajectory_step_id)

  choose_available_unit <- function() {
    for (index in seq_along(candidates)) {
      unit_name <- candidates[[index]]
      if (simmer::get_server_count(env, unit_name) <
          simmer::get_capacity(env, unit_name)) {
        return(index)
      }
    }
    length(candidates) + 1L
  }

  use_bed <- function(unit_name, release_wait_counter = FALSE) {
    bed_trajectory <- simmer::trajectory(paste0("use_", unit_name))
    if (release_wait_counter) {
      bed_trajectory <- bed_trajectory |>
        simmer::release(waiting_resource, 1)
    }
    bed_trajectory |>
      simmer::seize(unit_name, 1) |>
      simmer::timeout(function() random_stay(mean_stay)) |>
      simmer::release(unit_name, 1)
  }

  retry_branches <- c(
    lapply(candidates, use_bed, release_wait_counter = TRUE),
    list(
      simmer::trajectory("wait_and_recheck") |>
        simmer::timeout(recheck_interval_days) |>
        simmer::rollback(retry_tag)
    )
  )
  retry_trajectory <- do.call(
    simmer::branch,
    c(
      list(
        .trj = simmer::trajectory("recheck_beds"),
        option = choose_available_unit,
        continue = rep(TRUE, length(retry_branches))
      ),
      retry_branches,
      list(tag = retry_tag)
    )
  )

  initial_branches <- c(
    lapply(candidates, use_bed),
    list(
      simmer::trajectory("enter_logical_queue") |>
        simmer::seize(waiting_resource, 1) |>
        simmer::timeout(recheck_interval_days) |>
        simmer::join(retry_trajectory)
    )
  )
  initial_attempt <- do.call(
    simmer::branch,
    c(
      list(
        .trj = simmer::trajectory("initial_bed_attempt"),
        option = choose_available_unit,
        continue = rep(TRUE, length(initial_branches))
      ),
      initial_branches
    )
  )

  traj |> simmer::join(initial_attempt)
}

profile_trajectory <- function(env, profile_name, patient_profiles, fallbacks,
                               recheck_interval_days = 1) {
  if (!profile_name %in% names(patient_profiles)) {
    stop("Unknown patient profile: ", profile_name)
  }
  profile <- patient_profiles[[profile_name]]
  trajectory <- simmer::trajectory(profile_name)
  if (is.null(profile$unit) || length(profile$unit) == 0) {
    return(trajectory |> simmer::timeout(0.1))
  }
  for (index in seq_along(profile$unit)) {
    trajectory <- attempt_bed(
      env = env,
      traj = trajectory,
      primary = profile$unit[[index]],
      mean_stay = profile$los[[index]],
      fallbacks = fallbacks,
      trajectory_step_id = paste(profile_name, index, sep = "_"),
      recheck_interval_days = recheck_interval_days
    )
  }
  trajectory
}

sample_patient_profile <- function(profile_prob) {
  base::sample(names(profile_prob), size = 1, prob = profile_prob)
}

run_simulation <- function(capacities, duration, n_patients, sim_days,
                           patient_profiles, profile_prob, fallbacks = list(),
                           recheck_interval_days = 1) {
  validate_patient_configuration(capacities, patient_profiles, profile_prob, fallbacks)
  stopifnot(
    length(recheck_interval_days) == 1,
    is.finite(recheck_interval_days),
    recheck_interval_days > 0
  )
  hospital_sim <- simmer::simmer("hospital-simulation")
  for (unit_name in names(capacities)) {
    hospital_sim <- simmer::add_resource(
      hospital_sim,
      unit_name,
      capacity = as.integer(capacities[[unit_name]]),
      queue_size = Inf
    )
    hospital_sim <- simmer::add_resource(
      hospital_sim,
      logical_queue_resource(unit_name),
      capacity = Inf,
      queue_size = 0
    )
  }

  trajectories <- stats::setNames(
    lapply(names(patient_profiles), function(profile_name) {
      profile_trajectory(
        hospital_sim,
        profile_name,
        patient_profiles,
        fallbacks,
        recheck_interval_days = recheck_interval_days
      )
    }),
    names(patient_profiles)
  )

  patient_data <- base::expand.grid(
    day = seq_len(duration),
    patient_num = seq_len(n_patients),
    KEEP.OUT.ATTRS = FALSE
  ) |>
    dplyr::mutate(
      arrival_time = .data$day + (.data$patient_num - 1) / n_patients
    )

  patient_data$profile_name <- base::sample(
    names(profile_prob),
    size = nrow(patient_data),
    replace = TRUE,
    prob = profile_prob
  )

  for (profile_name in names(trajectories)) {
    arrival_times <- sort(patient_data$arrival_time[
      patient_data$profile_name == profile_name
    ])
    if (length(arrival_times) == 0) next

    hospital_sim <- simmer::add_generator(
      hospital_sim,
      name_prefix = paste0("patient_", profile_name, "_"),
      trajectory = trajectories[[profile_name]],
      distribution = simmer::at(arrival_times)
    )
  }

  hospital_sim |>
    simmer::run(until = sim_days) |>
    simmer::wrap()
}

get_hospital_mon_resources <- function(simulation, include_resources = NULL) {
  resources <- simmer::get_mon_resources(simulation)
  required_columns <- c(
    "resource", "time", "server", "queue", "capacity",
    "queue_size", "system", "limit"
  )
  stopifnot(all(required_columns %in% names(resources)))
  if (nrow(resources) == 0) return(resources)

  if (!is.null(include_resources)) {
    include_resources <- unique(as.character(include_resources))
  }

  logical_rows <- startsWith(resources$resource, logical_queue_prefix)
  if (!any(logical_rows)) {
    if (!is.null(include_resources)) {
      resources <- resources[resources$resource %in% include_resources, , drop = FALSE]
    }
    return(resources)
  }

  waiting_resources <- unique(resources$resource[logical_rows])
  if (!is.null(include_resources)) {
    waiting_primary <- substring(
      waiting_resources,
      nchar(logical_queue_prefix) + 1L
    )
    waiting_resources <- waiting_resources[waiting_primary %in% include_resources]
    physical_resources <- resources[
      !logical_rows & resources$resource %in% include_resources,
      , drop = FALSE
    ]
  } else {
    physical_resources <- resources[!logical_rows, , drop = FALSE]
  }

  carry_forward <- function(event_times, event_values, query_times, default = 0) {
    order_index <- order(event_times, seq_along(event_times))
    event_times <- event_times[order_index]
    event_values <- event_values[order_index]
    event_index <- findInterval(query_times, event_times)
    values <- rep(default, length(query_times))
    has_value <- event_index > 0
    values[has_value] <- event_values[event_index[has_value]]
    values
  }

  consolidated <- lapply(waiting_resources, function(waiting_resource) {
    primary <- substring(waiting_resource, nchar(logical_queue_prefix) + 1L)
    primary_rows <- resources[resources$resource == primary, , drop = FALSE]
    waiting_rows <- resources[
      resources$resource == waiting_resource, , drop = FALSE
    ]
    if (nrow(primary_rows) == 0) {
      primary_rows <- waiting_rows[1, , drop = FALSE]
      primary_rows$resource <- primary
      primary_rows$server <- 0
      primary_rows$queue <- 0
      primary_rows$capacity <- simmer::get_capacity(simulation, primary)
      primary_rows$queue_size <- simmer::get_queue_size(simulation, primary)
      primary_rows$system <- 0
      primary_rows$limit <- primary_rows$capacity + primary_rows$queue_size
    }

    replication_values <- if ("replication" %in% names(resources)) {
      unique(waiting_rows$replication)
    } else {
      NA_integer_
    }

    dplyr::bind_rows(lapply(replication_values, function(replication_value) {
      if ("replication" %in% names(resources)) {
        primary_rep <- primary_rows[
          primary_rows$replication == replication_value, , drop = FALSE
        ]
        waiting_rep <- waiting_rows[
          waiting_rows$replication == replication_value, , drop = FALSE
        ]
      } else {
        primary_rep <- primary_rows
        waiting_rep <- waiting_rows
      }

      event_times <- sort(unique(c(primary_rep$time, waiting_rep$time)))
      primary_order <- order(primary_rep$time, seq_len(nrow(primary_rep)))
      primary_rep <- primary_rep[primary_order, , drop = FALSE]
      template_index <- pmax(findInterval(event_times, primary_rep$time), 1L)
      result <- primary_rep[template_index, , drop = FALSE]
      result$resource <- primary
      result$time <- event_times
      result$server <- carry_forward(
        primary_rep$time, primary_rep$server, event_times
      )
      result$queue <- carry_forward(
        primary_rep$time, primary_rep$queue, event_times
      ) + carry_forward(
        waiting_rep$time, waiting_rep$server, event_times
      )
      result$system <- result$server + result$queue
      result
    }))
  })

  consolidated_primary <- unique(vapply(
    waiting_resources,
    function(waiting_resource) {
      substring(waiting_resource, nchar(logical_queue_prefix) + 1L)
    },
    character(1)
  ))

  dplyr::bind_rows(
    physical_resources[
      !physical_resources$resource %in% consolidated_primary, , drop = FALSE
    ],
    consolidated
  ) |>
    dplyr::arrange(.data$time, .data$resource)
}

get_hospital_mon_arrivals <- function(simulation) {
  arrivals <- simmer::get_mon_arrivals(simulation)
  arrivals_by_resource <- simmer::get_mon_arrivals(
    simulation,
    per_resource = TRUE
  )
  logical_wait <- arrivals_by_resource |>
    dplyr::filter(startsWith(.data$resource, logical_queue_prefix)) |>
    dplyr::group_by(.data$name, .data$replication) |>
    dplyr::summarise(
      logical_wait_days = sum(.data$activity_time, na.rm = TRUE),
      .groups = "drop"
    )
  if (nrow(logical_wait) == 0) return(arrivals)

  arrivals |>
    dplyr::left_join(logical_wait, by = c("name", "replication")) |>
    dplyr::mutate(
      logical_wait_days = dplyr::coalesce(.data$logical_wait_days, 0),
      activity_time = pmax(0, .data$activity_time - .data$logical_wait_days)
    ) |>
    dplyr::select(-dplyr::all_of("logical_wait_days"))
}

estimate_peak_unit_demand <- function(patient_profiles, profile_prob, n_patients,
                                      duration, sim_days,
                                      units = c("GenMed", "ICU")) {
  stopifnot(
    length(n_patients) == 1,
    length(duration) == 1,
    length(sim_days) == 1,
    n_patients >= 1,
    duration >= 1,
    sim_days >= 1
  )

  arrival_times <- as.vector(
    outer(seq_len(duration), (seq_len(n_patients) - 1) / n_patients, "+")
  )
  events <- stats::setNames(lapply(units, function(unit_name) {
    data.frame(unit = character(), time = numeric(), change = numeric())
  }), units)

  for (profile_name in names(profile_prob)) {
    path <- patient_profiles[[profile_name]]
    if (is.null(path$unit) || length(path$unit) == 0) next
    start_offsets <- c(0, utils::head(cumsum(path$los), -1))
    profile_probability <- profile_prob[[profile_name]]

    for (path_index in seq_along(path$unit)) {
      unit_name <- path$unit[[path_index]]
      if (!unit_name %in% units) next
      start_times <- arrival_times + start_offsets[[path_index]]
      end_times <- pmin(start_times + path$los[[path_index]], sim_days)
      valid <- start_times < sim_days & end_times > start_times
      if (!any(valid)) next
      events[[unit_name]] <- rbind(
        events[[unit_name]],
        data.frame(
          unit = unit_name,
          time = c(start_times[valid], end_times[valid]),
          change = c(
            rep(profile_probability, sum(valid)),
            rep(-profile_probability, sum(valid))
          )
        )
      )
    }
  }

  dplyr::bind_rows(lapply(units, function(unit_name) {
    unit_events <- events[[unit_name]]
    if (nrow(unit_events) == 0) {
      return(data.frame(unit = unit_name, expected_peak_beds = 0))
    }
    occupancy <- unit_events |>
      dplyr::group_by(.data$time) |>
      dplyr::summarise(change = sum(.data$change), .groups = "drop") |>
      dplyr::arrange(.data$time) |>
      dplyr::mutate(expected_occupancy = cumsum(.data$change))
    data.frame(
      unit = unit_name,
      expected_peak_beds = max(occupancy$expected_occupancy, 0)
    )
  }))
}
find_n_needed <- function(capacities, duration, n_patients, sim_days,
                          patient_profiles, profile_prob, fallbacks = list(),
                          num_sims = 40,
                          search_num_sims = min(10, num_sims),
                          max_evaluations = 100, max_validation_evaluations = 10,
                          minimum_step = 1, demand_safety_factor = 1.1,
                          search_queue_tolerance = 1,
                          reliability_level = 0.80,
                          congestion_index_opt = 5, congestion_index_opt_ICU = 5,
                          workers = 1, search_seed = 2026, verbose = FALSE) {
  validate_patient_configuration(capacities, patient_profiles, profile_prob, fallbacks)
  stopifnot(
    all(c("GenMed", "ICU") %in% names(capacities)),
    num_sims >= 1,
    max_evaluations >= 2,
    max_validation_evaluations >= 1,
    search_num_sims >= 1,
    search_queue_tolerance >= 0,
    minimum_step >= 1,
    demand_safety_factor >= 1,
    is.finite(reliability_level),
    reliability_level > 0,
    reliability_level <= 1
  )

  configured_capacities <- capacities
  initial_capacities <- as.integer(ceiling(capacities[c("GenMed", "ICU")]))
  names(initial_capacities) <- c("GenMed", "ICU")
  demand <- estimate_peak_unit_demand(
    patient_profiles = patient_profiles,
    profile_prob = profile_prob,
    n_patients = n_patients,
    duration = duration,
    sim_days = sim_days
  )
  expected_peak <- stats::setNames(demand$expected_peak_beds, demand$unit)
  estimated_capacities <- as.integer(ceiling(
    expected_peak[c("GenMed", "ICU")] * demand_safety_factor
  ))
  estimated_capacities <- pmax(initial_capacities, estimated_capacities)
  names(estimated_capacities) <- names(initial_capacities)
  estimated_additional_beds <- pmax(0L, estimated_capacities - initial_capacities)
  names(estimated_additional_beds) <- names(initial_capacities)

  cache <- new.env(parent = emptyenv())
  search_evaluation_count <- 0L
  validation_evaluation_count <- 0L

  replication_chunk_size <- function(replications) {
    as.integer(max(1L, ceiling(replications / max(1L, workers))))
  }

  evaluate <- function(candidate, replications = search_num_sims,
                       stage = c("search", "validation")) {
    stage <- match.arg(stage)
    replications <- as.integer(min(replications, num_sims))
    candidate <- as.integer(candidate[c("GenMed", "ICU")])
    names(candidate) <- c("GenMed", "ICU")
    key <- paste(c(stage, candidate, replications), collapse = ":")
    if (exists(key, envir = cache, inherits = FALSE)) {
      return(get(key, envir = cache, inherits = FALSE))
    }
    if (stage == "search" && search_evaluation_count >= max_evaluations) return(NULL)
    if (stage == "validation" &&
        validation_evaluation_count >= max_validation_evaluations) return(NULL)

    if (stage == "search") {
      search_evaluation_count <<- search_evaluation_count + 1L
      stage_evaluation <- search_evaluation_count
      evaluation_seed <- search_seed
    } else {
      validation_evaluation_count <<- validation_evaluation_count + 1L
      stage_evaluation <- validation_evaluation_count
      evaluation_seed <- search_seed + 100000L
    }

    resources <- future.apply::future_lapply(
      seq_len(replications),
      function(replication_id) {
        simulation_capacities <- configured_capacities
        simulation_capacities[c("GenMed", "ICU")] <- candidate
        simulation <- run_simulation(
          capacities = simulation_capacities,
          duration = duration,
          n_patients = n_patients,
          sim_days = sim_days,
          patient_profiles = patient_profiles,
          profile_prob = profile_prob,
          fallbacks = fallbacks
        )
        monitored_resources <- get_hospital_mon_resources(
          simulation,
          include_resources = c("GenMed", "ICU")
        )
        resource_summary <- monitored_resources |>
          dplyr::group_by(.data$resource) |>
          dplyr::summarise(
            maximum_occupied = safe_max(.data$server),
            maximum_queue = safe_max(.data$queue),
            .groups = "drop"
          )
        target_summary <- dplyr::tibble(resource = c("GenMed", "ICU")) |>
          dplyr::left_join(resource_summary, by = "resource")
        if (anyNA(target_summary$maximum_queue)) {
          stop(
            "Target resource monitoring is incomplete for optimization.",
            call. = FALSE
          )
        }
        dplyr::mutate(target_summary, replication = replication_id)
      },
      future.seed = evaluation_seed,
      future.chunk.size = replication_chunk_size(replications)
    ) |>
      dplyr::bind_rows()

    maximum_occupancy <- stats::setNames(
      vapply(c("GenMed", "ICU"), function(unit_name) {
        unit_rows <- resources[resources$resource == unit_name, , drop = FALSE]
        if (nrow(unit_rows) == 0) return(0L)
        as.integer(ceiling(safe_max(unit_rows$maximum_occupied)))
      }, integer(1)),
      c("GenMed", "ICU")
    )

    maximum_queues <- resources |>
      dplyr::select(dplyr::all_of(c("replication", "resource", "maximum_queue")))
    queue_for <- function(unit_name) {
      unit_rows <- maximum_queues[maximum_queues$resource == unit_name, , drop = FALSE]
      replications_found <- unit_rows$replication
      expected_replications <- seq_len(replications)
      missing_replications <- setdiff(expected_replications, replications_found)
      unexpected_replications <- setdiff(replications_found, expected_replications)

      if (anyDuplicated(replications_found) ||
          length(missing_replications) > 0 ||
          length(unexpected_replications) > 0) {
        stop(
          sprintf(
            paste0(
              "Invalid replication monitoring for %s: expected %d unique replications; ",
              "found %d. Missing: %s. Unexpected: %s."
            ),
            unit_name,
            replications,
            length(unique(replications_found)),
            if (length(missing_replications) == 0) "none" else paste(missing_replications, collapse = ", "),
            if (length(unexpected_replications) == 0) "none" else paste(unexpected_replications, collapse = ", ")
          ),
          call. = FALSE
        )
      }

      unit_rows$maximum_queue[match(expected_replications, replications_found)]
    }
    queue_matrix <- data.frame(
      replication = seq_len(replications),
      GenMed = queue_for("GenMed"),
      ICU = queue_for("ICU"),
      check.names = FALSE
    )

    queue_tolerance <- if (stage == "search") search_queue_tolerance else 0
    thresholds <- c(
      GenMed = congestion_index_opt + queue_tolerance,
      ICU = congestion_index_opt_ICU + queue_tolerance
    )
    unit_pass <- data.frame(
      GenMed = queue_matrix$GenMed <= thresholds[["GenMed"]],
      ICU = queue_matrix$ICU <= thresholds[["ICU"]]
    )
    joint_pass <- unit_pass$GenMed & unit_pass$ICU
    required_successes <- ceiling(reliability_level * replications)
    unit_successful_replications <- c(
      GenMed = sum(unit_pass$GenMed),
      ICU = sum(unit_pass$ICU)
    )
    unit_reliability <- unit_successful_replications / replications

    result <- list(
      capacities = candidate,
      maximum_occupancy = maximum_occupancy,
      queues = c(
        GenMed = safe_median(queue_matrix$GenMed),
        ICU = safe_median(queue_matrix$ICU)
      ),
      unit_reliability = unit_reliability,
      reliability = safe_mean(as.numeric(joint_pass)),
      successful_replications = sum(joint_pass),
      unit_successful_replications = unit_successful_replications,
      required_successes = required_successes,
      replications = replications,
      thresholds = thresholds,
      # Each unit must independently reach the reliability target. Joint
      # reliability is retained as a diagnostic because failures may occur in
      # different replications.
      passes = all(unit_successful_replications >= required_successes)
    )
    assign(key, result, envir = cache)

    if (verbose) {
      cat(sprintf(
        paste0(
          "%s evaluation %d: GenMed=%d (median max %.2f, %.0f%% pass), ",
          "ICU=%d (median max %.2f, %.0f%% pass), joint diagnostic %.0f%%, pass=%s\n"
        ),
        tools::toTitleCase(stage), stage_evaluation,
        candidate[["GenMed"]], result$queues[["GenMed"]],
        100 * result$unit_reliability[["GenMed"]],
        candidate[["ICU"]], result$queues[["ICU"]],
        100 * result$unit_reliability[["ICU"]],
        100 * result$reliability, result$passes
      ))
    }
    result
  }

  total_added <- function(candidate) {
    sum(candidate[c("GenMed", "ICU")] - initial_capacities)
  }

  target_units <- c("GenMed", "ICU")
  total_generated_patients <- as.integer(ceiling(duration * n_patients))
  unlimited_capacity_value <- max(500L, total_generated_patients)
  unlimited_simulation_used <- FALSE
  unlimited_max_occupancy <- stats::setNames(
    rep(NA_integer_, length(configured_capacities)),
    names(configured_capacities)
  )
  unlimited_reference_capacities <- initial_capacities
  safety_capacities <- pmax(
    initial_capacities,
    stats::setNames(rep(total_generated_patients, length(target_units)), target_units)
  )
  names(safety_capacities) <- target_units
  frontier_complete <- FALSE
  validation_refinement_complete <- FALSE
  current_capacity_validated <- FALSE
  current_validation <- NULL

  failing_units_for <- function(evaluation_result) {
    if (is.null(evaluation_result)) return(target_units)
    names(evaluation_result$unit_reliability)[
      evaluation_result$unit_reliability < reliability_level
    ]
  }

  estimate_unlimited_demand <- function(replications = num_sims) {
    observed <- future.apply::future_lapply(
      seq_len(replications),
      function(replication_id) {
        simulation_capacities <- stats::setNames(
          rep(unlimited_capacity_value, length(configured_capacities)),
          names(configured_capacities)
        )
        simulation <- run_simulation(
          capacities = simulation_capacities,
          duration = duration,
          n_patients = n_patients,
          sim_days = sim_days,
          patient_profiles = patient_profiles,
          profile_prob = profile_prob,
          fallbacks = fallbacks
        )
        simmer::get_mon_resources(simulation) |>
          dplyr::filter(
            !startsWith(.data$resource, logical_queue_prefix),
            .data$resource %in% names(configured_capacities)
          ) |>
          dplyr::group_by(.data$resource) |>
          dplyr::summarise(
            maximum_occupied = as.integer(ceiling(safe_max(.data$server))),
            .groups = "drop"
          )
      },
      future.seed = search_seed + 100000L,
      future.chunk.size = replication_chunk_size(replications)
    ) |>
      dplyr::bind_rows() |>
      dplyr::group_by(.data$resource) |>
      dplyr::summarise(
        maximum_occupied = max(.data$maximum_occupied),
        .groups = "drop"
      )

    maximum_occupancy <- stats::setNames(
      rep(0L, length(configured_capacities)),
      names(configured_capacities)
    )
    maximum_occupancy[observed$resource] <- observed$maximum_occupied
    maximum_occupancy
  }
  # Always evaluate current capacity first. If it passes the search screen,
  # verify it against the exact validation thresholds before estimating demand.
  capacities <- initial_capacities
  result <- evaluate(capacities)
  if (!is.null(result) && result$passes) {
    current_validation <- evaluate(
      capacities,
      replications = num_sims,
      stage = "validation"
    )
    if (!is.null(current_validation) && current_validation$passes) {
      result <- current_validation
      current_capacity_validated <- TRUE
      frontier_complete <- TRUE
      validation_refinement_complete <- TRUE
    }
  }

  if (!current_capacity_validated) {
    reference_result <- if (!is.null(current_validation) &&
                            !current_validation$passes) {
      current_validation
    } else {
      result
    }
    active_units <- failing_units_for(reference_result)
    if (length(active_units) == 0) active_units <- target_units

    # Estimate unconstrained primary demand with enough beds to exceed the
    # total number of arrivals. This is a reference, not a hard ceiling,
    # because constrained upstream units can route additional patients through
    # fallbacks to GenMed or ICU.
    unlimited_max_occupancy <- estimate_unlimited_demand()
    unlimited_simulation_used <- TRUE
    unlimited_reference_capacities <- pmax(
      initial_capacities,
      as.integer(unlimited_max_occupancy[target_units])
    )
    names(unlimited_reference_capacities) <- target_units

    if (verbose) {
      demand_text <- paste0(
        names(unlimited_max_occupancy),
        "=",
        unlimited_max_occupancy,
        collapse = ", "
      )
      cat(sprintf(
        "Unlimited-capacity demand (%d beds per unit): %s\n",
        unlimited_capacity_value,
        demand_text
      ))
      cat(sprintf(
        "Fallback-safe search ceiling: GenMed=%d, ICU=%d\n",
        safety_capacities[["GenMed"]],
        safety_capacities[["ICU"]]
      ))
    }

    capacities <- initial_capacities
    result <- reference_result
    growth_steps <- stats::setNames(
      rep(as.integer(minimum_step), length(target_units)),
      target_units
    )

    # Grow only units that currently fail. The increments double after each
    # evaluation and are clipped at demand observed with unlimited capacity.
    while (!is.null(result) && !result$passes) {
      failing_units <- failing_units_for(result)
      active_units <- union(active_units, failing_units)
      failing_units <- intersect(
        failing_units,
        target_units[capacities[target_units] < safety_capacities[target_units]]
      )
      if (length(failing_units) == 0) break

      trial <- capacities
      trial[failing_units] <- pmin(
        safety_capacities[failing_units],
        trial[failing_units] + growth_steps[failing_units]
      )
      if (identical(as.integer(trial), as.integer(capacities))) break

      trial_result <- evaluate(trial)
      if (is.null(trial_result)) break
      capacities <- trial
      result <- trial_result
      growth_steps[failing_units] <- pmin(
        pmax(0L, safety_capacities[failing_units] - initial_capacities[failing_units]),
        pmax(minimum_step, 2L * growth_steps[failing_units])
      )
    }
    # Reduce each resource with bounded binary searches, then inspect a small
    # two-dimensional neighborhood for useful GenMed/ICU trade-offs.
    if (!is.null(result) && result$passes) {
      refine_unit <- function(unit_name, current_capacities, current_result) {
        lower <- initial_capacities[[unit_name]]
        upper <- current_capacities[[unit_name]]
        best_capacities <- current_capacities
        best_result <- current_result
        complete <- TRUE

        while (lower < upper) {
          midpoint <- floor((lower + upper) / 2)
          trial <- best_capacities
          trial[[unit_name]] <- midpoint
          trial_result <- evaluate(trial)
          if (is.null(trial_result)) {
            complete <- FALSE
            break
          }
          if (trial_result$passes) {
            best_capacities <- trial
            best_result <- trial_result
            upper <- midpoint
          } else {
            lower <- midpoint + 1L
          }
        }

        list(
          capacities = best_capacities,
          result = best_result,
          complete = complete
        )
      }

      refinement_status <- logical()
      refinement_order <- c("ICU", "GenMed", "ICU")
      for (unit_name in refinement_order) {
        refined <- refine_unit(unit_name, capacities, result)
        capacities <- refined$capacities
        result <- refined$result
        refinement_status <- c(refinement_status, refined$complete)
        if (!refined$complete) break
      }

      local_complete <- all(refinement_status)
      if (local_complete) {
        local_radius <- 1L
        genmed_values <- seq.int(
          max(initial_capacities[["GenMed"]], capacities[["GenMed"]] - local_radius),
          min(safety_capacities[["GenMed"]], capacities[["GenMed"]] + local_radius)
        )
        icu_values <- seq.int(
          max(initial_capacities[["ICU"]], capacities[["ICU"]] - local_radius),
          min(safety_capacities[["ICU"]], capacities[["ICU"]] + local_radius)
        )
        candidates <- base::expand.grid(
          GenMed = genmed_values,
          ICU = icu_values,
          KEEP.OUT.ATTRS = FALSE
        ) |>
          dplyr::mutate(
            total_added = .data$GenMed - initial_capacities[["GenMed"]] +
              .data$ICU - initial_capacities[["ICU"]],
            distance = abs(.data$GenMed - capacities[["GenMed"]]) +
              abs(.data$ICU - capacities[["ICU"]])
          ) |>
          dplyr::filter(.data$total_added <= total_added(capacities)) |>
          dplyr::arrange(.data$total_added, .data$distance)

        for (row_index in seq_len(nrow(candidates))) {
          trial <- c(
            GenMed = as.integer(candidates$GenMed[[row_index]]),
            ICU = as.integer(candidates$ICU[[row_index]])
          )
          if (identical(as.integer(trial), as.integer(capacities))) next
          trial_result <- evaluate(trial)
          if (is.null(trial_result)) {
            local_complete <- FALSE
            break
          }
          if (trial_result$passes && total_added(trial) < total_added(capacities)) {
            capacities <- trial
            result <- trial_result
          }
        }
      }
      frontier_complete <- all(refinement_status) && local_complete
    }

    # Validation uses the full replication bank and exact queue limits. Failed
    # units grow exponentially without exceeding unlimited-capacity demand.
    validation_result <- NULL
    if (!is.null(result) && result$passes) {
      validation_result <- evaluate(
        capacities,
        replications = num_sims,
        stage = "validation"
      )
      validation_growth_steps <- stats::setNames(
        rep(as.integer(minimum_step), length(target_units)),
        target_units
      )
      # A value one below the initial capacity is a conceptual lower boundary
      # when validation has not yet observed a failure for that resource.
      validation_failure_floor <- initial_capacities - 1L
      validation_expanded <- FALSE

      while (!is.null(validation_result) && !validation_result$passes &&
             validation_evaluation_count < max_validation_evaluations) {
        failing_units <- failing_units_for(validation_result)
        validation_failure_floor[failing_units] <- pmax(
          validation_failure_floor[failing_units],
          capacities[failing_units]
        )
        failing_units <- intersect(
          failing_units,
          target_units[capacities[target_units] < safety_capacities[target_units]]
        )
        if (length(failing_units) == 0) break

        trial <- capacities
        trial[failing_units] <- pmin(
          safety_capacities[failing_units],
          trial[failing_units] + validation_growth_steps[failing_units]
        )
        if (identical(as.integer(trial), as.integer(capacities))) break

        capacities <- trial
        validation_expanded <- TRUE
        validation_result <- evaluate(
          capacities,
          replications = num_sims,
          stage = "validation"
        )
        validation_growth_steps[failing_units] <- pmin(
          safety_capacities[failing_units] - initial_capacities[failing_units],
          pmax(minimum_step, 2L * validation_growth_steps[failing_units])
        )
      }

      validation_refinement_complete <-
        !is.null(validation_result) && validation_result$passes
      if (validation_refinement_complete && !validation_expanded) {
        # Search refinement usually leaves a near-boundary candidate. When it
        # validates immediately, test one-bed reductions before using a wider
        # interval search.
        improved <- TRUE
        while (improved && validation_refinement_complete) {
          improved <- FALSE
          for (unit_name in target_units) {
            if (capacities[[unit_name]] <= initial_capacities[[unit_name]]) next
            trial <- capacities
            trial[[unit_name]] <- trial[[unit_name]] - 1L
            trial_result <- evaluate(
              trial,
              replications = num_sims,
              stage = "validation"
            )
            if (is.null(trial_result)) {
              validation_refinement_complete <- FALSE
              break
            }
            if (trial_result$passes) {
              capacities <- trial
              validation_result <- trial_result
              improved <- TRUE
              break
            }
          }
        }
      } else if (validation_refinement_complete) {
        # After validation expansion, the last failing value and first passing
        # value form a narrow bracket that can be refined efficiently.
        for (unit_name in c("ICU", "GenMed", "ICU")) {
          lower <- validation_failure_floor[[unit_name]]
          upper <- capacities[[unit_name]]

          while (upper - lower > 1L) {
            if (validation_evaluation_count >= max_validation_evaluations) {
              validation_refinement_complete <- FALSE
              break
            }
            midpoint <- as.integer(floor((lower + upper) / 2))
            trial <- capacities
            trial[[unit_name]] <- midpoint
            trial_result <- evaluate(
              trial,
              replications = num_sims,
              stage = "validation"
            )
            if (is.null(trial_result)) {
              validation_refinement_complete <- FALSE
              break
            }
            if (trial_result$passes) {
              capacities <- trial
              validation_result <- trial_result
              upper <- midpoint
            } else {
              lower <- midpoint
              validation_failure_floor[[unit_name]] <- max(
                validation_failure_floor[[unit_name]],
                midpoint
              )
            }
          }
          if (!validation_refinement_complete) break
        }
      }
      result <- validation_result
    }
  }

  if (is.null(result)) {
    result <- list(
      queues = c(GenMed = NA_real_, ICU = NA_real_),
      unit_reliability = c(GenMed = NA_real_, ICU = NA_real_),
      reliability = NA_real_,
      passes = FALSE
    )
  }

  list(
    avg_congestion_index_GenMed = result$queues[["GenMed"]],
    avg_congestion_index_ICU = result$queues[["ICU"]],
    reliability_GenMed = result$unit_reliability[["GenMed"]],
    reliability_ICU = result$unit_reliability[["ICU"]],
    joint_reliability = result$reliability,
    reliability_level = reliability_level,
    N_added = capacities[["GenMed"]] - initial_capacities[["GenMed"]],
    N_added_ICU = capacities[["ICU"]] - initial_capacities[["ICU"]],
    GenMed_N = capacities[["GenMed"]],
    ICU_N = capacities[["ICU"]],
    estimated_GenMed_N = estimated_capacities[["GenMed"]],
    estimated_ICU_N = estimated_capacities[["ICU"]],
    expected_peak_GenMed = expected_peak[["GenMed"]],
    expected_peak_ICU = expected_peak[["ICU"]],
    estimated_additional_GenMed = estimated_additional_beds[["GenMed"]],
    estimated_additional_ICU = estimated_additional_beds[["ICU"]],
    unlimited_simulation_used = unlimited_simulation_used,
    unlimited_capacity_value = unlimited_capacity_value,
    unlimited_max_occupancy_GenMed = unlimited_max_occupancy[["GenMed"]],
    unlimited_max_occupancy_ICU = unlimited_max_occupancy[["ICU"]],
    unlimited_reference_GenMed = unlimited_reference_capacities[["GenMed"]],
    unlimited_reference_ICU = unlimited_reference_capacities[["ICU"]],
    safety_capacity_GenMed = safety_capacities[["GenMed"]],
    safety_capacity_ICU = safety_capacities[["ICU"]],
    search_evaluations = search_evaluation_count,
    validation_evaluations = validation_evaluation_count,
    evaluations = search_evaluation_count + validation_evaluation_count,
    converged = isTRUE(result$passes),
    refinement_complete = frontier_complete && validation_refinement_complete,
    search_complete = isTRUE(result$passes)
  )
}