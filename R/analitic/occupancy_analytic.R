# Simulation-free occupancy under unlimited bed capacity.
#
# Assumptions (mirroring the unlimited-capacity DES, see unlimited_demand_replication()):
#  * No unit ever fills, so nobody queues, boards or uses a fallback: a patient
#    simply spends the sampled LOS in each pathway unit, back to back.
#  * LOS per step is log-normal with the profile's mean and CV (cv defaults to
#    0.1 as in make_service_times() when a profile carries none).
#  * Civilian streams start at absolute time 0 with an empty hospital (warm-up),
#    Poisson or evenly spaced; the surge starts at absolute time `warmup_days`.
#  * Surge profiles are drawn independently with probability `profile_prob`.
#
# Consequence: for a Poisson stream, the number of patients in a unit at time t
# is Poisson (M/G/infinity); for evenly spaced arrivals it is a Poisson-binomial.
# Civilian and surge counts are independent, so their pmfs are convolved.
# Numerical approximations: log-normal stays are discretised on a grid of width
# `step` days (mass placed at cell midpoints); evenly spaced arrivals read the
# age-probability curve by linear interpolation.
#
# Not modelled analytically: the distribution of the maximum over time. Peaks
# are reported as the maximum over time of the pointwise mean and quantiles.

analytic_default_cv <- 0.1

analytic_step_cv <- function(profile) {
  if (is.null(profile$cv)) rep(analytic_default_cv, length(profile$los)) else profile$cv
}

# Probability mass of one log-normal stay in cells ((i - 1) * step, i * step].
analytic_stay_pmf <- function(mean_los, cv, step, n_cells) {
  sigma <- sqrt(log(1 + cv^2))
  meanlog <- log(mean_los) - sigma^2 / 2
  diff(stats::plnorm((0:n_cells) * step, meanlog, sigma))
}

# F[m + 1, k + 1] = P(first k stays have ended by age m * step); column 1 is k = 0.
analytic_pathway_cdfs <- function(profile, step, n_cells) {
  n_steps <- length(profile$unit)
  cv <- analytic_step_cv(profile)
  cdf <- matrix(0, n_cells + 1L, n_steps + 1L)
  cdf[, 1] <- 1
  mass <- NULL
  for (k in seq_len(n_steps)) {
    stay <- analytic_stay_pmf(profile$los[[k]], cv[[k]], step, n_cells)
    # Position s + 1 holds P(sum of the k zero-based cell indices = s).
    mass <- if (is.null(mass)) stay else
      pmax(stats::convolve(mass, rev(stay), type = "open")[seq_len(n_cells)], 0)
    mass <- c(mass, numeric(n_cells - length(mass)))
    cumulative <- cumsum(mass)
    last_cell <- floor(0:n_cells - k / 2)  # cell sums s with (s + k / 2) * step <= age
    cdf[, k + 1L] <- ifelse(last_cell < 0, 0, cumulative[pmax(last_cell, 0) + 1L])
  }
  cdf
}

# P(patient with this profile occupies each unit at age m * step), per unit column.
analytic_unit_probability <- function(profile, units, step, n_cells) {
  zero <- matrix(0, n_cells + 1L, length(units), dimnames = list(NULL, units))
  if (is.null(profile$unit) || length(profile$unit) == 0L) return(zero)
  cdf <- analytic_pathway_cdfs(profile, step, n_cells)
  n_steps <- length(profile$unit)
  by_step <- cdf[, seq_len(n_steps), drop = FALSE] - cdf[, seq_len(n_steps) + 1L, drop = FALSE]
  for (unit in intersect(units, profile$unit)) {
    zero[, unit] <- rowSums(by_step[, profile$unit == unit, drop = FALSE])
  }
  zero
}

# Cumulative trapezoid integral of each column over age.
analytic_cumulative_integral <- function(probability, step) {
  rbind(0, apply((probability[-1, , drop = FALSE] + probability[-nrow(probability), , drop = FALSE]) / 2,
                 2, cumsum) * step)
}

# One arrival stream: Poisson (`rate` per day) or evenly spaced, active on
# [start, end) in absolute days, with age-probability matrix `probability`.
analytic_make_stream <- function(kind, population, rate, start, end, profiles, weights, units,
                                 step, n_cells) {
  probability <- Reduce(`+`, Map(function(profile, weight) {
    weight * analytic_unit_probability(profile, units, step, n_cells)
  }, profiles, weights))
  length_days <- end - start
  list(kind = kind, population = population, rate = rate, start = start, length = length_days,
       probability = probability,
       integral = if (kind == "poisson") analytic_cumulative_integral(probability, step),
       ages = if (kind == "even") {
         arrivals <- (seq_len(ceiling(length_days * rate)) - 1) / rate
         arrivals[arrivals < length_days]
       })
}

analytic_build_streams <- function(patient_profiles, profile_prob, rate, duration,
                                   surge_process, baseline, warmup, horizon, units,
                                   step, n_cells) {
  streams <- list()
  if (!is.null(baseline) && isTRUE(baseline$enabled)) {
    process <- if (is.null(baseline$arrival_process)) "poisson" else baseline$arrival_process
    for (name in names(baseline$profiles)) {
      streams[[length(streams) + 1L]] <- analytic_make_stream(
        if (process == "poisson") "poisson" else "even", "civilian",
        baseline$arrival_rates[[name]], 0, horizon,
        list(baseline$profiles[[name]]), 1, units, step, n_cells)
    }
  }
  if (rate > 0 && duration > 0) {
    streams[[length(streams) + 1L]] <- analytic_make_stream(
      surge_process, "surge", rate, warmup, warmup + duration,
      patient_profiles, profile_prob[names(patient_profiles)], units, step, n_cells)
  }
  streams
}

# Poisson-binomial pmf (DP) convolved with a Poisson pmf; support 0..kmax.
analytic_occupancy_pmf <- function(poisson_mean, bernoulli_q) {
  bernoulli_q <- bernoulli_q[bernoulli_q > 1e-12]
  total_mean <- poisson_mean + sum(bernoulli_q)
  total_variance <- poisson_mean + sum(bernoulli_q * (1 - bernoulli_q))
  kmax <- ceiling(total_mean + 10 * sqrt(total_variance) + 10)
  binomial_part <- c(1, numeric(kmax))
  for (q in bernoulli_q) {
    binomial_part <- binomial_part * (1 - q) + c(0, binomial_part[-(kmax + 1L)]) * q
  }
  pmf <- if (poisson_mean > 0) {
    pmax(stats::convolve(binomial_part, rev(stats::dpois(0:kmax, poisson_mean)),
                         type = "open")[seq_len(kmax + 1L)], 0)
  } else binomial_part
  pmf / sum(pmf)
}

# Tidy table: one row per resource and output time (days from surge onset).
analytic_occupancy_trajectory <- function(streams, units, warmup, horizon, step,
                                          output_step, levels) {
  stopifnot(abs(warmup / step - round(warmup / step)) < 1e-8,
            abs(output_step / step - round(output_step / step)) < 1e-8)
  grid_age <- (0:round(horizon / step)) * step
  absolute <- seq(0, horizon, by = output_step)
  rows <- lapply(units, function(unit) {
    per_time <- lapply(absolute, function(s) {
      poisson_mean <- 0
      bernoulli_q <- numeric()
      surge_mean <- 0
      for (stream in streams) {
        tau <- s - stream$start
        if (tau < 0) next
        if (stream$kind == "poisson") {
          upper <- round(tau / step) + 1L
          lower <- round(max(0, tau - stream$length) / step) + 1L
          contribution <- stream$rate * (stream$integral[upper, unit] - stream$integral[lower, unit])
          poisson_mean <- poisson_mean + contribution
        } else {
          ages <- tau - stream$ages
          q <- stats::approx(grid_age, stream$probability[, unit], xout = ages[ages >= 0])$y
          bernoulli_q <- c(bernoulli_q, q)
          contribution <- sum(q)
        }
        if (stream$population == "surge") surge_mean <- surge_mean + contribution
      }
      pmf <- analytic_occupancy_pmf(poisson_mean, bernoulli_q)
      support <- seq_along(pmf) - 1L
      cdf <- cumsum(pmf)
      mean_all <- sum(support * pmf)
      quantiles <- vapply(levels, function(level) support[which(cdf >= level)[1]], numeric(1))
      stats::setNames(c(mean_all, sqrt(sum((support - mean_all)^2 * pmf)), surge_mean, quantiles),
                      c("mean", "sd", "mean_surge", if (length(levels)) paste0("q", levels)))
    })
    cbind(data.frame(resource = unit, time = absolute - warmup),
          as.data.frame(do.call(rbind, per_time)))
  })
  dplyr::bind_rows(rows)
}

# Peak over time of the pointwise mean and quantiles, per resource (tidy, long).
analytic_peak_summary <- function(trajectory, from_time = -Inf) {
  statistics <- setdiff(names(trajectory), c("resource", "time", "mean_surge"))
  rows <- trajectory[trajectory$time >= from_time, , drop = FALSE]
  dplyr::bind_rows(lapply(split(rows, rows$resource), function(unit_rows) {
    dplyr::bind_rows(lapply(statistics, function(statistic) {
      peak_index <- which.max(unit_rows[[statistic]])
      data.frame(resource = unit_rows$resource[[1]], statistic = paste0("peak_", statistic),
                 peak_value = unit_rows[[statistic]][[peak_index]],
                 time_of_peak = unit_rows$time[[peak_index]])
    }))
  }))
}

# Entry point. `baseline` is the study's civilian configuration (profiles, arrival_rates,
# warmup_days, arrival_process); `surge_process` is "even" or "poisson".
analytic_unlimited_demand <- function(patient_profiles, profile_prob, rate, duration, sim_days,
                                      baseline = NULL, surge_process = "even", units = NULL,
                                      step = 0.01, output_step = 0.1,
                                      levels = c(0.05, 0.5, 0.95, 0.99)) {
  warmup <- if (!is.null(baseline) && isTRUE(baseline$enabled)) baseline$warmup_days else 0
  horizon <- warmup + sim_days
  if (is.null(units)) {
    units <- unique(c(unlist(lapply(patient_profiles, `[[`, "unit")),
                      unlist(lapply(baseline$profiles, `[[`, "unit"))))
  }
  n_cells <- round(horizon / step)
  streams <- analytic_build_streams(patient_profiles, profile_prob, rate, duration,
                                    surge_process, baseline, warmup, horizon, units,
                                    step, n_cells)
  trajectory <- analytic_occupancy_trajectory(streams, units, warmup, horizon, step,
                                              output_step, levels)
  list(trajectory = trajectory,
       peaks = analytic_peak_summary(trajectory),
       parameters = data.frame(rate = rate, duration = duration, sim_days = sim_days,
                               warmup_days = warmup, surge_process = surge_process,
                               step = step, output_step = output_step))
}
