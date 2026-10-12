# Prints the LaTeX table of surge patient profiles (care pathway with mean
# length of stay per unit, and arrival proportion) straight from a profile
# configuration in R/shared/profiles_deloitte.R, for manual copy into main.tex.
# Does not write to main.tex itself.
#
# Run via `Rscript paper/generate_surge_profile_table.R` from anywhere (the
# working directory is derived from the script's own path); if sourced
# interactively instead, first setwd() to the project root yourself.
# To tabulate another profile source, change `config` below (e.g.
# uc_davis_profile_config() or any other *_profile_config()).
script_args <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_args[grepl("^--file=", script_args)])
if (length(script_file) == 1L) setwd(dirname(dirname(normalizePath(script_file))))

source("R/shared/profiles_deloitte.R")

# Mean LOS without trailing zeros (3 -> "3", 0.0598 -> "0.06").
format_los <- function(x) format(round(x, 2), trim = TRUE, drop0trailing = TRUE, scientific = FALSE)

# "ICU (1) $\rightarrow$ GenMed (4)"; a profile with no unit is ambulatory.
profile_care_path <- function(profile) {
  if (is.null(profile$unit) || length(profile$unit) == 0) return("Ambulatory (no inpatient bed)")
  paste0(profile$unit, " (", vapply(profile$los, format_los, character(1)), ")",
         collapse = " $\\rightarrow$ ")
}

# One row per profile, in the configuration's own order; proportions are the
# profile_prob values in percent (they sum to 100). The profile column shows
# config$profile_labels when the configuration defines them, and otherwise the
# profile key with its underscores escaped for LaTeX.
surge_profile_table <- function(config) {
  stopifnot(setequal(names(config$patient_profiles), names(config$profile_prob)))
  profile_names <- names(config$patient_profiles)
  display_names <- if (is.null(config$profile_labels)) {
    gsub("_", "\\\\_", profile_names)
  } else {
    unname(config$profile_labels[profile_names])
  }
  data.frame(
    profile = display_names,
    care_path = vapply(config$patient_profiles[profile_names], profile_care_path, character(1)),
    proportion = 100 * unname(config$profile_prob[profile_names]),
    stringsAsFactors = FALSE
  )
}

surge_profile_table_latex <- function(table, caption, label) {
  rows <- sprintf("%s & %s & %.2f \\\\", table$profile, table$care_path, table$proportion)
  c("\\begin{table}[ht]",
    sprintf("\\tbl{%s}", caption),
    "{\\begin{tabular}{lcc} \\toprule",
    "\\textbf{Profile} & \\textbf{Care Path (LOS in days)} & \\textbf{Proportion (\\%)} \\\\",
    "\\midrule",
    rows,
    "\\bottomrule",
    "\\end{tabular}}",
    sprintf("\\label{%s}", label),
    "\\end{table}")
}

config <- deloitte_test_profile_config()
profile_table <- surge_profile_table(config)
stopifnot(abs(sum(profile_table$proportion) - 100) < 1e-6)

cat(surge_profile_table_latex(
  profile_table,
  caption = paste("Surge patient profiles used in the scenario analyses, including arrival",
                  "proportions and care pathways with mean length of stay (LOS, days)."),
  label = "tab:surge_profiles"
), sep = "\n")
