# Names are display labels; values are stable internal resource identifiers.
hospital_profile_units <- c(
  "ED" = "ED",
  "IP Surge" = "Surge",
  "General Medicine" = "GenMed",
  "ICU" = "ICU",
  "Burn Bed" = "BurnBed",
  "Cardiology" = "Cardiology",
  "Physical Medicine" = "PhysicalMed",
  "Psychiatric" = "Psychiatric",
  "Transitional Care" = "TransitionalCare"
)
# ED beds are modeled as practically unlimited (hallway/chair capacity is
# elastic in practice): a large finite value, not literal Inf, because
# validate_patient_configuration() and the resource setup require finite
# capacities throughout. Boarding is what actually constrains an ED patient,
# not this bed count. 10,000 beds is far above any simulated ED census, so the
# ED never fills.
unlimited_capacity_placeholder <- 10000L
# ED is always part of the hospital with the placeholder capacity above; it is
# never shown as an editable unit or bed count. Civilian pathways start there;
# it is not offered in surge trajectories or fallback rules.
internal_hospital_units <- c(ED = unlimited_capacity_placeholder)
editable_hospital_units <- hospital_profile_units[!hospital_profile_units %in% names(internal_hospital_units)]

# Default bed inputs --------------------------------------------------------------
# Predefined sources and unit defaults are total beds. With routine civilian
# flow enabled, the warm-up fills the hospital, so total beds are shown as-is.
# Without it the hospital starts empty, so the default shown is the beds still
# available to the surge under an assumed baseline occupancy (95% for GenMed
# and ICU, 50% for every other unit), rounded up to whole beds. These are only
# starting values; whatever the user types is kept.
assumed_baseline_occupancy <- c(GenMed = 0.95, ICU = 0.95)
other_unit_baseline_occupancy <- 0.5

available_beds_default <- function(unit, total_beds) {
  occupancy <- if (unit %in% names(assumed_baseline_occupancy)) {
    assumed_baseline_occupancy[[unit]]
  } else {
    other_unit_baseline_occupancy
  }
  # Rounding to 6 decimals first stops floating-point noise (80 * 0.05 =
  # 4.000000000000001) from adding a spurious bed in ceiling().
  ceiling(round(total_beds * (1 - occupancy), 6))
}

capacity_label <- function(civilian_enabled) {
  if (isTRUE(civilian_enabled)) "Total beds" else "Total available beds"
}

# Predefined hospitals ("Use predefined hospital" in Hospital units). Selecting
# one loads its units, beds and fallbacks plus its NDMS-based surge profiles and
# arrival percentages. Values are the stable internal source IDs used by saved
# configs and the civilian profile files.
predefined_hospitals <- c(
  "Regional hospital" = "regional_hospital_test",
  "Tertiary hospital" = "tertiary_hospital_test",
  "Community acute-care hospital" = "community_hospital_test"
)

# Total beds of each predefined hospital (assumed scenario capacities).
predefined_hospital_beds <- list(
  regional_hospital_test = c(GenMed = 240, ICU = 20, Cardiology = 12,
                             PhysicalMed = 12, TransitionalCare = 20),
  tertiary_hospital_test = c(GenMed = 400, ICU = 72, BurnBed = 12, Cardiology = 18,
                             PhysicalMed = 24, Psychiatric = 20, TransitionalCare = 24),
  community_hospital_test = c(GenMed = 80, ICU = 12, TransitionalCare = 12)
)

# Initial manual hospital, also restored when leaving a predefined hospital.
# Capacities are total beds (shown as available beds without civilian flow).
manual_hospital_defaults <- list(
  units = c("GenMed", "ICU"),
  capacities = c(GenMed = 15, ICU = 7),
  patient_profiles = list(),
  profile_prob = numeric(),
  fallbacks = list()
)

# NDMS-based profiles are selected by deloitte_profiles_for_hospital(): pathways
# using a unit the hospital lacks are excluded (not rerouted).
ndms_wia_prob <- 0.67

predefined_hospital_config <- function(hospital_id) {
  if (length(hospital_id) != 1L || !hospital_id %in% names(predefined_hospital_beds)) return(NULL)
  deloitte_profiles_for_hospital(
    predefined_hospital_beds[[hospital_id]], wia_prob = ndms_wia_prob, source = hospital_id,
    source_label = names(predefined_hospitals)[predefined_hospitals == hospital_id]
  )
}

# Predefined civilian profiles (data/) for each predefined hospital. Other
# hospitals have none, so the civilian "Use predefined" option is not offered.
civilian_profile_files <- c(
  regional_hospital_test = "baseline_civilian_profiles_regional.csv",
  tertiary_hospital_test = "baseline_civilian_profiles_tertiary.csv",
  community_hospital_test = "baseline_civilian_profiles_community.csv"
)

civilian_profile_file <- function(hospital_id) {
  if (length(hospital_id) != 1L || !hospital_id %in% names(civilian_profile_files)) return(NULL)
  file.path("data", civilian_profile_files[[hospital_id]])
}

# LOS variability -------------------------------------------------------------
# Users enter a standard deviation (SD, days) per pathway step. The engine keeps
# its log-normal parameterized by mean and CV (make_service_times()), and a
# log-normal with arithmetic mean m and SD s is exactly the one with CV = s / m,
# so SD is converted to CV once, when a profile is saved or imported.
engine_default_cv <- eval(formals(make_service_times)$cv)

# The CV the engine actually uses for each step; profiles without a cv field
# fall back to make_service_times()'s default, not default_cv_for_unit().
profile_step_cv <- function(profile) {
  if (!length(profile$unit)) return(NULL)
  if (!is.null(profile$cv)) profile$cv else rep(engine_default_cv, length(profile$los))
}

profile_step_sd <- function(profile) {
  if (!length(profile$unit)) return(NULL)
  profile_step_cv(profile) * profile$los
}

# Blank (NA) SDs use the default variability rule (CV 1 for ICU, 0.24 otherwise).
step_cv_from_sd <- function(units, los, sd) {
  cv <- sd / los
  missing <- is.na(sd)
  cv[missing] <- default_cv_for_unit(units[missing])
  cv
}

format_steps <- function(values) {
  if (!length(values)) return("-")
  paste(signif(values, 4), collapse = " -> ")
}

# Pathway step editor shared by the surge and civilian profile editors. Input
# IDs are "<field>_<form version>_<step>"; bumping the version discards stale
# browser values. Typed values win over the draft so adding a step keeps them.
pathway_step_input_id <- function(field, version, index) {
  paste(field, version, index, sep = "_")
}

pathway_step_inputs <- function(input, session, field, count, version, draft, units = NULL) {
  shiny::tagList(lapply(seq_len(count), function(index) {
    input_id <- pathway_step_input_id(field, version, index)
    value <- shiny::isolate(input[[input_id]])
    if (is.null(value)) {
      value <- if (index <= length(draft[[field]])) draft[[field]][[index]]
               else if (field == "unit") "None" else NA_real_
    }
    switch(
      field,
      unit = shiny::selectInput(session$ns(input_id), paste("Unit", index),
                                choices = unit_choices(unique(c("None", units, value))), selected = value),
      los = shiny::numericInput(session$ns(input_id), paste("Mean stay", index, "(days)"),
                                value = value, min = 0.01, step = "any"),
      sd = shiny::numericInput(session$ns(input_id), paste("SD", index, "(days)"),
                               value = if (is.na(value)) NA_real_ else signif(value, 12),
                               min = 0.01, step = "any")
    )
  }))
}

read_pathway_steps <- function(input, count, version) {
  read_field <- function(field, missing) {
    vapply(seq_len(count), function(index) {
      value <- input[[pathway_step_input_id(field, version, index)]]
      if (is.null(value)) missing else value
    }, if (is.character(missing)) character(1) else numeric(1))
  }
  steps <- list(unit = read_field("unit", "None"), los = read_field("los", NA_real_),
                sd = read_field("sd", NA_real_))
  keep <- !is.na(steps$unit) & steps$unit != "None"
  lapply(steps, `[`, keep)
}

# Returns an error message, or NULL when every kept step is valid.
pathway_step_error <- function(steps, available_units) {
  if (!length(steps$unit)) return("A profile must contain at least one unit with a positive mean stay.")
  if (!all(is.finite(steps$los) & steps$los > 0)) return("Enter a positive mean stay for each selected unit.")
  if (!all(is.na(steps$sd) | (is.finite(steps$sd) & steps$sd > 0))) {
    return("Each SD must be positive, or blank to use the default variability.")
  }
  if (!all(steps$unit %in% available_units)) return("All trajectory units must be selected hospital units.")
  NULL
}

pathway_sd_help <- function() {
  shiny::helpText(
    "Mean stay and SD are in days. Leave SD blank to use the default variability",
    "(SD = 1 x mean stay for ICU, 0.24 x mean stay for other units)."
  )
}

profile_excel_sheet_columns <- list(
  Profiles = c("Profile", "Arrival_percent", "Ambulatory"),
  Trajectories = c("Profile", "Step", "Unit", "LOS_days"),
  Fallbacks = c("Primary_unit", "Priority", "Fallback_unit"),
  Hospital = c("Unit", "Total_beds")
)
# SD (days) is an optional Trajectories column: always written, but only
# required on read when the uploaded workbook includes it. Older workbooks may
# instead carry a CV column (used as-is), or neither (default: CV 1 for ICU
# steps, 0.24 for every other unit). Older Hospital sheets named the bed
# column Available_beds; it is still accepted.
profile_config_to_excel_tables <- function(profile_config) {
  profile_names <- names(profile_config$patient_profiles)
  trajectory_rows <- lapply(profile_names, function(profile_name) {
    profile <- profile_config$patient_profiles[[profile_name]]
    if (is.null(profile$unit) || length(profile$unit) == 0) return(NULL)
    data.frame(Profile = profile_name, Step = seq_along(profile$unit),
               Unit = profile$unit, LOS_days = profile$los, SD = profile_step_sd(profile),
               check.names = FALSE)
  })
  trajectory_rows <- Filter(Negate(is.null), trajectory_rows)
  fallback_rows <- lapply(names(profile_config$fallbacks), function(primary_unit) {
    fallback_units <- profile_config$fallbacks[[primary_unit]]
    if (length(fallback_units) == 0) return(NULL)
    data.frame(Primary_unit = primary_unit, Priority = seq_along(fallback_units),
               Fallback_unit = fallback_units, check.names = FALSE)
  })
  fallback_rows <- Filter(Negate(is.null), fallback_rows)

  list(
    Profiles = data.frame(
      Profile = profile_names,
      Arrival_percent = round(100 * profile_config$profile_prob[profile_names], 8),
      Ambulatory = vapply(profile_config$patient_profiles, function(profile) {
        is.null(profile$unit) || length(profile$unit) == 0
      }, logical(1)),
      check.names = FALSE
    ),
    Trajectories = if (length(trajectory_rows) == 0) {
      data.frame(Profile = character(), Step = integer(), Unit = character(),
                 LOS_days = numeric(), SD = numeric())
    } else do.call(rbind, trajectory_rows),
    Fallbacks = if (length(fallback_rows) == 0) {
      data.frame(Primary_unit = character(), Priority = integer(), Fallback_unit = character())
    } else do.call(rbind, fallback_rows),
    Hospital = data.frame(
      Unit = profile_config$units,
      Total_beds = unname(profile_config$capacities[profile_config$units]),
      check.names = FALSE
    )
  )
}

write_profile_config_xlsx <- function(profile_config, file) {
  tables <- profile_config_to_excel_tables(profile_config)
  workbook <- openxlsx::createWorkbook()
  header_style <- openxlsx::createStyle(
    fgFill = "#2F75B5", fontColour = "#FFFFFF", textDecoration = "bold",
    halign = "center", valign = "center"
  )
  editable_style <- openxlsx::createStyle(fgFill = "#FFF2CC")
  formats <- list(
    Profiles = list(column = 2, format = "0.00"),
    Trajectories = list(column = c(4, 5), format = "0.00"),
    Fallbacks = list(column = 2, format = "0"),
    Hospital = list(column = 2, format = "0")
  )

  for (sheet_name in names(tables)) {
    table_data <- tables[[sheet_name]]
    openxlsx::addWorksheet(workbook, sheet_name)
    openxlsx::writeData(workbook, sheet_name, table_data, headerStyle = header_style)
    openxlsx::freezePane(workbook, sheet_name, firstRow = TRUE)
    widths <- vapply(table_data, function(column) {
      content_width <- if (length(column) == 0) 0 else max(nchar(as.character(column)), na.rm = TRUE)
      min(28, max(12, content_width + 2))
    }, numeric(1))
    openxlsx::setColWidths(workbook, sheet_name, seq_along(table_data), widths)
    if (nrow(table_data) > 0) {
      data_rows <- 2:(nrow(table_data) + 1)
      openxlsx::addStyle(
        workbook, sheet_name, editable_style, rows = data_rows,
        cols = seq_along(table_data), gridExpand = TRUE
      )
      openxlsx::addFilter(workbook, sheet_name, row = 1, cols = seq_along(table_data))
      number_style <- openxlsx::createStyle(numFmt = formats[[sheet_name]]$format)
      openxlsx::addStyle(
        workbook, sheet_name, number_style, rows = data_rows,
        cols = formats[[sheet_name]]$column, gridExpand = TRUE, stack = TRUE
      )
    }
  }
  openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
  invisible(file)
}

write_empty_profile_template_xlsx <- function(file) {
  empty_configuration <- list(
    units = character(),
    capacities = stats::setNames(numeric(), character()),
    patient_profiles = stats::setNames(list(), character()),
    profile_prob = stats::setNames(numeric(), character()),
    fallbacks = stats::setNames(list(), character())
  )
  write_profile_config_xlsx(empty_configuration, file)
}

read_profile_config_xlsx <- function(file) {
  available_sheets <- readxl::excel_sheets(file)
  missing_sheets <- setdiff(names(profile_excel_sheet_columns), available_sheets)
  if (length(missing_sheets) > 0) {
    stop("Missing Excel sheet(s): ", paste(missing_sheets, collapse = ", "), call. = FALSE)
  }
  tables <- lapply(names(profile_excel_sheet_columns), function(sheet_name) {
    value <- as.data.frame(
      readxl::read_excel(file, sheet = sheet_name, .name_repair = "minimal"),
      check.names = FALSE
    )
    if (sheet_name == "Hospital" && !"Total_beds" %in% names(value) &&
        "Available_beds" %in% names(value)) {
      names(value)[names(value) == "Available_beds"] <- "Total_beds"
    }
    missing_columns <- setdiff(profile_excel_sheet_columns[[sheet_name]], names(value))
    if (length(missing_columns) > 0) {
      stop("Sheet '", sheet_name, "' is missing column(s): ",
           paste(missing_columns, collapse = ", "), call. = FALSE)
    }
    optional_columns <- if (sheet_name == "Trajectories") intersect(c("SD", "CV"), names(value)) else character()
    value[, c(profile_excel_sheet_columns[[sheet_name]], optional_columns), drop = FALSE]
  })
  names(tables) <- names(profile_excel_sheet_columns)

  profiles <- tables$Profiles
  profiles$Profile <- trimws(as.character(profiles$Profile))
  profiles$Arrival_percent <- suppressWarnings(as.numeric(profiles$Arrival_percent))
  ambulatory_map <- c("true" = TRUE, "false" = FALSE, "1" = TRUE, "0" = FALSE,
                      "yes" = TRUE, "no" = FALSE, "y" = TRUE, "n" = FALSE)
  profiles$Ambulatory <- unname(ambulatory_map[
    tolower(trimws(as.character(profiles$Ambulatory)))
  ])
  if (nrow(profiles) == 0) stop("Sheet 'Profiles' must contain at least one profile.", call. = FALSE)
  if (any(is.na(profiles$Profile)) || any(!nzchar(profiles$Profile))) {
    stop("Every row in 'Profiles' must have a profile name.", call. = FALSE)
  }
  if (any(!grepl("^[A-Za-z][A-Za-z0-9_-]*$", profiles$Profile))) {
    stop("Profile names must start with a letter and use only letters, numbers, underscores, or hyphens.", call. = FALSE)
  }
  if (anyDuplicated(profiles$Profile)) stop("Profile names in 'Profiles' must be unique.", call. = FALSE)
  if (any(!is.finite(profiles$Arrival_percent)) || any(profiles$Arrival_percent < 0)) {
    stop("'Arrival_percent' must contain non-negative numbers.", call. = FALSE)
  }
  if (abs(sum(profiles$Arrival_percent) - 100) > 0.01) {
    stop("'Arrival_percent' values must sum to 100.", call. = FALSE)
  }
  if (any(is.na(profiles$Ambulatory))) {
    stop("'Ambulatory' must use TRUE/FALSE, Yes/No, or 1/0.", call. = FALSE)
  }

  hospital <- tables$Hospital
  hospital$Unit <- trimws(as.character(hospital$Unit))
  hospital$Total_beds <- suppressWarnings(as.numeric(hospital$Total_beds))
  if (nrow(hospital) == 0) stop("Sheet 'Hospital' must contain at least one unit.", call. = FALSE)
  if (any(!hospital$Unit %in% hospital_profile_units)) {
    stop("Unknown hospital unit(s): ",
         paste(setdiff(unique(hospital$Unit), hospital_profile_units), collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(hospital$Unit)) stop("Hospital units must be unique.", call. = FALSE)
  # Internal units (ED) are always present with their fixed capacity, whatever
  # the workbook lists for them.
  hospital <- hospital[!hospital$Unit %in% names(internal_hospital_units), , drop = FALSE]
  hospital <- rbind(
    data.frame(Unit = names(internal_hospital_units),
               Total_beds = unname(internal_hospital_units)),
    hospital
  )
  if (any(!is.finite(hospital$Total_beds)) || any(hospital$Total_beds < 0)) {
    stop("'Total_beds' must contain non-negative numbers.", call. = FALSE)
  }

  trajectories <- tables$Trajectories
  trajectories$Profile <- trimws(as.character(trajectories$Profile))
  trajectories$Step <- suppressWarnings(as.numeric(trajectories$Step))
  trajectories$Unit <- trimws(as.character(trajectories$Unit))
  trajectories$LOS_days <- suppressWarnings(as.numeric(trajectories$LOS_days))
  variability_column <- intersect(c("SD", "CV"), names(trajectories))[1]
  trajectories$CV <- if (identical(variability_column, "SD")) {
    suppressWarnings(as.numeric(trajectories$SD)) / trajectories$LOS_days
  } else if (identical(variability_column, "CV")) {
    suppressWarnings(as.numeric(trajectories$CV))
  } else {
    default_cv_for_unit(trajectories$Unit)
  }
  if (nrow(trajectories) > 0) {
    if (any(!trajectories$Profile %in% profiles$Profile)) {
      stop("Every trajectory must reference a profile listed in 'Profiles'.", call. = FALSE)
    }
    if (any(!trajectories$Unit %in% hospital$Unit)) {
      stop("Every trajectory unit must be listed in the 'Hospital' sheet.", call. = FALSE)
    }
    if (any(!is.finite(trajectories$Step)) || any(trajectories$Step < 1) ||
        any(trajectories$Step != floor(trajectories$Step))) {
      stop("'Step' must contain positive whole numbers.", call. = FALSE)
    }
    if (any(!is.finite(trajectories$LOS_days)) || any(trajectories$LOS_days <= 0)) {
      stop("'LOS_days' must contain positive numbers.", call. = FALSE)
    }
    if (any(!is.finite(trajectories$CV)) || any(trajectories$CV <= 0)) {
      stop("'", variability_column, "' must contain positive numbers, or be left out entirely ",
           "to default to SD = 1 x LOS for ICU steps and 0.24 x LOS for every other unit.",
           call. = FALSE)
    }
  }

  patient_profiles <- stats::setNames(vector("list", nrow(profiles)), profiles$Profile)
  for (profile_index in seq_len(nrow(profiles))) {
    profile_name <- profiles$Profile[[profile_index]]
    rows <- trajectories[trajectories$Profile == profile_name, , drop = FALSE]
    if (profiles$Ambulatory[[profile_index]]) {
      if (nrow(rows) > 0) {
        stop("Ambulatory profile '", profile_name, "' cannot contain trajectory rows.", call. = FALSE)
      }
      patient_profiles[[profile_name]] <- list(unit = NULL, los = NULL)
    } else {
      if (nrow(rows) == 0) {
        stop("Non-ambulatory profile '", profile_name, "' requires a trajectory.", call. = FALSE)
      }
      rows <- rows[order(rows$Step), , drop = FALSE]
      if (!identical(as.integer(rows$Step), seq_len(nrow(rows)))) {
        stop("Trajectory steps for '", profile_name, "' must be unique and sequential from 1.", call. = FALSE)
      }
      patient_profiles[[profile_name]] <- list(unit = rows$Unit, los = rows$LOS_days, cv = rows$CV)
    }
  }

  fallback_table <- tables$Fallbacks
  fallbacks <- list()
  if (nrow(fallback_table) > 0) {
    fallback_table$Primary_unit <- trimws(as.character(fallback_table$Primary_unit))
    fallback_table$Priority <- suppressWarnings(as.numeric(fallback_table$Priority))
    fallback_table$Fallback_unit <- trimws(as.character(fallback_table$Fallback_unit))
    fallback_units <- unique(c(fallback_table$Primary_unit, fallback_table$Fallback_unit))
    if (any(!fallback_units %in% hospital$Unit)) {
      stop("Every fallback unit must be listed in the 'Hospital' sheet.", call. = FALSE)
    }
    if (any(fallback_table$Primary_unit == fallback_table$Fallback_unit)) {
      stop("A hospital unit cannot be its own fallback.", call. = FALSE)
    }
    if (any(!is.finite(fallback_table$Priority)) || any(fallback_table$Priority < 1) ||
        any(fallback_table$Priority != floor(fallback_table$Priority))) {
      stop("'Priority' must contain positive whole numbers.", call. = FALSE)
    }
    for (primary_unit in unique(fallback_table$Primary_unit)) {
      rows <- fallback_table[fallback_table$Primary_unit == primary_unit, , drop = FALSE]
      rows <- rows[order(rows$Priority), , drop = FALSE]
      if (!identical(as.integer(rows$Priority), seq_len(nrow(rows)))) {
        stop("Fallback priorities for '", primary_unit, "' must be sequential from 1.", call. = FALSE)
      }
      if (anyDuplicated(rows$Fallback_unit)) {
        stop("Fallback units for '", primary_unit, "' must be unique.", call. = FALSE)
      }
      fallbacks[[primary_unit]] <- rows$Fallback_unit
    }
  }

  list(
    source = "excel_upload",
    source_label = paste("Uploaded Excel:", basename(file)),
    units = hospital$Unit,
    capacities = stats::setNames(hospital$Total_beds, hospital$Unit),
    patient_profiles = patient_profiles,
    profile_prob = stats::setNames(profiles$Arrival_percent / 100, profiles$Profile),
    fallbacks = fallbacks
  )
}
hospital_profiles_ui <- function(id) {
  ns <- shiny::NS(id)
  # TRUE when profiles were loaded rather than typed: an Excel upload or a
  # predefined hospital.
  test_condition <- sprintf("input['%s'] != 'manual' || input['%s'] == 'predefined'",
                            ns("profile_source"), ns("hospital_source"))
  excel_condition <- sprintf("input['%s'] == 'excel_upload'", ns("profile_source"))
  not_predefined_condition <- sprintf("input['%s'] != 'predefined'", ns("profile_source"))

  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(
        title = "Hospital Information",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        rintrojs::introBox(
          shiny::fluidRow(
            shinydashboard::box(
              title = "Hospital units",
              status = "info",
              solidHeader = TRUE,
              width = 3,
              rintrojs::introBox(
              shiny::radioButtons(
                ns("hospital_source"),
                label = NULL,
                choices = c(
                  "Add hospital units manually" = "manual",
                  "Use predefined hospital" = "predefined"
                ),
                selected = "manual"
              ),
              shiny::conditionalPanel(
                condition = sprintf("input['%s'] == 'manual'", ns("hospital_source")),
                # Shaded panel (CSS in ui_main.R) sets the option's content apart
                # from the two option buttons above.
                class = "option-detail-panel",
                shiny::checkboxGroupInput(
                  ns("hospital_units"),
                  label = NULL,
                  choices = editable_hospital_units,
                  selected = manual_hospital_defaults$units
                )
              ),
              shiny::conditionalPanel(
                condition = sprintf("input['%s'] == 'predefined'", ns("hospital_source")),
                class = "option-detail-panel",
                shiny::selectInput(ns("predefined_hospital"), "Predefined hospital",
                                   choices = predefined_hospitals),
                shiny::helpText(
                  "Loads the hospital's units, beds, fallbacks and NDMS-based surge profiles.",
                  "Profiles you created are kept. Switching back to 'Add hospital units manually'",
                  "clears everything this hospital loaded."
                )
              ),
              data.step = 3,
              data.intro = paste(
                "<strong>Hospital units.</strong><br>",
                "Choose <em>Use predefined hospital</em> (Regional, Tertiary or Community) to load",
                "its units, beds, fallbacks and NDMS-based surge profiles in one step, or",
                "<em>Add hospital units manually</em> and tick the units your hospital has.",
                "Only the selected units can be used in pathways and fallbacks."
              ),
              data.position = "right"
              )
            ),
            shinydashboard::box(
              title = "Fallbacks",
              status = "info",
              solidHeader = TRUE,
              width = 3,
              rintrojs::introBox(
                shiny::tagList(
                  shiny::selectInput(ns("fallback_unit"), "Primary unit", choices = NULL),
                  shiny::selectizeInput(
                    ns("fallback_options"), "Fallback units", choices = NULL, multiple = TRUE
                  ),
                  shiny::actionButton(ns("add_fallback"), "Save fallback", class = "btn-primary"),
                  shiny::actionButton(ns("remove_fallback"), "Remove fallback"),
                  shiny::helpText("Select a primary unit to edit its existing alternatives. Alternatives are tried in the displayed order."),
                  shiny::tableOutput(ns("fallbacks_summary"))
                ),
                data.step = 4,
                data.intro = paste(
                  "<strong>Fallbacks.</strong><br>",
                  "When a primary unit is full, the model tries these alternatives",
                  "in the displayed order. If none is available, the patient is",
                  "counted in the primary-unit queue. When a bed is released, patients",
                  "already holding a bed elsewhere (boarding) go first, then the oldest request.",
                  "A predefined hospital or Use predefined profiles fills these rules for you."
                ),
                data.position = "top"
              )
            ),
            shinydashboard::box(
              # "Total available beds" or "Total beds", depending on civilian flow.
              title = shiny::textOutput(ns("capacity_title"), inline = TRUE),
              status = "info",
              solidHeader = TRUE,
              width = 6,
              # State behind the "Select unlimited capacity" button (hidden).
              shiny::div(style = "display: none;",
                         shiny::checkboxInput(ns("unlimited_capacity"), NULL, FALSE)),
              rintrojs::introBox(
                shiny::uiOutput(ns("capacity_ui")),
                data.step = 5,
                data.intro = paste(
                  "<strong>Beds.</strong><br>",
                  "Enter the beds of each selected unit. Without routine civilian arrivals the",
                  "box is <em>Total available beds</em>: the beds free for the surge on an empty",
                  "hospital. With them it becomes <em>Total beds</em>, because the civilian warm-up",
                  "fills the hospital first.<br>",
                  "<em>Select unlimited capacity</em> instead computes the expected occupancy with no",
                  "bed limit (analytic, no simulation); click it again to use the bed counts."
                ),
                data.position = "left"
              )
            )
          ),
          shiny::helpText(
            "The ED is always part of the hospital.",
            "Its capacity is fixed at 10,000 beds (practically unlimited), so it is not listed here.",
            "Civilian pathways start in the ED; it is not offered in surge trajectories or fallbacks."
          ),
          data.step = 2,
          data.intro = paste(
            "<strong>Describe the hospital.</strong><br>",
            "Hospital Information has three boxes: the hospital units, the fallback rules,",
            "and the beds of each unit. The next steps explain each one.",
            "The ED is always included with practically unlimited capacity (10,000 beds) and",
            "is not editable; civilian pathways start there."
          ),
          data.position = "bottom"
        )
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        title = "Create or edit surge patient trajectory",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        shiny::fluidRow(
          shinydashboard::box(
            title = "Patient profile source",
            status = "info",
            solidHeader = TRUE,
            width = 3,
            rintrojs::introBox(
              shiny::radioButtons(
                ns("profile_source"),
                "Patient profile source",
                choices = c(
                  "Create profiles manually" = "manual",
                  "Use predefined profiles" = "predefined",
                  "Upload profiles from Excel" = "excel_upload"
                ),
                selected = "manual"
              ),
              shiny::conditionalPanel(
                condition = sprintf("input['%s'] == 'predefined'", ns("profile_source")),
                class = "option-detail-panel",
                shiny::helpText(
                  "NDMS-Based Classification profiles selected for the units in Hospital Information:",
                  "pathways that use a unit the hospital lacks are excluded, and the fallbacks are set",
                  "for the hospital's units. Profiles update when the units change; profiles you created are kept."
                ),
                shiny::uiOutput(ns("ndms_profiles_note"))
              ),
              shiny::helpText("Uploading an Excel workbook replaces the hospital units, beds, fallbacks and surge profiles with the workbook's. All values can be edited for the current scenario."),
              shiny::conditionalPanel(
                condition = excel_condition,
                shiny::downloadButton(
                  ns("download_profile_template"),
                  "Download empty Excel template",
                  class = "btn-default"
                ),
                shiny::helpText(
                  "Download the blank workbook first if you need the required Excel format."
                ),
                shiny::fileInput(
                  ns("profile_excel_file"),
                  "Profile configuration (.xlsx)",
                  accept = c(
                    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                    ".xlsx"
                  )
                ),
                shiny::helpText(
                  "Upload a workbook previously downloaded from this app. ",
                  "It must contain Profiles, Trajectories, Fallbacks, and Hospital sheets."
                )
              ),
              data.step = 6,
              data.intro = paste(
                "<strong>Choose the surge patient profiles.</strong><br>",
                "A predefined hospital already loads its NDMS-based profiles. Otherwise use",
                "predefined profiles (NDMS-based, selected for your units), upload an Excel",
                "configuration, or create profiles manually with the trajectory editor below:",
                "ordered units with a mean stay and standard deviation (SD) per unit."
              ),
              data.position = "bottom"
            )
          ),
          shinydashboard::box(
            title = "Profile arrival percentages",
            status = "info",
            solidHeader = TRUE,
            width = 4,
            rintrojs::introBox(
              shiny::tagList(
                shiny::div(
                  style = "max-height: 320px; overflow-y: auto;",
                  shiny::uiOutput(ns("profile_percent_ui"))
                ),
                shiny::actionButton(
                  ns("set_profile_percentages"),
                  "Set arrival percentages",
                  class = "btn-primary"
                ),
                shiny::uiOutput(ns("profile_percent_status"))
              ),
              shiny::conditionalPanel(
                condition = test_condition,
                shiny::helpText("Loaded percentages are already confirmed. After changing percentages or adding/removing profiles, confirm a total of 100%.")
              ),
              data.step = 7,
              data.intro = paste(
                "<strong>Set the patient mix.</strong><br>",
                "Assign the percentage of arrivals belonging to each profile.",
                "Percentages must total 100%; after editing them, click Set arrival percentages."
              ),
              data.position = "top"
            )
          ),
          shinydashboard::box(
            title = "Configuration status",
            status = "success",
            solidHeader = TRUE,
            width = 5,
            style = "overflow-x: auto;",
            rintrojs::introBox(
              shiny::uiOutput(ns("configuration_status")),
              shiny::div(
                style = "max-height: 320px; overflow-y: auto;",
                shiny::tableOutput(ns("profiles_summary"))
              ),
              shiny::downloadButton(
                ns("download_profile_config"),
                "Download surge profile configuration",
                class = "btn-primary"
              ),
              shiny::helpText("Excel saves hospital beds and surge profiles. Save civilian profiles with Download saved profiles in Routine Civilian Flow."),
              data.step = 8,
              data.intro = paste(
                "<strong>Confirm that the setup is ready.</strong><br>",
                "This panel turns green when the setup is ready; otherwise it lists what is",
                "missing. It also summarizes all profiles and lets you download the surge",
                "setup as an Excel workbook for future simulations."
              ),
              data.position = "left"
            )
          )
        ),
        shiny::conditionalPanel(
          condition = not_predefined_condition,
          shiny::fluidRow(
          shinydashboard::box(
            title = "Surge patient trajectory editor",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            rintrojs::introBox(
            shiny::fluidRow(
              shiny::column(
                width = 4,
                shiny::selectInput(ns("remove_profile_name"), "Saved profile", choices = NULL),
                shiny::actionButton(ns("edit_profile"), "Load profile for editing"),
                shiny::actionButton(ns("remove_profile"), "Remove selected profile", class = "btn-danger"),
                shiny::hr(),
                shiny::textInput(ns("profile_name"), "Patient profile name", "profile_1"),
                shiny::checkboxInput(ns("ambulatory_profile"), "Ambulatory (no inpatient beds)", FALSE)
              ),
              shiny::column(
                width = 8,
                shiny::fluidRow(
                  shiny::column(4, shiny::uiOutput(ns("trajectory_units_ui"))),
                  shiny::column(4, shiny::uiOutput(ns("trajectory_los_ui"))),
                  shiny::column(4, shiny::uiOutput(ns("trajectory_sd_ui")))
                ),
                pathway_sd_help(),
                shiny::actionButton(
                  ns("add_trajectory_unit"),
                  "Add unit",
                  icon = shiny::icon("plus"),
                  class = "btn-default"
                ),
                shiny::actionButton(ns("remove_trajectory_unit"), "Remove last unit"),
                shiny::actionButton(ns("add_profile"), "Save patient profile", class = "btn-primary")
              )
            ),
            data.step = 9,
            data.intro = paste(
              "<strong>Surge patient trajectory editor.</strong><br>",
              "To create a profile: type a name, choose Unit 1 and its mean stay (days), and",
              "optionally its SD (blank uses a default). Use <em>Add unit</em> for each further",
              "unit the patient visits, in order, then click <em>Save patient profile</em>.",
              "To change a profile, select it under Saved profile and click <em>Load profile",
              "for editing</em>. Tick Ambulatory for patients who need no inpatient bed.",
              "Remember to set the arrival percentages again after adding profiles."
            ),
            data.position = "top"
            ),
            shiny::conditionalPanel(
              condition = test_condition,
              shiny::div(
                class = "alert alert-info",
                style = "margin-top: 10px;",
                "Profiles, arrival percentages, beds and fallbacks are loaded and editable. Load a saved profile above to change its pathway, mean stay or SD."
              )
            )
          )
        )
        )
      )
    )
  )
}

# civilian_enabled: reactive TRUE when routine civilian arrivals are enabled;
# it switches the bed inputs between total beds and beds available to the surge.
# Unlimited capacity is stored in the hidden input "unlimited_capacity" (read by
# the app server and the sidebar's conditional panels as
# input['profiles-unlimited_capacity']); the visible button toggles it.
hospital_profiles_server <- function(id, require_surge_profiles = function() TRUE,
                                     civilian_enabled = function() FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$select_unlimited_capacity, {
      shiny::updateCheckboxInput(session, "unlimited_capacity",
                                 value = !isTRUE(input$unlimited_capacity))
    })
    patient_profiles <- shiny::reactiveVal(list())
    fallbacks <- shiny::reactiveVal(list())
    confirmed_profile_probabilities <- shiny::reactiveVal(NULL)
    trajectory_unit_count <- shiny::reactiveVal(1L)
    trajectory_form_version <- shiny::reactiveVal(1L)
    pending_profile_replacement <- shiny::reactiveVal(NULL)
    pending_profile_removal <- shiny::reactiveVal(NULL)
    uploaded_config <- shiny::reactiveVal(NULL)
    configuration_version <- shiny::reactiveVal(0L)
    loaded_probabilities <- shiny::reactiveVal(numeric())
    loaded_capacities <- shiny::reactiveVal(manual_hospital_defaults$capacities)
    pending_loaded_units <- shiny::reactiveVal(NULL)
    empty_trajectory_draft <- list(unit = character(), los = numeric(), sd = numeric())
    trajectory_draft <- shiny::reactiveVal(empty_trajectory_draft)
    # Names of the surge profiles loaded from the current predefined hospital.
    # They are replaced when another hospital is selected; every other profile
    # was created or imported by the user and is kept.
    hospital_profile_names <- shiny::reactiveVal(character())

    configuration_input_id <- function(prefix, name) {
      paste(prefix, configuration_version(), name, sep = "_")
    }

    # Percentage inputs get their own version so loading profiles can refresh
    # them without resetting the bed inputs the user may have edited.
    percentage_version <- shiny::reactiveVal(0L)
    percentage_input_id <- function(name) {
      paste("prob", configuration_version(), percentage_version(), name, sep = "_")
    }

    # Identifies the configuration for labels, civilian profile files and the
    # civilian profile store: the predefined hospital ID when one is in use,
    # otherwise the profile source ("manual" or "excel_upload").
    configuration_source <- shiny::reactive({
      if (identical(input$hospital_source, "predefined") && !is.null(input$predefined_hospital)) {
        input$predefined_hospital
      } else if (is.null(input$profile_source)) {
        "manual"
      } else {
        input$profile_source
      }
    })

    predefined_source_label <- function(source) {
      hospital_name <- names(predefined_hospitals)[predefined_hospitals %in% source]
      if (length(hospital_name)) paste0("NDMS-Based Classification (", hospital_name, ")") else NULL
    }

    shiny::observeEvent(input$profile_excel_file, {
      uploaded_file <- input$profile_excel_file
      shiny::req(!is.null(uploaded_file$datapath))
      tryCatch(
        {
          imported_config <- read_profile_config_xlsx(uploaded_file$datapath)
          imported_config$source_label <- paste("Uploaded Excel:", uploaded_file$name)
          uploaded_config(imported_config)
          shiny::showNotification(
            paste(length(imported_config$patient_profiles), "profiles loaded from Excel."),
            type = "message"
          )
        },
        error = function(error) {
          uploaded_config(NULL)
          shiny::showNotification(
            paste("Excel file could not be loaded:", conditionMessage(error)),
            type = "error",
            duration = NULL
          )
        }
      )
    })

    # Templates seed mutable state once; simulation/export read only that state.
    # Bumping configuration_version gives the bed and percentage inputs new IDs,
    # so stale browser values from the previous configuration are discarded.
    apply_hospital <- function(config) {
      # ED is always part of the hospital, whether or not the source lists it.
      units <- union(names(internal_hospital_units), config$units)
      pending_loaded_units(units)
      loaded_capacities(config$capacities)
      configuration_version(configuration_version() + 1L)
      fallbacks(config$fallbacks)
      shiny::updateCheckboxGroupInput(session, "hospital_units",
                                      selected = setdiff(units, names(internal_hospital_units)))
      shiny::updateSelectInput(session, "fallback_unit", selected = "")
      shiny::updateSelectizeInput(session, "fallback_options", choices = unit_choices(units),
                                  selected = character(), server = TRUE)
    }

    reset_profile_form <- function() {
      pending_profile_replacement(NULL)
      pending_profile_removal(NULL)
      trajectory_draft(empty_trajectory_draft)
      trajectory_unit_count(1L)
      trajectory_form_version(trajectory_form_version() + 1L)
      shiny::removeModal()
      shiny::updateTextInput(session, "profile_name", value = "profile_1")
      shiny::updateCheckboxInput(session, "ambulatory_profile", value = FALSE)
    }

    # Loads a hospital's units, beds, fallbacks and profiles. Profiles of the
    # previously loaded predefined hospital are replaced; the user's own
    # profiles are kept with their entered percentages. Percentages count as
    # confirmed only if they still total 100%; otherwise Configuration status
    # asks the user to confirm them again.
    load_profiles <- function(config) {
      current <- patient_profiles()
      user_names <- setdiff(names(current), c(hospital_profile_names(), names(config$patient_profiles)))
      entered <- shiny::isolate(entered_profile_percentages()) / 100
      user_probabilities <- stats::setNames(
        vapply(user_names, function(name) {
          value <- unname(entered[name])
          if (length(value) == 1L && is.finite(value)) value else 0
        }, numeric(1)),
        user_names
      )
      profiles <- c(config$patient_profiles, current[user_names])
      probabilities <- c(config$profile_prob[names(config$patient_profiles)], user_probabilities)
      loaded_probabilities(probabilities)
      patient_profiles(profiles)
      hospital_profile_names(names(config$patient_profiles))
      confirmed_profile_probabilities(
        if (isTRUE(abs(sum(probabilities) - 1) < 1e-8)) probabilities else NULL
      )
      percentage_version(percentage_version() + 1L)
      reset_profile_form()
    }

    load_hospital <- function(config) {
      load_profiles(config)
      apply_hospital(config)
    }

    # TRUE while the current configuration comes from a predefined hospital.
    predefined_hospital_loaded <- shiny::reactiveVal(FALSE)

    # Selecting a predefined hospital loads it. Going back to "Add hospital
    # units manually" clears everything that hospital loaded (units, beds,
    # fallbacks and its NDMS-based profiles) and restores the initial manual
    # units; profiles created by the user are kept.
    shiny::observeEvent(list(input$hospital_source, input$predefined_hospital), {
      if (identical(input$hospital_source, "predefined")) {
        config <- predefined_hospital_config(input$predefined_hospital)
        shiny::req(config)
        load_hospital(config)
        predefined_hospital_loaded(TRUE)
      } else if (isTRUE(predefined_hospital_loaded())) {
        load_hospital(manual_hospital_defaults)
        predefined_hospital_loaded(FALSE)
      }
    }, ignoreInit = TRUE, priority = 100)

    # Units available for NDMS-based profile selection: selected units, except
    # those with 0 beds. A reactiveVal only signals when the set changes, so
    # typing bed counts does not regenerate the profiles.
    ndms_units <- shiny::reactiveVal(NULL)
    shiny::observe({
      caps <- capacities()
      ndms_units(sort(names(caps)[is.na(caps) | caps > 0]))
    })
    ndms_profiles_config <- shiny::reactiveVal(NULL)

    # "Use predefined profiles": NDMS-based profiles for the hospital's current
    # units, regenerated whenever that set changes. Pathways through missing
    # units are excluded (not rerouted) and percentages renormalized; the
    # fallback network is fallbacks_list restricted to the hospital's units.
    # Profiles of the previous selection are replaced; the user's own
    # profiles are kept.
    shiny::observeEvent(list(input$profile_source, ndms_units()), {
      shiny::req(identical(input$profile_source, "predefined"), length(ndms_units()) > 0)
      units <- ndms_units()
      config <- tryCatch(
        deloitte_profiles_for_hospital(stats::setNames(rep(1, length(units)), units),
                                       wia_prob = ndms_wia_prob),
        error = function(error) NULL
      )
      ndms_profiles_config(config)
      shiny::req(config)
      load_profiles(config)
      fallbacks(config$fallbacks)
    }, ignoreInit = TRUE)

    output$ndms_profiles_note <- shiny::renderUI({
      shiny::req(identical(input$profile_source, "predefined"))
      config <- ndms_profiles_config()
      if (is.null(config)) {
        return(shiny::div(class = "alert alert-warning",
                          "No NDMS-based pathway uses only this hospital's units."))
      }
      n_profiles <- length(config$patient_profiles)
      shiny::helpText(sprintf("%d NDMS-based %s loaded.", n_profiles,
                              ngettext(n_profiles, "profile", "profiles")))
    })

    # Excel upload: the workbook replaces the hospital and every surge profile.
    shiny::observeEvent(list(input$profile_source, uploaded_config()), {
      config <- uploaded_config()
      shiny::req(identical(input$profile_source, "excel_upload"), config)
      # The workbook's hospital is manual; switching the radio button below must
      # not clear it as if a predefined hospital were being left.
      predefined_hospital_loaded(FALSE)
      shiny::updateRadioButtons(session, "hospital_source", selected = "manual")
      loaded_probabilities(config$profile_prob)
      patient_profiles(config$patient_profiles)
      hospital_profile_names(character())
      confirmed_profile_probabilities(if (length(config$profile_prob)) config$profile_prob else NULL)
      apply_hospital(config)
      reset_profile_form()
    }, ignoreInit = TRUE, priority = 100)

    # Internal units (ED) come first, matching the previous checkbox order.
    selected_units <- shiny::reactive({
      unique(c(names(internal_hospital_units), input$hospital_units))
    })

    # Default value last written into each capacity input. A current value that
    # differs from it was typed by the user and is kept when the civilian
    # toggle changes the defaults; untouched inputs follow the new default.
    shown_capacity_defaults <- list()

    output$capacity_title <- shiny::renderText(capacity_label(civilian_enabled()))

    output$capacity_ui <- shiny::renderUI({
      units <- setdiff(selected_units(), names(internal_hospital_units))
      shiny::req(length(units) > 0)
      civilian <- isTRUE(civilian_enabled())
      unlimited <- isTRUE(input$unlimited_capacity)
      shiny::tagList(
        shiny::actionButton(
          session$ns("select_unlimited_capacity"),
          if (unlimited) "Unlimited capacity selected (click to use the bed counts)"
          else "Select unlimited capacity",
          icon = shiny::icon("infinity"),
          class = if (unlimited) "btn-success" else "btn-default"
        ),
        if (unlimited) shiny::helpText(
          "Unlimited capacity: the bed counts below are not used. Occupancy is computed analytically",
          "when you click Run Simulation."
        ) else shiny::helpText(
          "Alternatively, evaluate demand with no bed limit (analytic occupancy, no simulation)."
        ),
        if (!civilian) shiny::helpText(
          "Beds available to the surge. Defaults assume 95% occupancy in GenMed and ICU",
          "and 50% in other units (rounded up); enable routine civilian arrivals to enter total beds instead."
        ),
        shiny::fluidRow(
          lapply(units, function(unit_name) {
            input_id <- configuration_input_id("capacity", unit_name)
            current_capacity <- shiny::isolate(input[[input_id]])
            configured_capacity <- unname(loaded_capacities()[unit_name])
            total_beds <- if (length(configured_capacity) == 1 &&
                              is.finite(configured_capacity)) {
              configured_capacity
            } else if (unit_name == "GenMed") {
              422
            } else if (unit_name == "ICU") {
              84
            } else if (unit_name == "Surge") {
              15
            } else {
              15
            }
            default_capacity <- if (civilian) total_beds else available_beds_default(unit_name, total_beds)
            user_edited <- !is.null(current_capacity) &&
              !isTRUE(as.numeric(current_capacity) == as.numeric(shown_capacity_defaults[[input_id]]))
            if (user_edited) {
              default_capacity <- current_capacity
            } else {
              shown_capacity_defaults[[input_id]] <<- default_capacity
            }
            shiny::column(
              width = 4,
              shiny::numericInput(
                session$ns(input_id),
                unit_display_label(unit_name),
                min = 0,
                max = 500,
                value = default_capacity
              )
            )
          })
        )
      )
    })

    shiny::observe({
      units <- setdiff(selected_units(), names(internal_hospital_units))
      primary <- input$fallback_unit
      selected_primary <- if (!is.null(primary) && primary %in% units) primary else ""
      shiny::updateSelectInput(
        session,
        "fallback_unit",
        choices = c("Select a primary unit" = "", unit_choices(units)),
        selected = selected_primary
      )
    })

    shiny::observeEvent(list(input$fallback_unit, selected_units()), {
      primary <- input$fallback_unit
      if (is.null(primary)) primary <- ""
      fallback_choices <- setdiff(selected_units(), c(primary, names(internal_hospital_units)))
      selected_fallbacks <- intersect(fallbacks()[[primary]], fallback_choices)
      shiny::updateSelectizeInput(
        session,
        "fallback_options",
        choices = unit_choices(fallback_choices),
        selected = selected_fallbacks,
        server = TRUE
      )
    }, ignoreNULL = FALSE)

    shiny::observe({
      units <- selected_units()
      expected_units <- pending_loaded_units()
      if (!is.null(expected_units)) {
        # updateCheckboxGroupInput reaches the browser on the next flush.
        if (!setequal(units, expected_units)) return(invisible(NULL))
        pending_loaded_units(NULL)
      }
      current <- fallbacks()
      valid_primary_units <- intersect(names(current), units)
      cleaned <- lapply(current[valid_primary_units], function(fallback_units) {
        intersect(fallback_units, units)
      })
      cleaned <- Filter(function(fallback_units) length(fallback_units) > 0, cleaned)
      if (!identical(current, cleaned)) fallbacks(cleaned)
    })

    output$trajectory_units_ui <- shiny::renderUI({
      units <- setdiff(selected_units(), names(internal_hospital_units))
      shiny::req(length(units) > 0)
      pathway_step_inputs(input, session, "unit", trajectory_unit_count(),
                          trajectory_form_version(), trajectory_draft(), units)
    })

    output$trajectory_los_ui <- shiny::renderUI({
      pathway_step_inputs(input, session, "los", trajectory_unit_count(),
                          trajectory_form_version(), trajectory_draft())
    })

    output$trajectory_sd_ui <- shiny::renderUI({
      pathway_step_inputs(input, session, "sd", trajectory_unit_count(),
                          trajectory_form_version(), trajectory_draft())
    })

    shiny::observeEvent(input$add_trajectory_unit, {
      trajectory_unit_count(trajectory_unit_count() + 1L)
    })

    shiny::observeEvent(input$remove_trajectory_unit, {
      trajectory_unit_count(max(1L, trajectory_unit_count() - 1L))
    })

    shiny::observeEvent(input$edit_profile, {
      profile_name <- input$remove_profile_name
      shiny::req(profile_name %in% names(patient_profiles()))
      profile <- patient_profiles()[[profile_name]]
      # Show the SD the engine actually uses, so re-saving keeps the same draws.
      trajectory_draft(list(unit = profile$unit, los = profile$los, sd = profile_step_sd(profile)))
      trajectory_unit_count(max(1L, length(profile$unit)))
      trajectory_form_version(trajectory_form_version() + 1L)
      shiny::updateTextInput(session, "profile_name", value = profile_name)
      shiny::updateCheckboxInput(session, "ambulatory_profile", value = length(profile$unit) == 0)
    })

    commit_patient_profile <- function(profile_name, profile) {
      profiles <- patient_profiles()
      profiles[[profile_name]] <- profile
      patient_profiles(profiles)
      trajectory_draft(empty_trajectory_draft)
      trajectory_unit_count(1L)
      trajectory_form_version(trajectory_form_version() + 1L)
      shiny::updateCheckboxInput(session, "ambulatory_profile", value = FALSE)
    }

    shiny::observeEvent(input$add_profile, {
      profile_name <- trimws(input$profile_name)
      shiny::req(nzchar(profile_name))
      shiny::validate(shiny::need(
        grepl("^[A-Za-z][A-Za-z0-9_-]*$", profile_name),
        "Profile names must start with a letter and use only letters, numbers, underscores, or hyphens."
      ))
      if (isTRUE(input$ambulatory_profile)) {
        new_profile <- list(unit = NULL, los = NULL, cv = NULL)
      } else {
        steps <- read_pathway_steps(input, trajectory_unit_count(), trajectory_form_version())
        step_error <- pathway_step_error(steps, selected_units())
        shiny::validate(shiny::need(is.null(step_error), step_error))
        new_profile <- list(unit = steps$unit, los = steps$los,
                            cv = step_cv_from_sd(steps$unit, steps$los, steps$sd))
      }
      if (profile_name %in% names(patient_profiles())) {
        pending_profile_replacement(list(
          name = profile_name,
          profile = new_profile
        ))
        shiny::showModal(shiny::modalDialog(
          title = "Replace patient profile?",
          paste0(
            "Profile '", profile_name,
            "' already exists. Are you sure you want to replace it?"
          ),
          easyClose = FALSE,
          footer = shiny::tagList(
            shiny::actionButton(
              session$ns("cancel_replace_profile"),
              "Cancel",
              class = "btn-default"
            ),
            shiny::actionButton(
              session$ns("confirm_replace_profile"),
              "Yes, replace",
              class = "btn-danger"
            )
          )
        ))
      } else {
        commit_patient_profile(profile_name, new_profile)
      }
    })

    shiny::observeEvent(input$cancel_replace_profile, {
      pending_profile_replacement(NULL)
      shiny::removeModal()
    })

    shiny::observeEvent(input$confirm_replace_profile, {
      pending <- pending_profile_replacement()
      shiny::req(!is.null(pending))
      commit_patient_profile(pending$name, pending$profile)
      pending_profile_replacement(NULL)
      shiny::removeModal()
    })

    shiny::observe({
      profile_names <- names(patient_profiles())
      selected <- shiny::isolate(input$remove_profile_name)
      # With no profiles, clear the selector explicitly; otherwise the browser
      # keeps showing the last selected (now deleted) profile.
      shiny::updateSelectInput(
        session, "remove_profile_name", choices = if (length(profile_names)) profile_names else character(),
        selected = if (length(selected) == 1L && selected %in% profile_names) selected
                   else if (length(profile_names)) profile_names[1] else character()
      )
    })

    shiny::observeEvent(input$remove_profile, {
      profile_name <- input$remove_profile_name
      shiny::req(nzchar(profile_name))
      shiny::req(profile_name %in% names(patient_profiles()))
      pending_profile_removal(profile_name)
      shiny::showModal(shiny::modalDialog(
        title = "Delete patient profile?",
        paste0(
          "Are you sure you want to delete profile '",
          profile_name,
          "'? This action cannot be undone."
        ),
        easyClose = FALSE,
        footer = shiny::tagList(
          shiny::actionButton(
            session$ns("cancel_remove_profile"),
            "Cancel",
            class = "btn-default"
          ),
          shiny::actionButton(
            session$ns("confirm_remove_profile"),
            "Yes, delete",
            class = "btn-danger"
          )
        )
      ))
    })

    shiny::observeEvent(input$cancel_remove_profile, {
      pending_profile_removal(NULL)
      shiny::removeModal()
    })

    shiny::observeEvent(input$confirm_remove_profile, {
      profile_name <- pending_profile_removal()
      shiny::req(!is.null(profile_name))
      profiles <- patient_profiles()
      profiles[[profile_name]] <- NULL
      patient_profiles(profiles)
      pending_profile_removal(NULL)
      shiny::removeModal()
    })

    default_profile_percentage <- function(profile_name) {
      probabilities <- loaded_probabilities()
      if (profile_name %in% names(probabilities)) return(100 * probabilities[[profile_name]])
      if (length(patient_profiles()) == 1L) 100 else 0
    }

    output$profile_percent_ui <- shiny::renderUI({
      profiles <- patient_profiles()
      if (length(profiles) == 0) {
        return(shiny::helpText("Create at least one patient profile."))
      }
      shiny::tagList(lapply(names(profiles), function(profile_name) {
        input_id <- percentage_input_id(profile_name)
        percentage <- shiny::isolate(input[[input_id]])
        if (is.null(percentage)) percentage <- default_profile_percentage(profile_name)
        shiny::numericInput(
          session$ns(input_id),
          paste(profile_name, "arrival percentage (%)"),
          value = round(percentage, 1),
          min = 0,
          max = 100,
          step = 0.01
        )
      }))
    })

    entered_profile_percentages <- shiny::reactive({
      profiles <- patient_profiles()
      if (length(profiles) == 0) return(numeric())
      stats::setNames(vapply(names(profiles), function(profile_name) {
        value <- input[[percentage_input_id(profile_name)]]
        exact <- default_profile_percentage(profile_name)
        # Inputs display 1 decimal; an untouched field keeps its exact loaded value.
        if (is.null(value) || isTRUE(abs(value - round(exact, 1)) < 1e-9)) exact else value
      }, numeric(1)), names(profiles))
    })

    shiny::observeEvent(patient_profiles(), {
      # Changing a pathway does not change its arrival probability.
      confirmed <- confirmed_profile_probabilities()
      if (!is.null(confirmed) && !identical(names(confirmed), names(patient_profiles()))) {
        confirmed_profile_probabilities(NULL)
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(entered_profile_percentages(), {
      confirmed <- confirmed_profile_probabilities()
      if (is.null(confirmed)) return()
      entered <- entered_profile_percentages() / 100
      if (!identical(names(confirmed), names(entered)) ||
          any(!is.finite(entered)) ||
          !isTRUE(all.equal(unname(confirmed), unname(entered), tolerance = 1e-10))) {
        confirmed_profile_probabilities(NULL)
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$set_profile_percentages, {
      percentages <- entered_profile_percentages()
      total_percent <- sum(percentages, na.rm = TRUE)
      valid <- length(percentages) > 0 &&
        all(is.finite(percentages)) &&
        all(percentages >= 0) &&
        abs(total_percent - 100) <= 0.01

      if (!valid) {
        confirmed_profile_probabilities(NULL)
        shiny::showNotification(
          sprintf(
            "Arrival percentages were not saved. They must be non-negative and total 100%% (current total: %.2f%%).",
            total_percent
          ),
          type = "error",
          duration = 8
        )
        return()
      }

      confirmed_profile_probabilities(percentages / 100)
      shiny::showNotification(
        "Arrival percentages saved and added to Configuration status.",
        type = "message"
      )
    })

    profile_probabilities <- shiny::reactive({
      probabilities <- confirmed_profile_probabilities()
      profiles <- names(patient_profiles())
      if (is.null(probabilities) || !identical(names(probabilities), profiles)) {
        return(numeric())
      }
      probabilities
    })

    output$profile_percent_status <- shiny::renderUI({
      if (length(patient_profiles()) == 0) return(NULL)
      if (length(profile_probabilities()) == 0) {
        return(shiny::div(
          class = "alert alert-warning",
          "Percentages have not been saved. Enter values totaling 100% and click Set arrival percentages."
        ))
      }
      shiny::div(
        class = "alert alert-success",
        "Arrival percentages saved. Total: 100%."
      )
    })

    shiny::observeEvent(input$add_fallback, {
      primary <- input$fallback_unit
      shiny::req(nzchar(primary))
      fallback_units <- setdiff(input$fallback_options, primary)
      shiny::validate(shiny::need(
        primary %in% selected_units() && length(fallback_units) > 0 &&
          all(fallback_units %in% selected_units()),
        "Select a configured primary unit and at least one configured fallback unit."
      ))
      current <- fallbacks()
      current[[primary]] <- fallback_units
      fallbacks(current)
      shiny::updateSelectInput(session, "fallback_unit", selected = "")
      shiny::updateSelectizeInput(
        session,
        "fallback_options",
        choices = unit_choices(setdiff(selected_units(), names(internal_hospital_units))),
        selected = character(),
        server = TRUE
      )
    })

    shiny::observeEvent(input$remove_fallback, {
      primary <- input$fallback_unit
      shiny::req(primary %in% names(fallbacks()))
      current <- fallbacks()
      current[[primary]] <- NULL
      fallbacks(current)
      shiny::updateSelectizeInput(session, "fallback_options", selected = character())
    })

    output$fallbacks_summary <- shiny::renderTable({
      current <- fallbacks()
      if (length(current) == 0) return(data.frame(Unit = character(), Fallback = character()))
      data.frame(
        Unit = unit_display_label(names(current)),
        Fallback = vapply(current, function(units) paste(unit_display_label(units), collapse = ", "), character(1)),
        row.names = NULL
      )
    })

    capacities <- shiny::reactive({
      units <- selected_units()
      values <- vapply(units, function(unit_name) {
        if (unit_name %in% names(internal_hospital_units)) {
          return(as.numeric(internal_hospital_units[[unit_name]]))
        }
        value <- input[[configuration_input_id("capacity", unit_name)]]
        if (is.null(value)) NA_real_ else value
      }, numeric(1))
      stats::setNames(values, units)
    })

    effective_profile_data <- shiny::reactive({
      source <- configuration_source()
      display_label <- predefined_source_label(source)
      source_label <- if (!is.null(display_label)) {
        paste(display_label, "(editable scenario)")
      } else if (identical(source, "excel_upload")) {
        uploaded <- uploaded_config()
        if (is.null(uploaded)) "Uploaded Excel: select a valid .xlsx file"
        else paste(uploaded$source_label, "(editable scenario)")
      } else if (identical(source, "predefined")) {
        "NDMS-Based Classification profiles for the selected hospital units (editable scenario)"
      } else "Manually entered profiles"
      list(
        source = source,
        source_label = source_label,
        patient_profiles = patient_profiles(),
        profile_prob = profile_probabilities(),
        fallbacks = fallbacks()
      )
    })

    configuration_errors <- shiny::reactive({
      units <- selected_units()
      profile_data <- effective_profile_data()
      profiles <- profile_data$patient_profiles
      probabilities <- profile_data$profile_prob
      active_fallbacks <- profile_data$fallbacks
      capacity_values <- capacities()
      errors <- character()

      if (length(units) == 0) errors <- c(errors, "Select at least one hospital unit.")
      if (isTRUE(require_surge_profiles()) && length(profiles) == 0) errors <- c(errors, "Create at least one patient profile or use predefined profiles.")
      if (length(capacity_values) == 0 || any(!is.finite(capacity_values)) || any(capacity_values < 0)) {
        errors <- c(errors, "Enter a valid non-negative capacity for every selected unit.")
      }
      if (isTRUE(require_surge_profiles()) && (length(probabilities) == 0 || any(!is.finite(probabilities)) ||
          abs(sum(probabilities) - 1) > 1e-4)) {
        errors <- c(
          errors,
          "Enter percentages totaling 100% and click Set arrival percentages."
        )
      }
      profile_units <- unique(unlist(lapply(profiles, `[[`, "unit"), use.names = FALSE))
      if (isTRUE(require_surge_profiles()) && length(setdiff(profile_units, units)) > 0) {
        affected_profiles <- names(profiles)[vapply(profiles, function(profile) {
          any(!profile$unit %in% units)
        }, logical(1))]
        errors <- c(errors, paste(
          "Every trajectory unit must remain selected as a hospital unit. Update or remove these profiles:",
          paste(affected_profiles, collapse = ", ")
        ))
      }
      fallback_units <- unique(c(
        names(active_fallbacks),
        unlist(active_fallbacks, use.names = FALSE)
      ))
      if (length(setdiff(fallback_units, units)) > 0) {
        errors <- c(errors, "Every fallback unit must remain selected as a hospital unit.")
      }
      unique(errors)
    })

    output$configuration_status <- shiny::renderUI({
      errors <- configuration_errors()
      profile_data <- effective_profile_data()
      source_message <- paste("Profile source:", profile_data$source_label)
      if (length(errors) == 0) {
        shiny::div(
          class = "alert alert-success",
          shiny::tags$strong(source_message),
          shiny::tags$br(),
          if (isTRUE(require_surge_profiles())) {
            paste(length(profile_data$patient_profiles), "profiles loaded. Configuration is ready to run.")
          } else {
            "Hospital beds and fallbacks are ready. Configure Routine Civilian Flow below to run without surge patients."
          }
        )
      } else {
        shiny::div(
          class = "alert alert-warning",
          shiny::tags$strong(source_message),
          shiny::tags$br(),
          "Complete the following:",
          shiny::tags$ul(lapply(errors, shiny::tags$li))
        )
      }
    })

    output$profiles_summary <- shiny::renderTable({
      profile_data <- effective_profile_data()
      profiles <- profile_data$patient_profiles
      probabilities <- profile_data$profile_prob
      if (length(profiles) == 0) return(NULL)
      arrival_percent <- if (length(probabilities) == 0) {
        rep("Pending", length(profiles))
      } else {
        paste0(round(100 * probabilities[names(profiles)], 2), "%")
      }
      data.frame(
        Profile = names(profiles),
        Trajectory = vapply(
          profiles,
          function(profile) {
            if (is.null(profile$unit)) "Ambulatory" else paste(unit_display_label(profile$unit), collapse = " -> ")
          },
          character(1)
        ),
        Mean_stay_days = vapply(profiles, function(profile) format_steps(profile$los), character(1)),
        SD_days = vapply(profiles, function(profile) format_steps(profile_step_sd(profile)), character(1)),
        Arrival_percent = arrival_percent,
        check.names = FALSE
      )
    })

    current_configuration <- shiny::reactive({
      if (length(configuration_errors()) > 0) return(NULL)
      profile_data <- effective_profile_data()
      normalized_probabilities <- profile_data$profile_prob / sum(profile_data$profile_prob)
      list(
        source = profile_data$source,
        source_label = profile_data$source_label,
        units = selected_units(),
        capacities = capacities(),
        patient_profiles = profile_data$patient_profiles,
        profile_prob = normalized_probabilities,
        fallbacks = profile_data$fallbacks
      )
    })

    output$download_profile_config <- shiny::downloadHandler(
      filename = function() {
        paste0("hospital_profile_configuration_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        profile_config <- current_configuration()
        shiny::req(!is.null(profile_config))
        shiny::validate(shiny::need(
          length(profile_config$patient_profiles) > 0 &&
            setequal(names(profile_config$patient_profiles), names(profile_config$profile_prob)),
          "Complete surge profiles and arrival percentages before exporting a surge workbook. Civilian profiles are saved separately as CSV in Routine Civilian Flow."
        ))
        write_profile_config_xlsx(profile_config, file)
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )
    output$download_profile_template <- shiny::downloadHandler(
      filename = function() {
        "hospital_profile_template.xlsx"
      },
      content = function(file) {
        write_empty_profile_template_xlsx(file)
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )

    current_configuration
  })
}
