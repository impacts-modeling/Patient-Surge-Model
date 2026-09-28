# CSV pathways, stays and SDs use comma-separated values inside quoted CSV
# fields. SD_values (days) is optional and is converted to CV = SD / mean stay
# (see step_cv_from_sd() in R/app/mod_profiles.R). Files written before the
# switch to SD carry CV_values instead, which is still read as-is. A blank or
# absent entry defaults every step to CV 1 for ICU and 0.24 for every other
# unit (default_cv_for_unit() from R/shared/profiles_deloitte.R).

# Civilian profiles always run with profile$cv, or default_cv_for_unit() when
# it is absent (see configuration() in mod_baseline_server()).
civilian_step_cv <- function(profile) {
  if (!is.null(profile$cv)) profile$cv else default_cv_for_unit(profile$unit)
}

civilian_step_sd <- function(profile) civilian_step_cv(profile) * profile$los

baseline_profiles_to_table <- function(profiles) {
  if (!length(profiles)) {
    return(data.frame(Profile = character(), Patients_per_day = numeric(),
                      Pathway = character(), Mean_stays_days = character(),
                      SD_values = character()))
  }
  do.call(rbind, lapply(names(profiles), function(name) {
    profile <- profiles[[name]]
    data.frame(Profile = name, Patients_per_day = profile$rate,
               Pathway = paste(profile$unit, collapse = ", "),
               Mean_stays_days = paste(format(profile$los, digits = 15, trim = TRUE),
                                       collapse = ", "),
               SD_values = paste(format(civilian_step_sd(profile), digits = 15, trim = TRUE),
                                 collapse = ", "))
  }))
}

read_baseline_profiles_csv <- function(file, hospital) {
  required_columns <- c("Profile", "Patients_per_day", "Pathway", "Mean_stays_days")
  rows <- utils::read.csv(file, check.names = FALSE, colClasses = "character",
                          fileEncoding = "UTF-8-BOM", na.strings = "",
                          strip.white = TRUE, fill = FALSE)
  if (anyDuplicated(names(rows)) || !all(required_columns %in% names(rows))) {
    stop("CSV must contain unique columns: ", paste(required_columns, collapse = ", "), call. = FALSE)
  }
  if (!nrow(rows)) stop("CSV must contain at least one civilian profile.", call. = FALSE)
  variability_columns <- intersect(c("SD_values", "CV_values"), names(rows))
  rows <- rows[, c(required_columns, variability_columns), drop = FALSE]
  rows[required_columns] <- lapply(rows[required_columns], trimws)
  rows[variability_columns] <- lapply(rows[variability_columns], trimws)
  if (anyNA(rows[required_columns]) ||
      any(vapply(rows[required_columns], function(column) any(!nzchar(column)), logical(1)))) {
    stop("Every profile needs a name, arrival rate, pathway, and mean stays.", call. = FALSE)
  }
  if (anyDuplicated(rows$Profile)) stop("CSV profile names must be unique.", call. = FALSE)
  rates <- suppressWarnings(as.numeric(rows$Patients_per_day))
  if (any(!is.finite(rates)) || any(rates <= 0)) {
    stop("Patients_per_day must contain positive finite numbers. Use a decimal point.", call. = FALSE)
  }
  split_steps <- function(value) {
    if (grepl("(^|,)\\s*(,|$)", value)) {
      stop("Pathway, Mean_stays_days, SD_values and CV_values cannot contain empty steps.", call. = FALSE)
    }
    trimws(strsplit(value, ",", fixed = TRUE)[[1]])
  }
  profiles <- stats::setNames(lapply(seq_len(nrow(rows)), function(index) {
    units <- split_steps(rows$Pathway[[index]])
    stays <- suppressWarnings(as.numeric(split_steps(rows$Mean_stays_days[[index]])))
    if (length(units) != length(stays) || any(!is.finite(stays)) || any(stays <= 0)) {
      stop("Profile '", rows$Profile[[index]], "' needs one positive mean stay per pathway step.",
           call. = FALSE)
    }
    # Per row, a filled SD_values entry takes precedence over CV_values.
    entry_of <- function(column) {
      if (!column %in% variability_columns) return(NA_character_)
      value <- rows[[column]][[index]]
      if (is.na(value) || !nzchar(value)) NA_character_ else value
    }
    parse_steps <- function(value) {
      steps <- suppressWarnings(as.numeric(split_steps(value)))
      if (length(steps) == 1L && length(units) > 1L) steps <- rep(steps, length(units))
      steps
    }
    sd_entry <- entry_of("SD_values")
    cv_entry <- entry_of("CV_values")
    cv <- if (!is.na(sd_entry)) {
      sd <- parse_steps(sd_entry)
      if (length(sd) == length(stays)) sd / stays else NA_real_
    } else if (!is.na(cv_entry)) {
      parse_steps(cv_entry)
    } else {
      default_cv_for_unit(units)
    }
    if (length(cv) != length(units) || any(!is.finite(cv)) || any(cv <= 0)) {
      stop("Profile '", rows$Profile[[index]], "' needs one positive SD (days) per pathway step, ",
           "or leave SD_values blank to default to SD = 1 x mean stay for ICU steps and ",
           "0.24 x mean stay for other steps.", call. = FALSE)
    }
    unknown <- setdiff(units, hospital$units)
    if (length(unknown)) {
      stop("Profile '", rows$Profile[[index]], "' uses unselected units: ",
           paste(unknown, collapse = ", "), ". Select these hospital units first.", call. = FALSE)
    }
    list(unit = units, los = stays, cv = cv, rate = rates[[index]])
  }), rows$Profile)
  candidate <- baseline_defaults()
  candidate$enabled <- TRUE
  candidate$profiles <- lapply(profiles, function(profile) profile[c("unit", "los", "cv")])
  candidate$arrival_rates <- stats::setNames(rates, rows$Profile)
  validate_baseline_config(candidate, hospital$capacities, hospital$fallbacks)
  profiles
}

mod_baseline_ui <- function(id) {
  ns <- shiny::NS(id)
  defaults <- baseline_defaults()
  shiny::tagList(
    shiny::fluidRow(
      rintrojs::introBox(shinydashboard::box(
        title = "Routine Civilian Flow", width = 12, status = "primary", solidHeader = TRUE,
        collapsible = TRUE,
        shiny::checkboxInput(ns("enabled"), "Enable routine civilian arrivals", FALSE),
        shiny::helpText("Civilian and surge patients share the selected hospital beds and fallback rules. Civilian profiles are stored separately for each hospital source and unit selection."),
        shiny::conditionalPanel(sprintf("input['%s']", ns("enabled")),
          shiny::actionButton(
            ns("use_predefined_profiles"),
            "Use predefined civilian profiles",
            class = "btn-primary"
          ),
          shiny::helpText(paste(
            "Loads the UC Davis-based civilian profiles that match the predefined hospital selected",
            "above (Regional, Tertiary, or Community acute-care hospital) and replaces the currently",
            "saved civilian profiles. Any other source uses data/baseline_civilian_profiles.csv",
            "(ED, General Medicine, Inpatient Surge, and ICU). Every unit in those pathways must be",
            "selected (ED is always included). Review rates and stays for your hospital."
          )),
          shiny::tags$hr(),
          shiny::fluidRow(
            shiny::column(
              width = 4,
              shiny::selectInput(ns("selected"), "Saved civilian profile", choices = character()),
              shiny::actionButton(ns("edit"), "Load profile for editing"),
              shiny::actionButton(ns("remove"), "Remove profile"),
              shiny::hr(),
              shiny::textInput(ns("name"), "Civilian profile name", "routine_medical"),
              shiny::numericInput(ns("rate"), "Arrival rate (patients/day)", 1, min = 0, step = "any")
            ),
            shiny::column(
              width = 8,
              shiny::tags$label("Ordered pathway"),
              shiny::fluidRow(
                shiny::column(4, shiny::uiOutput(ns("pathway_units_ui"))),
                shiny::column(4, shiny::uiOutput(ns("pathway_los_ui"))),
                shiny::column(4, shiny::uiOutput(ns("pathway_sd_ui")))
              ),
              pathway_sd_help(),
              shiny::actionButton(ns("add_step"), "Add unit", icon = shiny::icon("plus"),
                                  class = "btn-default"),
              shiny::actionButton(ns("remove_step"), "Remove last unit"),
              shiny::actionButton(ns("save"), "Save civilian profile", class = "btn-primary")
            )
          ),
          shiny::tags$hr(),
          shiny::tableOutput(ns("profiles")),
          shiny::uiOutput(ns("status")),
          shiny::tags$hr(),
          shiny::fileInput(ns("profile_csv"), "Civilian profiles CSV", accept = ".csv"),
          shiny::helpText(paste(
            "Columns: Profile, Patients_per_day, Pathway, Mean_stays_days, and optional SD_values (days).",
            "Use decimal points. Separate pathway units, stays and SDs with commas inside each cell",
            "(for example: ICU, GenMed and 8.294710, 0.142857).",
            "SD_values may be left blank, or omitted entirely, to default to SD = 1 x mean stay for",
            "ICU steps and 0.24 x mean stay for every other unit. Older files with CV_values are still accepted.",
            "Import adds profiles and updates matching names; other saved profiles are kept.",
            "Select the hospital units before importing. Beds and warm-up settings are configured separately."
          )),
          shiny::actionButton(ns("import_csv"), "Import civilian profiles", class = "btn-primary"),
          shiny::downloadButton(ns("download_csv_template"), "Download CSV template"),
          shiny::downloadButton(ns("download_csv"), "Download saved profiles"),
          shiny::uiOutput(ns("import_status"))
        )
      ), id = ns("tour_routine_flow"), data.step = 7,
        data.intro = paste(
          "<strong>Routine civilian operation and warm-up.</strong><br>",
          "Enable this flow for a populated hospital before the surge. Civilian arrivals continue during the event.",
          "Predefined civilian profiles follow the selected predefined hospital (Regional, Tertiary or Community); other sources use the ED, General Medicine, Inpatient Surge and ICU set.",
          "Enter a rate and build the ordered pathway unit by unit, with a mean stay and SD (days) for each unit.",
          "Arrival process and warm-up settings are under Advanced Flow Settings. Fixed warm-up continues even if its diagnostic fails; adaptive mode must pass.",
          "Patients and queues remain at day zero. Additional beds activate then. Review baseline stability before comparing surge effects."
        ), data.position = "top")
    ),
    shiny::fluidRow(
      shinydashboard::box(
        title = "Advanced Flow Settings", width = 12, status = "primary", solidHeader = TRUE,
        collapsible = TRUE, collapsed = TRUE,
        shiny::helpText("These settings apply only when routine civilian arrivals are enabled."),
        shiny::fluidRow(
          shiny::column(6,
            shiny::selectInput(ns("arrival_process"), "Civilian arrival process",
              choices = c("Evenly spaced" = "even", "Poisson (random arrivals)" = "poisson")),
            shiny::selectInput(ns("warmup_mode"), "Warm-up method",
              choices = c("Fixed duration with diagnostics" = "fixed", "Adaptive stability screen" = "adaptive"),
              selected = defaults$warmup_mode),
            shiny::numericInput(ns("warmup_min"), "Minimum warm-up (days)", defaults$warmup_min_days, min = 1),
            shiny::numericInput(ns("warmup_max"), "Maximum warm-up (days)", defaults$warmup_max_days, min = 1)
          ),
          shiny::column(6,
            shiny::numericInput(ns("window"), "Stability window (days; three windows compared)", defaults$window_days, min = 1),
            shiny::numericInput(ns("occupancy_tolerance"), "Occupancy tolerance (fraction of beds)", defaults$occupancy_tolerance, min = 0.001, step = 0.01),
            shiny::numericInput(ns("queue_tolerance"), "Queue tolerance (patients)", defaults$queue_tolerance, min = 0.01, step = 0.1)
          )
        ),
        shiny::helpText("Poisson uses each profile's mean patients/day and random exponential interarrival times. Fixed warm-up uses the minimum duration and retains the diagnostic even if it fails. Adaptive warm-up extends until the screen passes or the maximum is reached. Neither method proves equilibrium."),
        shiny::helpText("No patients are removed at surge onset. The civilian warm-up uses existing capacity; additional beds are activated when the surge begins.")
      )
    )
  )
}

mod_baseline_server <- function(id, hospital_config) {
  shiny::moduleServer(id, function(input, output, session) {
    saved <- shiny::reactiveVal(list())
    import_status <- shiny::reactiveVal(NULL)
    step_count <- shiny::reactiveVal(1L)
    form_version <- shiny::reactiveVal(1L)
    pathway_draft <- shiny::reactiveVal(list(unit = "GenMed", los = 3, sd = NA_real_))
    key <- shiny::reactive({
      hospital <- hospital_config()
      if (is.null(hospital)) return("pending")
      paste(hospital$source, hospital$source_label, paste(sort(hospital$units), collapse = ","), sep = "|")
    })
    profiles <- shiny::reactive({
      value <- saved()[[key()]]
      if (is.null(value)) list() else value
    })
    replace_profiles <- function(value) {
      all <- saved()
      all[[key()]] <- value
      saved(all)
    }
    shiny::observeEvent(list(key(), input$profile_csv), {
      import_status(NULL)
    }, ignoreNULL = FALSE)
    shiny::observeEvent(input$use_predefined_profiles, {
      hospital <- hospital_config()
      result <- tryCatch({
        if (is.null(hospital)) stop("Complete the hospital configuration before loading profiles.")
        predefined_file <- civilian_profile_file(hospital$source)
        if (!file.exists(predefined_file)) {
          stop("The predefined baseline profile file is unavailable.")
        }
        list(file = predefined_file, profiles = read_baseline_profiles_csv(predefined_file, hospital))
      }, error = function(error) error)
      if (inherits(result, "error")) {
        import_status(list(ok = FALSE, message = paste(
          "Predefined profiles were not loaded.", conditionMessage(result))))
        return(invisible(NULL))
      }
      replace_profiles(result$profiles)
      import_status(list(ok = TRUE, message = sprintf(
        "Loaded %d predefined baseline profiles from %s. Total arrival rate: %.9g patients/day.",
        length(result$profiles), result$file,
        sum(vapply(result$profiles, `[[`, numeric(1), "rate")))))
    })
    shiny::observeEvent(input$import_csv, {
      hospital <- hospital_config()
      result <- tryCatch({
        if (is.null(hospital)) stop("Complete the hospital configuration before importing.")
        if (is.null(input$profile_csv)) stop("Select a civilian profiles CSV first.")
        if (tolower(tools::file_ext(input$profile_csv$name)) != "csv") {
          stop("Select a .csv file.")
        }
        read_baseline_profiles_csv(input$profile_csv$datapath, hospital)
      }, error = function(error) error)
      if (inherits(result, "error")) {
        import_status(list(ok = FALSE, message = paste(
          "Import failed. Saved profiles were not changed.", conditionMessage(result))))
        return(invisible(NULL))
      }
      # Validate the complete upload before changing the active hospital's profiles.
      current <- profiles()
      updated <- sum(names(result) %in% names(current))
      current[names(result)] <- result
      replace_profiles(current)
      import_status(list(ok = TRUE, message = sprintf(
        "Imported %d profiles: %d added, %d updated. Total saved arrival rate: %.9g patients/day.",
        length(result), length(result) - updated, updated,
        sum(vapply(current, `[[`, numeric(1), "rate")))))
    })
    output$import_status <- shiny::renderUI({
      status <- import_status()
      if (is.null(status)) return(NULL)
      shiny::div(class = if (status$ok) "alert alert-success" else "alert alert-warning",
                 status$message)
    })
    output$download_csv_template <- shiny::downloadHandler(
      filename = function() "civilian_profiles_template.csv",
      content = function(file) {
        utils::write.csv(baseline_profiles_to_table(list()), file, row.names = FALSE,
                         fileEncoding = "UTF-8")
      },
      contentType = "text/csv"
    )
    output$download_csv <- shiny::downloadHandler(
      filename = function() "civilian_profiles.csv",
      content = function(file) {
        shiny::req(length(profiles()) > 0)
        utils::write.csv(baseline_profiles_to_table(profiles()), file, row.names = FALSE,
                         fileEncoding = "UTF-8")
      },
      contentType = "text/csv"
    )
    # Pathway editor: same step inputs as the surge trajectory editor.
    output$pathway_units_ui <- shiny::renderUI({
      hospital <- hospital_config()
      if (is.null(hospital)) return(shiny::helpText("Complete a valid surge/hospital configuration first."))
      pathway_step_inputs(input, session, "unit", step_count(), form_version(),
                          pathway_draft(), hospital$units)
    })
    output$pathway_los_ui <- shiny::renderUI({
      pathway_step_inputs(input, session, "los", step_count(), form_version(), pathway_draft())
    })
    output$pathway_sd_ui <- shiny::renderUI({
      pathway_step_inputs(input, session, "sd", step_count(), form_version(), pathway_draft())
    })
    shiny::observeEvent(input$add_step, step_count(step_count() + 1L))
    shiny::observeEvent(input$remove_step, step_count(max(1L, step_count() - 1L)))
    shiny::observe({
      names <- names(profiles())
      selected <- shiny::isolate(input$selected)
      shiny::updateSelectInput(session, "selected", choices = names,
                                selected = if (length(selected) == 1L && selected %in% names) selected else names[1])
    })
    shiny::observeEvent(input$save, {
      hospital <- hospital_config()
      shiny::req(hospital)
      name <- trimws(input$name)
      steps <- read_pathway_steps(input, step_count(), form_version())
      cv <- step_cv_from_sd(steps$unit, steps$los, steps$sd)
      candidate <- baseline_defaults()
      candidate$enabled <- TRUE
      candidate$profiles <- stats::setNames(list(list(unit = steps$unit, los = steps$los, cv = cv)), name)
      candidate$arrival_rates <- stats::setNames(input$rate, name)
      error <- tryCatch({
        if (!nzchar(name)) stop("Enter a civilian profile name.")
        step_error <- pathway_step_error(steps, hospital$units)
        if (!is.null(step_error)) stop(step_error)
        validate_baseline_config(candidate, hospital$capacities, hospital$fallbacks)
        NULL
      }, error = function(error) conditionMessage(error))
      if (!is.null(error)) {
        shiny::showNotification(error, type = "error")
        return(invisible(NULL))
      }
      all <- saved()
      current <- profiles()
      current[[name]] <- list(unit = steps$unit, los = steps$los, cv = cv, rate = input$rate)
      all[[key()]] <- current
      saved(all)
    })
    shiny::observeEvent(input$edit, {
      profile <- profiles()[[input$selected]]
      shiny::req(profile)
      pathway_draft(list(unit = profile$unit, los = profile$los, sd = civilian_step_sd(profile)))
      step_count(max(1L, length(profile$unit)))
      form_version(form_version() + 1L)
      shiny::updateTextInput(session, "name", value = input$selected)
      shiny::updateNumericInput(session, "rate", value = profile$rate)
    })
    shiny::observeEvent(input$remove, {
      shiny::req(input$selected)
      all <- saved()
      current <- profiles()
      current[[input$selected]] <- NULL
      all[[key()]] <- current
      saved(all)
    })
    configuration <- shiny::reactive({
      config <- baseline_defaults()
      config$enabled <- isTRUE(input$enabled)
      if (!config$enabled) return(config)
      config$profiles <- lapply(profiles(), function(profile) {
        list(unit = profile$unit, los = profile$los, cv = civilian_step_cv(profile))
      })
      config$arrival_rates <- vapply(profiles(), `[[`, numeric(1), "rate")
      config$arrival_process <- if (is.null(input$arrival_process)) "even" else input$arrival_process
      config$warmup_mode <- if (is.null(input$warmup_mode)) "fixed" else input$warmup_mode
      config$warmup_min_days <- input$warmup_min
      config$warmup_max_days <- input$warmup_max
      config$window_days <- input$window
      config$occupancy_tolerance <- input$occupancy_tolerance
      config$queue_tolerance <- input$queue_tolerance
      config
    })
    output$profiles <- shiny::renderTable({
      dplyr::bind_rows(lapply(names(profiles()), function(name) {
        profile <- profiles()[[name]]
        data.frame(Profile = name, Patients_per_day = profile$rate,
                   Pathway = paste(profile$unit, collapse = " -> "),
                   Mean_stays_days = format_steps(profile$los),
                   SD_days = format_steps(civilian_step_sd(profile)))
      }))
    })
    output$status <- shiny::renderUI({
      hospital <- hospital_config()
      shiny::req(hospital)
      error <- tryCatch({
        validate_baseline_config(configuration(), hospital$capacities, hospital$fallbacks)
        NULL
      }, error = function(error) conditionMessage(error))
      shiny::div(class = if (is.null(error)) "alert alert-success" else "alert alert-warning",
                  if (is.null(error)) "Civilian configuration is ready." else error)
    })
    configuration
  })
}
