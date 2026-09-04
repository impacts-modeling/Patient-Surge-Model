# CSV pathways and stays use comma-separated values inside quoted CSV fields.
baseline_profiles_to_table <- function(profiles) {
  if (!length(profiles)) {
    return(data.frame(Profile = character(), Patients_per_day = numeric(),
                      Pathway = character(), Mean_stays_days = character()))
  }
  do.call(rbind, lapply(names(profiles), function(name) {
    profile <- profiles[[name]]
    data.frame(Profile = name, Patients_per_day = profile$rate,
               Pathway = paste(profile$unit, collapse = ", "),
               Mean_stays_days = paste(format(profile$los, digits = 15, trim = TRUE),
                                       collapse = ", "))
  }))
}

read_baseline_profiles_csv <- function(file, hospital) {
  columns <- names(baseline_profiles_to_table(list()))
  rows <- utils::read.csv(file, check.names = FALSE, colClasses = "character",
                          fileEncoding = "UTF-8-BOM", na.strings = "",
                          strip.white = TRUE, fill = FALSE)
  if (anyDuplicated(names(rows)) || !all(columns %in% names(rows))) {
    stop("CSV must contain unique columns: ", paste(columns, collapse = ", "), call. = FALSE)
  }
  if (!nrow(rows)) stop("CSV must contain at least one civilian profile.", call. = FALSE)
  rows <- rows[, columns, drop = FALSE]
  rows[] <- lapply(rows, trimws)
  if (anyNA(rows) || any(vapply(rows, function(column) any(!nzchar(column)), logical(1)))) {
    stop("Every profile needs a name, arrival rate, pathway, and mean stays.", call. = FALSE)
  }
  if (anyDuplicated(rows$Profile)) stop("CSV profile names must be unique.", call. = FALSE)
  rates <- suppressWarnings(as.numeric(rows$Patients_per_day))
  if (any(!is.finite(rates)) || any(rates <= 0)) {
    stop("Patients_per_day must contain positive finite numbers. Use a decimal point.", call. = FALSE)
  }
  split_steps <- function(value) {
    if (grepl("(^|,)\\s*(,|$)", value)) {
      stop("Pathway and Mean_stays_days cannot contain empty steps.", call. = FALSE)
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
    unknown <- setdiff(units, hospital$units)
    if (length(unknown)) {
      stop("Profile '", rows$Profile[[index]], "' uses unselected units: ",
           paste(unknown, collapse = ", "), ". Select these hospital units first.", call. = FALSE)
    }
    list(unit = units, los = stays, rate = rates[[index]])
  }), rows$Profile)
  candidate <- baseline_defaults()
  candidate$enabled <- TRUE
  candidate$profiles <- lapply(profiles, function(profile) profile[c("unit", "los")])
  candidate$arrival_rates <- stats::setNames(rates, rows$Profile)
  validate_baseline_config(candidate, hospital$capacities, hospital$fallbacks)
  profiles
}

mod_baseline_ui <- function(id) {
  ns <- shiny::NS(id)
  defaults <- baseline_defaults()
  shinydashboard::box(
    title = "Routine Civilian Flow", width = 12, status = "primary", solidHeader = TRUE,
    shiny::checkboxInput(ns("enabled"), "Enable routine civilian arrivals", FALSE),
    shiny::helpText("Civilian and surge patients share the selected hospital beds and fallback rules. Civilian profiles are stored separately for each hospital source and unit selection."),
    shiny::conditionalPanel(sprintf("input['%s']", ns("enabled")),
      shiny::actionButton(
        ns("use_predefined_profiles"),
        "Use predefined baseline profiles",
        class = "btn-primary"
      ),
      shiny::helpText(paste(
        "Loads the profiles supplied with the app from",
        "R/data/baseline_civilian_profiles.csv and replaces the currently saved civilian profiles."
      )),
      shiny::tags$hr(),
      shiny::fileInput(ns("profile_csv"), "Civilian profiles CSV", accept = ".csv"),
      shiny::helpText(paste(
        "Columns: Profile, Patients_per_day, Pathway, Mean_stays_days. Use decimal points.",
        "Separate pathway units and stays with commas inside each cell",
        "(for example: ICU, GenMed and 8.294710, 0.142857).",
        "Import adds profiles and updates matching names; other saved profiles are kept.",
        "Select the hospital units before importing. Beds and warm-up settings are configured separately."
      )),
      shiny::actionButton(ns("import_csv"), "Import civilian profiles", class = "btn-primary"),
      shiny::downloadButton(ns("download_csv_template"), "Download CSV template"),
      shiny::downloadButton(ns("download_csv"), "Download saved profiles"),
      shiny::uiOutput(ns("import_status")),
      shiny::tags$hr(),
      shiny::fluidRow(
        shiny::column(6,
          shiny::textInput(ns("name"), "Civilian profile name", "routine_medical"),
          shiny::numericInput(ns("rate"), "Arrival rate (patients/day)", 1, min = 0, step = "any"),
          shiny::textInput(ns("units"), "Ordered pathway (unit IDs separated by commas)", "GenMed"),
          shiny::textInput(ns("los"), "Mean stay at each step (days, separated by commas)", "3"),
          shiny::textOutput(ns("available_units")),
          shiny::actionButton(ns("save"), "Save civilian profile", class = "btn-primary"),
          shiny::selectInput(ns("selected"), "Saved civilian profile", choices = character()),
          shiny::actionButton(ns("edit"), "Load profile for editing"),
          shiny::actionButton(ns("remove"), "Remove profile")
        ),
        shiny::column(6,
          shiny::numericInput(ns("warmup_min"), "Minimum warm-up (days)", defaults$warmup_min_days, min = 1),
          shiny::numericInput(ns("warmup_max"), "Maximum warm-up (days)", defaults$warmup_max_days, min = 1),
          shiny::numericInput(ns("window"), "Stability window (days; three windows compared)", defaults$window_days, min = 1),
          shiny::numericInput(ns("occupancy_tolerance"), "Occupancy tolerance (fraction of beds)", defaults$occupancy_tolerance, min = 0.001, step = 0.01),
          shiny::numericInput(ns("queue_tolerance"), "Queue tolerance (patients)", defaults$queue_tolerance, min = 0.01, step = 0.1),
          shiny::helpText("Arrivals are evenly spaced at each profile's constant rate. Stays remain stochastic. Warm-up extends until the screen passes or the maximum is reached. This screen is not proof of equilibrium."),
          shiny::helpText("No patients are removed at surge onset. In expansion searches, additional beds are available during warm-up: this represents planned capacity, not delayed emergency activation.")
        )
      ),
      shiny::tableOutput(ns("profiles")),
      shiny::uiOutput(ns("status"))
    )
  )
}

mod_baseline_server <- function(id, hospital_config) {
  shiny::moduleServer(id, function(input, output, session) {
    saved <- shiny::reactiveVal(list())
    import_status <- shiny::reactiveVal(NULL)
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
        predefined_file <- file.path("R", "data", "baseline_civilian_profiles.csv")
        if (!file.exists(predefined_file)) {
          stop("The predefined baseline profile file is unavailable.")
        }
        read_baseline_profiles_csv(predefined_file, hospital)
      }, error = function(error) error)
      if (inherits(result, "error")) {
        import_status(list(ok = FALSE, message = paste(
          "Predefined profiles were not loaded.", conditionMessage(result))))
        return(invisible(NULL))
      }
      replace_profiles(result)
      import_status(list(ok = TRUE, message = sprintf(
        "Loaded %d predefined baseline profiles. Total arrival rate: %.9g patients/day.",
        length(result), sum(vapply(result, `[[`, numeric(1), "rate")))))
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
    output$available_units <- shiny::renderText({
      hospital <- hospital_config()
      if (is.null(hospital)) return("Complete a valid surge/hospital configuration first.")
      paste("Available unit IDs:", paste(hospital$units, collapse = ", "))
    })
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
      units <- trimws(strsplit(input$units, ",", fixed = TRUE)[[1]])
      los <- suppressWarnings(as.numeric(trimws(strsplit(input$los, ",", fixed = TRUE)[[1]])))
      candidate <- baseline_defaults()
      candidate$enabled <- TRUE
      candidate$profiles <- stats::setNames(list(list(unit = units, los = los)), name)
      candidate$arrival_rates <- stats::setNames(input$rate, name)
      error <- tryCatch({
        if (!nzchar(name)) stop("Enter a civilian profile name.")
        validate_baseline_config(candidate, hospital$capacities, hospital$fallbacks)
        NULL
      }, error = function(error) conditionMessage(error))
      if (!is.null(error)) {
        shiny::showNotification(error, type = "error")
        return(invisible(NULL))
      }
      all <- saved()
      current <- profiles()
      current[[name]] <- list(unit = units, los = los, rate = input$rate)
      all[[key()]] <- current
      saved(all)
    })
    shiny::observeEvent(input$edit, {
      profile <- profiles()[[input$selected]]
      shiny::req(profile)
      shiny::updateTextInput(session, "name", value = input$selected)
      shiny::updateTextInput(session, "units", value = paste(profile$unit, collapse = ", "))
      shiny::updateTextInput(session, "los", value = paste(profile$los, collapse = ", "))
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
      config$profiles <- lapply(profiles(), function(profile) profile[c("unit", "los")])
      config$arrival_rates <- vapply(profiles(), `[[`, numeric(1), "rate")
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
                   Mean_stays_days = paste(profile$los, collapse = " -> "))
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
