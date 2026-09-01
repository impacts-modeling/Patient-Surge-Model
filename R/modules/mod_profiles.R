# Names are display labels; values are stable internal resource identifiers.
hospital_profile_units <- c(
  "Surge" = "Surge",
  "GenMed" = "GenMed",
  "ICU" = "ICU",
  "BurnBed" = "BurnBed",
  "Cardiac ICU" = "CardiacICU",
  "PhysicalMed" = "PhysicalMed",
  "Psychiatric" = "Psychiatric",
  "TransitionalCare" = "TransitionalCare"
)

profile_excel_sheet_columns <- list(
  Profiles = c("Profile", "Arrival_percent", "Ambulatory"),
  Trajectories = c("Profile", "Step", "Unit", "LOS_days"),
  Fallbacks = c("Primary_unit", "Priority", "Fallback_unit"),
  Hospital = c("Unit", "Available_beds")
)

profile_config_to_excel_tables <- function(profile_config) {
  profile_names <- names(profile_config$patient_profiles)
  trajectory_rows <- lapply(profile_names, function(profile_name) {
    profile <- profile_config$patient_profiles[[profile_name]]
    if (is.null(profile$unit) || length(profile$unit) == 0) return(NULL)
    data.frame(Profile = profile_name, Step = seq_along(profile$unit),
               Unit = profile$unit, LOS_days = profile$los, check.names = FALSE)
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
      data.frame(Profile = character(), Step = integer(), Unit = character(), LOS_days = numeric())
    } else do.call(rbind, trajectory_rows),
    Fallbacks = if (length(fallback_rows) == 0) {
      data.frame(Primary_unit = character(), Priority = integer(), Fallback_unit = character())
    } else do.call(rbind, fallback_rows),
    Hospital = data.frame(
      Unit = profile_config$units,
      Available_beds = unname(profile_config$capacities[profile_config$units]),
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
    Trajectories = list(column = 4, format = "0.00"),
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
    missing_columns <- setdiff(profile_excel_sheet_columns[[sheet_name]], names(value))
    if (length(missing_columns) > 0) {
      stop("Sheet '", sheet_name, "' is missing column(s): ",
           paste(missing_columns, collapse = ", "), call. = FALSE)
    }
    value[, profile_excel_sheet_columns[[sheet_name]], drop = FALSE]
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
  hospital$Available_beds <- suppressWarnings(as.numeric(hospital$Available_beds))
  if (nrow(hospital) == 0) stop("Sheet 'Hospital' must contain at least one unit.", call. = FALSE)
  if (any(!hospital$Unit %in% hospital_profile_units)) {
    stop("Unknown hospital unit(s): ",
         paste(setdiff(unique(hospital$Unit), hospital_profile_units), collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(hospital$Unit)) stop("Hospital units must be unique.", call. = FALSE)
  if (any(!is.finite(hospital$Available_beds)) || any(hospital$Available_beds < 0)) {
    stop("'Available_beds' must contain non-negative numbers.", call. = FALSE)
  }

  trajectories <- tables$Trajectories
  trajectories$Profile <- trimws(as.character(trajectories$Profile))
  trajectories$Step <- suppressWarnings(as.numeric(trajectories$Step))
  trajectories$Unit <- trimws(as.character(trajectories$Unit))
  trajectories$LOS_days <- suppressWarnings(as.numeric(trajectories$LOS_days))
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
      patient_profiles[[profile_name]] <- list(unit = rows$Unit, los = rows$LOS_days)
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
    capacities = stats::setNames(hospital$Available_beds, hospital$Unit),
    patient_profiles = patient_profiles,
    profile_prob = stats::setNames(profiles$Arrival_percent / 100, profiles$Profile),
    fallbacks = fallbacks
  )
}
hospital_profiles_ui <- function(id) {
  ns <- shiny::NS(id)
  manual_condition <- sprintf("input['%s'] == 'manual'", ns("profile_source"))
  test_condition <- sprintf("input['%s'] != 'manual'", ns("profile_source"))
  deloitte_condition <- sprintf("input['%s'] == 'deloitte_test'", ns("profile_source"))
  injury_test_condition <- sprintf("input['%s'] == 'injury_path_test'", ns("profile_source"))
  excel_condition <- sprintf("input['%s'] == 'excel_upload'", ns("profile_source"))

  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(
        title = "Hospital information",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        rintrojs::introBox(
          shiny::radioButtons(
            ns("profile_source"),
            "Patient profile source",
            choices = c(
              "Create profiles manually" = "manual",
              "Use Deloitte profiles (completed)" = "injury_path_test",
              "Use Deloitte profiles (reduced)" = "deloitte_test",
              "Upload profiles from Excel" = "excel_upload"
            ),
            selected = "manual",
            inline = TRUE
          ),
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
          shiny::fluidRow(
            shiny::column(
              width = 4,
              shiny::checkboxGroupInput(
                ns("hospital_units"),
                "Hospital units",
                choices = hospital_profile_units,
                selected = c("GenMed", "ICU")
              )
            ),
            shiny::column(
              width = 8,
              shiny::uiOutput(ns("capacity_ui"))
            )
          ),
          data.step = 2,
          data.intro = paste(
            "<strong>Choose the patient profile source.</strong><br>",
            "Create profiles manually, load a built-in example, or upload",
            "an Excel configuration. Download the empty Excel template when",
            "you need the required sheets and columns. Then select hospital",
            "units and enter the available beds for each unit."
          ),
          data.position = "bottom"
        )
      ),
      shinydashboard::box(
        title = "Create patient trajectory",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        rintrojs::introBox(
          shiny::conditionalPanel(
            condition = manual_condition,
            shiny::textInput(ns("profile_name"), "Patient profile name", "profile_1"),
            shiny::fluidRow(
              shiny::column(6, shiny::uiOutput(ns("trajectory_units_ui"))),
              shiny::column(6, shiny::uiOutput(ns("trajectory_los_ui")))
            ),
            shiny::actionButton(
              ns("add_trajectory_unit"),
              "Add unit",
              icon = shiny::icon("plus"),
              class = "btn-default"
            ),
            shiny::actionButton(ns("add_profile"), "Save patient profile", class = "btn-primary"),
            shiny::hr(),
            shiny::selectInput(ns("remove_profile_name"), "Profile to remove", choices = NULL),
            shiny::actionButton(ns("remove_profile"), "Remove selected profile", class = "btn-danger")
          ),
          shiny::conditionalPanel(
            condition = test_condition,
            shiny::div(
              class = "alert alert-info",
              "The selected or uploaded trajectories and arrival probabilities are loaded automatically."
            )
          ),
          data.step = 3,
          data.intro = paste(
            "<strong>Define each patient trajectory.</strong><br>",
            "For manual profiles, enter a unique name and the ordered units",
            "visited by the patient. Enter the mean length of stay for every",
            "unit and use Add unit when another care step is needed."
          ),
          data.position = "bottom"
        )
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        title = "Profile arrival percentages",
        status = "info",
        solidHeader = TRUE,
        width = 4,
        rintrojs::introBox(
          shiny::conditionalPanel(
            condition = manual_condition,
            shiny::uiOutput(ns("profile_percent_ui")),
            shiny::actionButton(
              ns("set_profile_percentages"),
              "Set arrival percentages",
              class = "btn-primary"
            ),
            shiny::uiOutput(ns("profile_percent_status"))
          ),
          shiny::conditionalPanel(
            condition = test_condition,
            shiny::helpText("Arrival percentages are loaded from the selected or uploaded data.")
          ),
          data.step = 4,
          data.intro = paste(
            "<strong>Set the patient mix.</strong><br>",
            "Assign the percentage of arrivals belonging to each profile.",
            "For a valid configuration, all percentages must sum to 100%."
          ),
          data.position = "right"
        )
      ),
      shinydashboard::box(
        title = "Fallbacks",
        status = "info",
        solidHeader = TRUE,
        width = 4,
        rintrojs::introBox(
          shiny::conditionalPanel(
            condition = manual_condition,
            shiny::selectInput(ns("fallback_unit"), "Primary unit", choices = NULL),
            shiny::selectizeInput(
              ns("fallback_options"), "Fallback units", choices = NULL, multiple = TRUE
            ),
            shiny::actionButton(ns("add_fallback"), "Save fallback", class = "btn-primary"),
            shiny::verbatimTextOutput(ns("fallbacks_summary"))
          ),
          shiny::conditionalPanel(
            condition = deloitte_condition,
            shiny::helpText("No fallback units are included in the Deloitte test configuration.")
          ),
          shiny::conditionalPanel(
            condition = injury_test_condition,
            shiny::helpText("Fallback units are loaded automatically from fallbacks_list_2.")
          ),
          shiny::conditionalPanel(
            condition = test_condition,
            shiny::verbatimTextOutput(ns("active_fallbacks_summary"))
          ),
          data.step = 5,
          data.intro = paste(
            "<strong>Configure fallback beds.</strong><br>",
            "When a primary unit is full, the model tries these alternatives",
            "in the displayed order. If none is available, the patient is",
            "counted in the primary-unit queue, waits one day, and checks the",
            "primary unit and ordered fallbacks again until a bed is available."
          ),
          data.position = "top"
        )
      ),
      shinydashboard::box(
        title = "Configuration status",
        status = "success",
        solidHeader = TRUE,
        width = 4,
        rintrojs::introBox(
          shiny::uiOutput(ns("configuration_status")),
          shiny::tableOutput(ns("profiles_summary")),
          shiny::downloadButton(
            ns("download_profile_config"),
            "Download profile configuration",
            class = "btn-primary"
          ),
          shiny::helpText("Save the complete configuration as an Excel file for a future simulation."),
          data.step = 6,
          data.intro = paste(
            "<strong>Confirm that the setup is ready.</strong><br>",
            "This panel lists configuration problems, summarizes all profiles,",
            "and lets you download the complete setup for future simulations."
          ),
          data.position = "left"
        )
      )
    )
  )
}

hospital_profiles_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    patient_profiles <- shiny::reactiveVal(list())
    fallbacks <- shiny::reactiveVal(list())
    confirmed_profile_probabilities <- shiny::reactiveVal(NULL)
    deloitte_config <- deloitte_test_profile_config()
    injury_path_config <- injury_path_test_profile_config(wia_prob = 0.67)
    trajectory_unit_count <- shiny::reactiveVal(1L)
    trajectory_form_version <- shiny::reactiveVal(1L)
    pending_profile_replacement <- shiny::reactiveVal(NULL)
    pending_profile_removal <- shiny::reactiveVal(NULL)
    uploaded_config <- shiny::reactiveVal(NULL)

    selected_test_config <- shiny::reactive({
      switch(
        input$profile_source,
        deloitte_test = deloitte_config,
        injury_path_test = injury_path_config,
        excel_upload = uploaded_config(),
        NULL
      )
    })

    shiny::observeEvent(input$profile_excel_file, {
      uploaded_file <- input$profile_excel_file
      shiny::req(!is.null(uploaded_file$datapath))
      tryCatch(
        {
          imported_config <- read_profile_config_xlsx(uploaded_file$datapath)
          imported_config$source_label <- paste("Uploaded Excel:", uploaded_file$name)
          uploaded_config(imported_config)
          shiny::updateCheckboxGroupInput(
            session, "hospital_units", selected = imported_config$units
          )
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

    shiny::observeEvent(input$profile_source, {
      test_config <- selected_test_config()
      if (!is.null(test_config)) {
        shiny::updateCheckboxGroupInput(
          session,
          "hospital_units",
          selected = test_config$units
        )
      }
    }, ignoreInit = TRUE)

    selected_units <- shiny::reactive({
      units <- input$hospital_units
      if (is.null(units)) character() else unique(units)
    })

    output$capacity_ui <- shiny::renderUI({
      units <- selected_units()
      shiny::req(length(units) > 0)
      shiny::tagList(
        shiny::h4("Available beds"),
        shiny::fluidRow(
          lapply(units, function(unit_name) {
            active_config <- selected_test_config()
            configured_capacity <- if (is.null(active_config)) {
              numeric()
            } else {
              unname(active_config$capacities[unit_name])
            }
            default_capacity <- if (length(configured_capacity) == 1 &&
                                    is.finite(configured_capacity)) {
              configured_capacity
            } else if (unit_name == "GenMed") {
              15
            } else {
              7
            }
            shiny::column(
              width = 6,
              shiny::numericInput(
                session$ns(paste0("capacity_", unit_name)),
                unit_name,
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
      units <- selected_units()
      primary <- input$fallback_unit
      selected_primary <- if (!is.null(primary) && primary %in% units) primary else ""
      shiny::updateSelectInput(
        session,
        "fallback_unit",
        choices = c("Select a primary unit" = "", units),
        selected = selected_primary
      )
      fallback_choices <- setdiff(units, selected_primary)
      selected_fallbacks <- intersect(
        shiny::isolate(input$fallback_options),
        fallback_choices
      )
      shiny::updateSelectizeInput(
        session,
        "fallback_options",
        choices = fallback_choices,
        selected = selected_fallbacks,
        server = TRUE
      )
    })

    shiny::observeEvent(selected_units(), {
      units <- selected_units()
      current <- fallbacks()
      valid_primary_units <- intersect(names(current), units)
      cleaned <- lapply(current[valid_primary_units], function(fallback_units) {
        intersect(fallback_units, units)
      })
      cleaned <- Filter(function(fallback_units) length(fallback_units) > 0, cleaned)
      if (!identical(current, cleaned)) fallbacks(cleaned)
    }, ignoreInit = TRUE)

    trajectory_input_id <- function(prefix, index, version = trajectory_form_version()) {
      paste(prefix, version, index, sep = "_")
    }

    output$trajectory_units_ui <- shiny::renderUI({
      units <- selected_units()
      shiny::req(length(units) > 0)
      version <- trajectory_form_version()
      count <- trajectory_unit_count()
      shiny::tagList(lapply(seq_len(count), function(index) {
        input_id <- trajectory_input_id("unit", index, version)
        selected_unit <- shiny::isolate(input[[input_id]])
        if (is.null(selected_unit) || !selected_unit %in% c("None", units)) {
          selected_unit <- "None"
        }
        shiny::selectInput(
          session$ns(input_id),
          paste("Unit", index),
          choices = c("None", units),
          selected = selected_unit
        )
      }))
    })

    output$trajectory_los_ui <- shiny::renderUI({
      version <- trajectory_form_version()
      count <- trajectory_unit_count()
      shiny::tagList(lapply(seq_len(count), function(index) {
        input_id <- trajectory_input_id("los", index, version)
        los_value <- shiny::isolate(input[[input_id]])
        if (is.null(los_value)) los_value <- NA_real_
        shiny::numericInput(
          session$ns(input_id),
          paste("LOS unit", index, "(days)"),
          value = los_value,
          min = 0.01
        )
      }))
    })

    shiny::observeEvent(input$add_trajectory_unit, {
      trajectory_unit_count(trajectory_unit_count() + 1L)
    })

    commit_patient_profile <- function(profile_name, profile) {
      profiles <- patient_profiles()
      profiles[[profile_name]] <- profile
      patient_profiles(profiles)
      trajectory_unit_count(1L)
      trajectory_form_version(trajectory_form_version() + 1L)
    }

    shiny::observeEvent(input$add_profile, {
      profile_name <- trimws(input$profile_name)
      shiny::req(nzchar(profile_name))
      shiny::validate(shiny::need(
        grepl("^[A-Za-z][A-Za-z0-9_-]*$", profile_name),
        "Profile names must start with a letter and use only letters, numbers, underscores, or hyphens."
      ))
      version <- trajectory_form_version()
      count <- trajectory_unit_count()
      units <- vapply(seq_len(count), function(index) {
        value <- input[[trajectory_input_id("unit", index, version)]]
        if (is.null(value)) "None" else value
      }, character(1))
      los <- vapply(seq_len(count), function(index) {
        value <- input[[trajectory_input_id("los", index, version)]]
        if (is.null(value)) NA_real_ else value
      }, numeric(1))
      keep <- !is.na(units) & units != "None" & is.finite(los) & los > 0

      shiny::validate(
        shiny::need(any(keep), "A profile must contain at least one unit with a positive LOS."),
        shiny::need(
          all(units[keep] %in% selected_units()),
          "All trajectory units must be selected hospital units."
        )
      )

      new_profile <- list(unit = units[keep], los = los[keep])
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
      shiny::updateSelectInput(
        session, "remove_profile_name", choices = names(patient_profiles())
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

    output$profile_percent_ui <- shiny::renderUI({
      profiles <- patient_profiles()
      if (length(profiles) == 0) {
        return(shiny::helpText("Create at least one patient profile."))
      }
      default_percent <- 100 / length(profiles)
      shiny::tagList(lapply(names(profiles), function(profile_name) {
        shiny::numericInput(
          session$ns(paste0("prob_", profile_name)),
          paste(profile_name, "arrival percentage (%)"),
          value = round(default_percent, 4),
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
        value <- input[[paste0("prob_", profile_name)]]
        if (is.null(value)) NA_real_ else value
      }, numeric(1)), names(profiles))
    })

    shiny::observeEvent(patient_profiles(), {
      confirmed_profile_probabilities(NULL)
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
        length(fallback_units) > 0,
        "Select at least one fallback unit."
      ))
      current <- fallbacks()
      current[[primary]] <- fallback_units
      fallbacks(current)
      shiny::updateSelectInput(session, "fallback_unit", selected = "")
      shiny::updateSelectizeInput(
        session,
        "fallback_options",
        choices = selected_units(),
        selected = character(),
        server = TRUE
      )
    })

    output$fallbacks_summary <- shiny::renderPrint({
      print(fallbacks())
    })

    capacities <- shiny::reactive({
      units <- selected_units()
      values <- vapply(units, function(unit_name) {
        value <- input[[paste0("capacity_", unit_name)]]
        if (is.null(value)) NA_real_ else value
      }, numeric(1))
      stats::setNames(values, units)
    })

    effective_profile_data <- shiny::reactive({
      test_config <- selected_test_config()
      if (!is.null(test_config)) return(test_config)
      if (identical(input$profile_source, "excel_upload")) {
        return(list(
          source = "excel_upload",
          source_label = "Uploaded Excel: select a valid .xlsx file",
          patient_profiles = list(),
          profile_prob = numeric(),
          fallbacks = list()
        ))
      }
      list(
        source = "manual",
        source_label = "Manually entered profiles",
        patient_profiles = patient_profiles(),
        profile_prob = profile_probabilities(),
        fallbacks = fallbacks()
      )
    })

    output$active_fallbacks_summary <- shiny::renderPrint({
      active_fallbacks <- effective_profile_data()$fallbacks
      if (length(active_fallbacks) == 0) {
        cat("No fallback units are configured for this example.\n")
      } else {
        print(active_fallbacks)
      }
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
      if (length(profiles) == 0) errors <- c(errors, "Create at least one patient profile or use the Deloitte test profiles.")
      if (length(capacity_values) == 0 || any(!is.finite(capacity_values)) || any(capacity_values < 0)) {
        errors <- c(errors, "Enter a valid non-negative capacity for every selected unit.")
      }
      if (length(probabilities) == 0 || any(!is.finite(probabilities)) ||
          abs(sum(probabilities) - 1) > 1e-4) {
        errors <- c(
          errors,
          if (identical(profile_data$source, "manual")) {
            "Enter percentages totaling 100% and click Set arrival percentages."
          } else {
            "Profile arrival percentages must sum to 100%."
          }
        )
      }
      profile_units <- unique(unlist(lapply(profiles, `[[`, "unit"), use.names = FALSE))
      if (length(setdiff(profile_units, units)) > 0) {
        errors <- c(errors, "Every trajectory unit must remain selected as a hospital unit.")
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
          paste(length(profile_data$patient_profiles), "profiles loaded. Configuration is ready to run.")
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
            if (is.null(profile$unit)) "Ambulatory" else paste(profile$unit, collapse = " -> ")
          },
          character(1)
        ),
        LOS_days = vapply(
          profiles,
          function(profile) {
            if (is.null(profile$los)) "-" else paste(profile$los, collapse = " -> ")
          },
          character(1)
        ),
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