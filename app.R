.libPaths(c("./r_libs", .libPaths()))

library(shiny)
library(RODBC)

species_name <- "Pandalus borealis"
criterion_type_defaults <- c("ShrimpRoe", "EggHair", "ShrimpSex")
criterion_attribute_defaults <- c("None", "0", "FP")

escape_access_string <- function(value) {
  value <- as.character(value)
  value <- gsub("'", "''", value, fixed = TRUE)
  paste0("'", value, "'")
}

is_blank <- function(value) {
  is.null(value) || !nzchar(trimws(as.character(value)))
}

default_value <- function(values, idx) {
  if (idx <= length(values)) {
    values[[idx]]
  } else {
    ""
  }
}

sql_name <- function(value) {
  paste0("[", value, "]")
}

sql_field <- function(table_name, field_name) {
  paste0(sql_name(table_name), ".", sql_name(field_name))
}

build_station_conditions <- function(config) {
  c(
    paste0("((", sql_field("tblStation", "TripYear"), ")=", as.integer(config$trip_year), ")"),
    paste0("((", sql_field("tblStation", "Ship"), ")=", escape_access_string(config$ship), ")"),
    paste0("((", sql_field("tblStation", "Trip"), ")=", escape_access_string(config$trip), ")"),
    paste0("((", sql_field("tblStation", "Station"), ")=", as.integer(config$station), ")"),
    paste0("((", sql_field("tblLstSpecies", "NameLatin"), ")=", escape_access_string(species_name), ")")
  )
}

build_criteria_conditions <- function(criteria) {
  vapply(
    criteria,
    function(item) {
      paste0(
        "((", sql_field("tblIndividual", "KeyIndividual"), ") In ",
        "(SELECT ", sql_name("KeyIndividual"), " FROM ", sql_name("tblIndividualMeasure"), " ",
        "WHERE ", sql_name("IndividualMeasureType"), "=", escape_access_string(item$type),
        " AND ", sql_name("Attribute"), "=", escape_access_string(item$attribute),
        "))"
      )
    },
    character(1)
  )
}

build_target_conditions <- function(config) {
  c(
    paste0(
      "((", sql_field("tblIndividualMeasure", "IndividualMeasureType"), ")=",
      escape_access_string(config$target_type),
      ")"
    ),
    paste0(
      "((", sql_field("tblIndividualMeasure", "Attribute"), ")=",
      escape_access_string(config$current_attribute),
      ")"
    )
  )
}

build_where_clause <- function(config) {
  conditions <- c(
    build_station_conditions(config),
    build_criteria_conditions(config$criteria),
    build_target_conditions(config)
  )

  paste0("WHERE ", paste(conditions, collapse = " AND "))
}

select_join_sql <- paste(
  "FROM [tblStation]",
  "INNER JOIN ([tblStationSubGear] INNER JOIN ([tblLstSpecies] INNER JOIN",
  "(((([tblCatch] INNER JOIN [tblCatchSub1] ON [tblCatch].[KeyCatch] = [tblCatchSub1].[KeyCatch])",
  "INNER JOIN [tblCatchSub2] ON [tblCatchSub1].[KeyCatchSub1] = [tblCatchSub2].[KeyCatchSub1])",
  "INNER JOIN [tblIndividual] ON [tblCatchSub2].[KeyCatchSub2] = [tblIndividual].[KeyCatchSub2])",
  "INNER JOIN [tblIndividualMeasure] ON [tblIndividual].[KeyIndividual] = [tblIndividualMeasure].[KeyIndividual])",
  "ON [tblLstSpecies].[Species] = [tblCatchSub2].[Species])",
  "ON [tblStationSubGear].[KeyStationSubGear] = [tblCatch].[KeyStationSubGear])",
  "ON [tblStation].[KeyStation] = [tblStationSubGear].[KeyStation]"
)

update_join_sql <- paste(
  "UPDATE [tblLstSpecies] INNER JOIN (((((([tblStation]",
  "INNER JOIN [tblStationSubGear] ON [tblStation].[KeyStation] = [tblStationSubGear].[KeyStation])",
  "INNER JOIN [tblCatch] ON [tblStationSubGear].[KeyStationSubGear] = [tblCatch].[KeyStationSubGear])",
  "INNER JOIN [tblCatchSub1] ON [tblCatch].[KeyCatch] = [tblCatchSub1].[KeyCatch])",
  "INNER JOIN [tblCatchSub2] ON [tblCatchSub1].[KeyCatchSub1] = [tblCatchSub2].[KeyCatchSub1])",
  "INNER JOIN [tblIndividual] ON [tblCatchSub2].[KeyCatchSub2] = [tblIndividual].[KeyCatchSub2])",
  "INNER JOIN [tblIndividualMeasure] ON [tblIndividual].[KeyIndividual] = [tblIndividualMeasure].[KeyIndividual])",
  "ON [tblLstSpecies].[Species] = [tblCatchSub2].[Species]"
)

build_count_sql <- function(config) {
  paste(
    "SELECT Count(*) AS [AffectedRows]",
    select_join_sql,
    build_where_clause(config)
  )
}

build_preview_sql <- function(config) {
  paste(
    "SELECT [tblStation].[TripYear], [tblStation].[Ship], [tblStation].[Trip],",
    "[tblStation].[Station], [tblIndividual].[KeyIndividual], [tblLstSpecies].[NameLatin],",
    "[tblIndividualMeasure].[IndividualMeasureType], [tblIndividualMeasure].[Attribute],",
    "[tblIndividualMeasure].[Measure]",
    select_join_sql,
    build_where_clause(config),
    "ORDER BY [tblIndividual].[KeyIndividual]"
  )
}

build_update_sql <- function(config) {
  paste(
    update_join_sql,
    "SET [tblIndividualMeasure].[Attribute] =",
    escape_access_string(config$new_attribute),
    build_where_clause(config)
  )
}

run_sql <- function(channel, sql) {
  result <- tryCatch(
    sqlQuery(channel, sql, stringsAsFactors = FALSE, believeNRows = FALSE),
    error = function(err) err
  )

  if (inherits(result, "error")) {
    return(list(ok = FALSE, message = conditionMessage(result), data = NULL))
  }

  if (is.character(result)) {
    return(list(ok = FALSE, message = paste(result, collapse = "\n"), data = NULL))
  }

  list(ok = TRUE, message = NULL, data = result)
}

is_open_rodbc_channel <- function(channel) {
  inherits(channel, "RODBC") && isTRUE(tryCatch(odbcValidChannel(channel), error = function(err) FALSE))
}

collect_config <- function(input) {
  criteria_count <- input$criteria_count

  if (is.null(criteria_count) || is.na(criteria_count)) {
    criteria_count <- 0
  }

  criteria <- vector("list", criteria_count)

  for (idx in seq_len(criteria_count)) {
    criteria[[idx]] <- list(
      type = input[[paste0("criterion_type_", idx)]],
      attribute = input[[paste0("criterion_attribute_", idx)]]
    )
  }

  list(
    db_path = input$db_path,
    trip_year = input$trip_year,
    ship = input$ship,
    trip = input$trip,
    station = input$station,
    criteria = criteria,
    target_type = input$target_type,
    current_attribute = input$current_attribute,
    new_attribute = input$new_attribute
  )
}

validate_config <- function(config) {
  if (is_blank(config$db_path)) {
    return("Database path is required.")
  }

  if (is.null(config$trip_year) || is.na(config$trip_year)) {
    return("TripYear is required.")
  }

  if (is_blank(config$ship)) {
    return("Ship is required.")
  }

  if (is_blank(config$trip)) {
    return("Trip is required.")
  }

  if (is.null(config$station) || is.na(config$station)) {
    return("Station is required.")
  }

  if (length(config$criteria) < 1) {
    return("At least one criterion is required.")
  }

  for (idx in seq_along(config$criteria)) {
    if (is_blank(config$criteria[[idx]]$type) || is_blank(config$criteria[[idx]]$attribute)) {
      return(paste0("Criterion ", idx, " must include both measure type and attribute."))
    }
  }

  if (is_blank(config$target_type)) {
    return("Target IndividualMeasureType is required.")
  }

  if (is_blank(config$current_attribute)) {
    return("Current target Attribute is required.")
  }

  if (is_blank(config$new_attribute)) {
    return("New Attribute is required.")
  }

  if (identical(trimws(config$current_attribute), trimws(config$new_attribute))) {
    return("New Attribute must differ from the current target Attribute.")
  }

  NULL
}

config_signature <- function(config) {
  parts <- c(
    config$db_path,
    as.character(config$trip_year),
    config$ship,
    config$trip,
    as.character(config$station),
    unlist(lapply(config$criteria, function(item) c(item$type, item$attribute)), use.names = FALSE),
    config$target_type,
    config$current_attribute,
    config$new_attribute
  )

  paste(parts, collapse = "\r")
}

ui <- fluidPage(
  titlePanel("tblIndividualMeasure Bulk Update"),
  fluidRow(
    column(
      width = 4,
      wellPanel(
        h4("Database"),
        textInput(
          "db_path",
          "Access database path",
          value = "F:\\data\\2026-TA-togt1to3\\togt2\\Mallotus.v.7.d.80_ship_2026_TA_2.accdb"
        ),
        actionButton("connect_db", "Connect"),
        tags$p(strong("Fixed species:"), species_name),
        textOutput("connection_status")
      ),
      wellPanel(
        h4("Station Scope"),
        numericInput("trip_year", "TripYear", value = 2026, min = 0, step = 1),
        textInput("ship", "Ship", value = "TA"),
        textInput("trip", "Trip", value = "2"),
        numericInput("station", "Station", value = 50, min = 0, step = 1)
      ),
      wellPanel(
        h4("Individual Criteria"),
        numericInput("criteria_count", "Number of criteria", value = 3, min = 1, max = 10, step = 1),
        uiOutput("criteria_inputs")
      ),
      wellPanel(
        h4("Update Target"),
        textInput("target_type", "Target IndividualMeasureType", value = "ShrimpSex"),
        textInput("current_attribute", "Current target Attribute", value = "FP"),
        textInput("new_attribute", "New Attribute", value = "M")
      ),
      actionButton("preview", "Preview affected rows", class = "btn-primary"),
      actionButton("confirm_update", "Confirm update", class = "btn-danger")
    ),
    column(
      width = 8,
      wellPanel(
        h4("Preview"),
        textOutput("preview_status"),
        tableOutput("preview_count"),
        tags$p("Showing all matching rows."),
        tableOutput("preview_table")
      ),
      wellPanel(
        h4("Generated SQL"),
        verbatimTextOutput("sql_preview")
      )
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    channel = NULL,
    connection_status = "Not connected.",
    preview_status = "No preview has been run.",
    preview_count = NULL,
    preview_data = NULL,
    preview_sql = NULL,
    update_sql = NULL,
    preview_ready = FALSE,
    preview_signature = NULL,
    last_preview_count = NULL
  )

  close_channel <- function() {
    if (!is.null(rv$channel)) {
      try(sqlClose(rv$channel), silent = TRUE)
      rv$channel <- NULL
    }
  }

  session$onSessionEnded(close_channel)

  observeEvent(input$connect_db, {
    close_channel()
    rv$preview_ready <- FALSE
    rv$preview_signature <- NULL
    rv$preview_count <- NULL
    rv$preview_data <- NULL
    rv$preview_sql <- NULL
    rv$update_sql <- NULL
    rv$last_preview_count <- NULL
    rv$preview_status <- "No preview has been run."

    if (is_blank(input$db_path)) {
      rv$connection_status <- "Connection failed: database path is required."
      return()
    }

    connection_warnings <- character(0)
    channel <- tryCatch(
      withCallingHandlers(
        odbcConnectAccess2007(input$db_path),
        warning = function(warn) {
          connection_warnings <<- c(connection_warnings, conditionMessage(warn))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(err) err
    )

    if (inherits(channel, "error")) {
      rv$connection_status <- paste("Connection failed:", conditionMessage(channel))
      return()
    }

    if (!is_open_rodbc_channel(channel)) {
      rv$channel <- NULL
      status_message <- "Connection failed: RODBC did not return an open channel."

      if (length(connection_warnings) > 0) {
        status_message <- paste(status_message, paste(connection_warnings, collapse = " | "))
      }

      rv$connection_status <- status_message
      return()
    }

    rv$channel <- channel

    validation_result <- run_sql(rv$channel, "SELECT Count(*) AS [StationCount] FROM [tblStation]")
    if (!validation_result$ok) {
      close_channel()
      rv$connection_status <- paste("Connection failed validation:", validation_result$message)
      return()
    }

    rv$connection_status <- paste(
      c(
        paste("Connected to", input$db_path),
        if (length(connection_warnings) > 0) paste("Warnings:", paste(connection_warnings, collapse = " | "))
      ),
      collapse = " "
    )
  })

  observe({
    config <- collect_config(input)

    if (length(config$criteria) < 1) {
      return()
    }

    signature <- config_signature(config)

    if (!is.null(rv$preview_signature) && !identical(signature, rv$preview_signature)) {
      rv$preview_ready <- FALSE
      rv$preview_status <- "Inputs changed after the last preview. Run preview again before updating."
    }
  })

  output$criteria_inputs <- renderUI({
    criteria_count <- input$criteria_count

    if (is.null(criteria_count) || is.na(criteria_count) || criteria_count < 1) {
      return(NULL)
    }

    tagList(lapply(seq_len(criteria_count), function(idx) {
      type_id <- paste0("criterion_type_", idx)
      attribute_id <- paste0("criterion_attribute_", idx)
      current_type <- input[[type_id]]
      current_attribute <- input[[attribute_id]]

      fluidRow(
        column(
          width = 6,
          textInput(
            type_id,
            paste("Criterion", idx, "IndividualMeasureType"),
            value = if (is.null(current_type)) default_value(criterion_type_defaults, idx) else current_type
          )
        ),
        column(
          width = 6,
          textInput(
            attribute_id,
            paste("Criterion", idx, "Attribute"),
            value = if (is.null(current_attribute)) default_value(criterion_attribute_defaults, idx) else current_attribute
          )
        )
      )
    }))
  })

  output$connection_status <- renderText(rv$connection_status)
  output$preview_status <- renderText(rv$preview_status)

  output$preview_count <- renderTable({
    rv$preview_count
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$preview_table <- renderTable({
    rv$preview_data
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$sql_preview <- renderText({
    if (is.null(rv$preview_sql)) {
      return("No SQL generated yet.")
    }

    paste(
      "-- Preview count",
      rv$preview_sql$count,
      "",
      "-- Preview sample",
      rv$preview_sql$preview,
      "",
      "-- Update",
      rv$update_sql,
      sep = "\n"
    )
  })

  observeEvent(input$preview, {
    if (is.null(rv$channel)) {
      rv$preview_status <- "Connect to the database before previewing."
      return()
    }

    config <- collect_config(input)
    validation_error <- validate_config(config)

    if (!is.null(validation_error)) {
      rv$preview_status <- validation_error
      return()
    }

    count_sql <- build_count_sql(config)
    preview_sql <- build_preview_sql(config)
    update_sql <- build_update_sql(config)
    rv$preview_sql <- list(count = count_sql, preview = preview_sql)
    rv$update_sql <- update_sql

    count_result <- run_sql(rv$channel, count_sql)
    if (!count_result$ok) {
      rv$preview_status <- paste("Count preview failed:", count_result$message)
      rv$preview_ready <- FALSE
      return()
    }

    preview_result <- run_sql(rv$channel, preview_sql)
    if (!preview_result$ok) {
      rv$preview_status <- paste("Row preview failed:", preview_result$message)
      rv$preview_ready <- FALSE
      return()
    }

    affected_rows <- if (nrow(count_result$data) > 0) count_result$data$AffectedRows[[1]] else 0

    rv$preview_count <- data.frame(AffectedRows = affected_rows, check.names = FALSE)
    rv$preview_data <- preview_result$data
    rv$preview_signature <- config_signature(config)
    rv$preview_ready <- TRUE
    rv$last_preview_count <- affected_rows
    rv$preview_status <- paste("Preview ready.", affected_rows, "row(s) match the update criteria.")
  })

  observeEvent(input$confirm_update, {
    if (!isTRUE(rv$preview_ready)) {
      rv$preview_status <- "Run a successful preview before updating."
      return()
    }

    showModal(modalDialog(
      title = "Confirm bulk update",
      paste(
        "The last preview found", rv$last_preview_count,
        "row(s) to update from", shQuote(input$current_attribute),
        "to", shQuote(input$new_attribute), "."
      ),
      "Make sure the preview table and SQL are correct before proceeding.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("execute_update", "Execute update", class = "btn-danger")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$execute_update, {
    removeModal()

    if (is.null(rv$channel)) {
      rv$preview_status <- "Connection is no longer available. Reconnect and preview again."
      rv$preview_ready <- FALSE
      return()
    }

    config <- collect_config(input)
    if (!identical(config_signature(config), rv$preview_signature)) {
      rv$preview_status <- "Inputs changed after the preview. Run preview again before updating."
      rv$preview_ready <- FALSE
      return()
    }

    update_result <- run_sql(rv$channel, rv$update_sql)
    if (!update_result$ok) {
      rv$preview_status <- paste("Update failed:", update_result$message)
      return()
    }

    rv$preview_ready <- FALSE
    rv$preview_status <- paste(
      "Update executed. The preview had identified",
      rv$last_preview_count,
      "row(s). Run preview again to inspect the new state."
    )
  })
}

shinyApp(ui = ui, server = server)
