.libPaths(c("./r_libs", .libPaths()))

library(shiny)
library(RODBC)

species_name <- "Pandalus borealis"
criterion_type_defaults <- c("ShrimpRoe", "EggHair", "ShrimpSex")
criterion_attribute_defaults <- c("None", "0", "FP")
db_path_storage_file <- "last_db_path.txt"
default_db_path <- "F:\\data\\2026-TA-togt1to3\\togt2\\Mallotus.v.7.d.80_ship_2026_TA_2.accdb"

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

load_saved_db_path <- function() {
  if (!file.exists(db_path_storage_file)) {
    return(default_db_path)
  }

  saved_path <- tryCatch(readLines(db_path_storage_file, warn = FALSE, n = 1), error = function(err) "")

  if (length(saved_path) < 1 || is_blank(saved_path[[1]])) {
    default_db_path
  } else {
    saved_path[[1]]
  }
}

save_db_path <- function(path) {
  tryCatch(writeLines(as.character(path), db_path_storage_file, useBytes = TRUE), error = function(err) invisible(NULL))
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

build_missing_target_conditions <- function(config) {
  paste0(
    "((", sql_field("tblIndividual", "KeyIndividual"), ") Not In ",
    "(SELECT ", sql_name("KeyIndividual"), " FROM ", sql_name("tblIndividualMeasure"), " ",
    "WHERE ", sql_name("IndividualMeasureType"), "=", escape_access_string(config$target_type),
    "))"
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

build_missing_where_clause <- function(config) {
  conditions <- c(
    build_station_conditions(config),
    build_criteria_conditions(config$criteria),
    build_missing_target_conditions(config)
  )

  paste0("WHERE ", paste(conditions, collapse = " AND "))
}

individual_join_sql <- paste(
  "FROM [tblStation]",
  "INNER JOIN ([tblStationSubGear] INNER JOIN ([tblLstSpecies] INNER JOIN",
  "((([tblCatch] INNER JOIN [tblCatchSub1] ON [tblCatch].[KeyCatch] = [tblCatchSub1].[KeyCatch])",
  "INNER JOIN [tblCatchSub2] ON [tblCatchSub1].[KeyCatchSub1] = [tblCatchSub2].[KeyCatchSub1])",
  "INNER JOIN [tblIndividual] ON [tblCatchSub2].[KeyCatchSub2] = [tblIndividual].[KeyCatchSub2])",
  "ON [tblLstSpecies].[Species] = [tblCatchSub2].[Species])",
  "ON [tblStationSubGear].[KeyStationSubGear] = [tblCatch].[KeyStationSubGear])",
  "ON [tblStation].[KeyStation] = [tblStationSubGear].[KeyStation]"
)

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
  if (isTRUE(config$absence_only)) {
    return(paste(
      "SELECT Count(*) AS [AffectedRows] FROM",
      "(SELECT DISTINCT [tblIndividual].[KeyIndividual]",
      individual_join_sql,
      build_missing_where_clause(config),
      ") AS [MatchingIndividuals]"
    ))
  }

  paste(
    "SELECT Count(*) AS [AffectedRows]",
    select_join_sql,
    build_where_clause(config)
  )
}

build_preview_sql <- function(config) {
  if (isTRUE(config$absence_only)) {
    return(NULL)
  }

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
  if (isTRUE(config$absence_only)) {
    return(paste(
      "INSERT INTO [tblIndividualMeasure] ([KeyIndividual], [IndividualMeasureType], [Attribute])",
      "SELECT DISTINCT [tblIndividual].[KeyIndividual],",
      escape_access_string(config$target_type), ",",
      escape_access_string(config$new_attribute),
      individual_join_sql,
      build_missing_where_clause(config)
    ))
  }

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
    absence_only = isTRUE(input$absence_only),
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

  if (!isTRUE(config$absence_only) && is_blank(config$current_attribute)) {
    return("Current target Attribute is required.")
  }

  if (is_blank(config$new_attribute)) {
    return("New Attribute is required.")
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
    as.character(config$absence_only),
    config$target_type,
    if (isTRUE(config$absence_only)) "" else config$current_attribute,
    config$new_attribute
  )

  paste(parts, collapse = "\r")
}

ui <- fluidPage(
  titlePanel("tblIndividualMeasure Bulk Update"),
  tags$head(tags$style(HTML("\n    .preview-status-box {\n      padding: 12px 14px;\n      margin-bottom: 12px;\n      border: 1px solid #b7c4d1;\n      border-left-width: 5px;\n      border-radius: 4px;\n      background-color: #eef3f8;\n      color: #1f2d3d;\n      font-size: 15px;\n      font-weight: 600;\n      line-height: 1.4;\n    }\n    .preview-status-box.status-success {\n      background-color: #edf7ed;\n      border-color: #3c763d;\n      color: #2f5f2f;\n    }\n    .preview-status-box.status-warning {\n      background-color: #fff8e5;\n      border-color: #b37a00;\n      color: #8a5a00;\n    }\n    .preview-status-box.status-error {\n      background-color: #fdecec;\n      border-color: #a94442;\n      color: #8f2f2d;\n    }\n    .preview-status-box.status-info {\n      background-color: #eef3f8;\n      border-color: #4f6f8f;\n      color: #1f2d3d;\n    }\n  "))),
  fluidRow(
    column(
      width = 4,
      wellPanel(
        h4("Database"),
        textInput(
          "db_path",
          "Access database path",
          value = load_saved_db_path()
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
        checkboxInput("absence_only", "Insert when target measure type is missing", value = FALSE),
        textInput("target_type", "Target IndividualMeasureType", value = "ShrimpSex"),
        conditionalPanel(
          condition = "!input.absence_only",
          textInput("current_attribute", "Current target Attribute", value = "FP")
        ),
        textInput("new_attribute", "New Attribute", value = "M")
      )
    ),
    column(
      width = 8,
      div(
        style = "margin-bottom: 15px;",
        actionButton("preview", "Preview affected rows", class = "btn-primary"),
        actionButton("confirm_update", "Confirm update", class = "btn-danger")
      ),
      wellPanel(
        h4("Preview"),
        uiOutput("preview_status"),
        tableOutput("preview_count"),
        tags$p("Showing all matching rows unless the target is configured for missing-measure insertion."),
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
  channel <- NULL

  rv <- reactiveValues(
    connection_status = "Not connected.",
    preview_status = "No preview has been run.",
    preview_status_type = "info",
    preview_count = NULL,
    preview_data = NULL,
    preview_sql = NULL,
    update_sql = NULL,
    preview_ready = FALSE,
    preview_signature = NULL,
    last_preview_count = NULL
  )

  close_channel <- function() {
    if (!is.null(channel)) {
      try(sqlClose(channel), silent = TRUE)
      channel <<- NULL
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
    rv$preview_status_type <- "info"

    if (is_blank(input$db_path)) {
      rv$connection_status <- "Connection failed: database path is required."
      return()
    }

    connection_warnings <- character(0)
    db_channel <- tryCatch(
      withCallingHandlers(
        odbcConnectAccess2007(input$db_path),
        warning = function(warn) {
          connection_warnings <<- c(connection_warnings, conditionMessage(warn))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(err) err
    )

    if (inherits(db_channel, "error")) {
      rv$connection_status <- paste("Connection failed:", conditionMessage(db_channel))
      return()
    }

    channel <<- db_channel

    validation_result <- run_sql(channel, "SELECT Count(*) AS [StationCount] FROM [tblStation]")
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
    save_db_path(input$db_path)
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
      rv$preview_status_type <- "warning"
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
      current_type <- isolate(input[[type_id]])
      current_attribute <- isolate(input[[attribute_id]])

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
  output$preview_status <- renderUI({
    div(
      class = paste("preview-status-box", paste0("status-", rv$preview_status_type)),
      rv$preview_status
    )
  })

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

    sql_sections <- c(
      "-- Preview count",
      rv$preview_sql$count
    )

    if (!is.null(rv$preview_sql$preview)) {
      sql_sections <- c(
        sql_sections,
        "",
        "-- Preview sample",
        rv$preview_sql$preview
      )
    }

    sql_sections <- c(
      sql_sections,
      "",
      if (isTRUE(input$absence_only)) "-- Insert missing target rows" else "-- Update",
      rv$update_sql
    )

    paste(sql_sections, collapse = "\n")
  })

  observeEvent(input$preview, {
    if (is.null(channel)) {
      rv$preview_status <- "Connect to the database before previewing."
      rv$preview_status_type <- "warning"
      return()
    }

    config <- collect_config(input)
    validation_error <- validate_config(config)

    if (!is.null(validation_error)) {
      rv$preview_status <- validation_error
      rv$preview_status_type <- "error"
      return()
    }

    count_sql <- build_count_sql(config)
    preview_sql <- build_preview_sql(config)
    update_sql <- build_update_sql(config)
    rv$preview_sql <- list(count = count_sql, preview = preview_sql)
    rv$update_sql <- update_sql

    count_result <- run_sql(channel, count_sql)
    if (!count_result$ok) {
      rv$preview_status <- paste("Count preview failed:", count_result$message)
      rv$preview_ready <- FALSE
      rv$preview_status_type <- "error"
      return()
    }

    affected_rows <- if (nrow(count_result$data) > 0) count_result$data$AffectedRows[[1]] else 0

    rv$preview_count <- data.frame(AffectedRows = affected_rows, check.names = FALSE)

    if (isTRUE(config$absence_only)) {
      rv$preview_data <- NULL
    } else {
      preview_result <- run_sql(channel, preview_sql)
      if (!preview_result$ok) {
        rv$preview_status <- paste("Row preview failed:", preview_result$message)
        rv$preview_ready <- FALSE
        rv$preview_status_type <- "error"
        return()
      }

      rv$preview_data <- preview_result$data
    }

    rv$preview_signature <- config_signature(config)
    rv$preview_ready <- TRUE
    rv$last_preview_count <- affected_rows
    rv$preview_status <- if (isTRUE(config$absence_only)) {
      paste("Preview ready.", affected_rows, "individual(s) are missing the target measure type and will receive a new row.")
    } else {
      paste("Preview ready.", affected_rows, "row(s) match the update criteria.")
    }
    rv$preview_status_type <- "success"
  })

  observeEvent(input$confirm_update, {
    if (!isTRUE(rv$preview_ready)) {
      rv$preview_status <- "Run a successful preview before updating."
      rv$preview_status_type <- "warning"
      return()
    }

    showModal(modalDialog(
      title = "Confirm bulk update",
      if (isTRUE(input$absence_only)) {
        paste(
          "The last preview found", rv$last_preview_count,
          "individual(s) missing", shQuote(input$target_type),
          "and will insert a new row with Attribute", shQuote(input$new_attribute), "."
        )
      } else {
        paste(
          "The last preview found", rv$last_preview_count,
          "row(s) to update from", shQuote(input$current_attribute),
          "to", shQuote(input$new_attribute), "."
        )
      },
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

    if (is.null(channel)) {
      rv$preview_status <- "Connection is no longer available. Reconnect and preview again."
      rv$preview_ready <- FALSE
      rv$preview_status_type <- "error"
      return()
    }

    config <- collect_config(input)
    if (!identical(config_signature(config), rv$preview_signature)) {
      rv$preview_status <- "Inputs changed after the preview. Run preview again before updating."
      rv$preview_ready <- FALSE
      rv$preview_status_type <- "warning"
      return()
    }

    update_result <- run_sql(channel, rv$update_sql)

    rv$preview_ready <- FALSE
    rv$preview_status <- if (isTRUE(config$absence_only)) {
      paste(
        "Insert executed. The preview had identified",
        rv$last_preview_count,
        "individual(s) missing the target measure type. Run preview again to inspect the new state."
      )
    } else {
      paste(
        "Update executed. The preview had identified",
        rv$last_preview_count,
        "row(s). Run preview again to inspect the new state."
      )
    }
    rv$preview_status_type <- "success"
  })
}

shinyApp(ui = ui, server = server)
