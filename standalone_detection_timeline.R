#!/usr/bin/env Rscript

# Standalone prototype for a session-based interactive detection timeline.
# Run from the ecostats-rshiny folder with:
#   Rscript standalone_detection_timeline.R

suppressPackageStartupMessages({
  library(base64enc)
  library(DBI)
  library(RSQLite)
  library(dplyr)
  library(magrittr)
  library(shiny)
})

if (!requireNamespace("plotly", quietly = TRUE)) {
  stop(
    "Package 'plotly' is required for this prototype. Install it with: ",
    "install.packages('plotly')"
  )
}

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "."
script_dir <- normalizePath(dirname(script_path), mustWork = TRUE)
repo_root <- normalizePath(file.path(script_dir, ".."), mustWork = TRUE)

source(file.path(script_dir, "R", "fct_database_inputs.R"))
source(file.path(script_dir, "R", "fct_helpers.R"))
source(file.path(script_dir, "R", "fct_detection_timeline.R"))
source(file.path(script_dir, "R", "mod_match_calls_fct_backend.R"))

db_paths <- sort(Sys.glob(file.path(repo_root, "data", "db", "NCNX06*_database.sqlite3")))
wav_root <- "/Users/id52/Documents/Gibbon2026/20260710/Set1_wav"
spectro_cache_dir <- file.path(tempdir(), "standalone_detection_timeline_spectros")
session_gap_minutes <- 30

if (length(db_paths) == 0) {
  stop("No NCNX06 SQLite databases found under data/db.")
}
if (!dir.exists(wav_root)) {
  stop("WAV root folder not found: ", wav_root)
}
dir.create(spectro_cache_dir, recursive = TRUE, showWarnings = FALSE)

db_data <- read_detection_databases(db_paths)
rec_data <- parse_rec_data(db_data$recData)
recording_intervals <- read_recording_intervals_from_databases(db_paths)
sessions <- build_recording_sessions(
  recording_intervals,
  gap_seconds = session_gap_minutes * 60
)
timeline_data <- assign_detections_to_sessions(rec_data, sessions)
recorder_levels <- attr(timeline_data, "recorder_levels")

session_counts <- timeline_data %>%
  filter(!is.na(session_id)) %>%
  count(session_id, name = "n_detections")
sessions <- sessions %>%
  left_join(session_counts, by = "session_id") %>%
  mutate(n_detections = ifelse(is.na(n_detections), 0L, n_detections))

initial_session_index <- which(sessions$n_detections > 0)
initial_session_index <- if (length(initial_session_index) > 0) initial_session_index[[1]] else 1L

session_title <- function(session_row) {
  paste0(
    format(session_row$real_start[[1]], "%Y-%m-%d", tz = "UTC"),
    " | ",
    format(session_row$real_start[[1]], "%H:%M:%S", tz = "UTC"),
    " - ",
    format(session_row$real_stop[[1]], "%H:%M:%S", tz = "UTC"),
    " UTC"
  )
}

ui <- fluidPage(
  titlePanel("Detection Timeline Prototype"),
  fluidRow(
    column(
      12,
      div(
        style = "display: flex; align-items: center; gap: 8px; margin-bottom: 10px;",
        actionButton("prev_session", "Previous session"),
        actionButton("next_session", "Next session"),
        tags$span(textOutput("session_summary", inline = TRUE))
      )
    )
  ),
  fluidRow(
    column(
      6,
      plotly::plotlyOutput("timeline_plot", height = "420px")
    ),
    column(
      6,
      div(
        style = "height: 420px; overflow-y: auto; border-left: 1px solid #ddd; padding-left: 12px;",
        uiOutput("selected_spectrograms")
      )
    )
  ),
  fluidRow(
    column(
      12,
      tags$strong("Clicked detection"),
      verbatimTextOutput("clicked_detection")
    )
  )
)

server <- function(input, output, session) {
  current_session_index <- reactiveVal(initial_session_index)

  observeEvent(input$prev_session, {
    current_session_index(max(1L, current_session_index() - 1L))
  })

  observeEvent(input$next_session, {
    current_session_index(min(nrow(sessions), current_session_index() + 1L))
  })

  current_session <- reactive({
    sessions[current_session_index(), , drop = FALSE]
  })

  session_data <- reactive({
    timeline_data %>%
      filter(session_id == current_session()$session_id[[1]]) %>%
      arrange(toa, mic_id)
  })

  output$session_summary <- renderText({
    row <- current_session()
    paste0(
      "Session ",
      current_session_index(),
      " of ",
      nrow(sessions),
      ": ",
      session_title(row),
      " | ",
      row$n_detections[[1]],
      " detections"
    )
  })

  output$timeline_plot <- plotly::renderPlotly({
    row <- current_session()
    dat <- session_data()
    plot_title <- paste0(
      "Detection timeline | ",
      format(row$real_start[[1]], "%Y-%m-%d", tz = "UTC")
    )

    p <- plotly::plot_ly(
      data = dat,
      x = ~toa,
      y = ~recorder_lane,
      type = "scatter",
      mode = "markers",
      source = "timeline",
      key = ~rec_id,
      text = ~paste0(
        rec_id,
        "<br>",
        mic_id,
        "<br>",
        format(toa, "%H:%M:%S", tz = "UTC")
      ),
      hoverinfo = "text",
      marker = list(size = 12, color = "#1f78b4", opacity = 0.85)
    )

    p %>%
      plotly::layout(
        title = list(text = plot_title),
        dragmode = "select",
        xaxis = list(
          title = "",
          range = list(row$real_start[[1]], row$real_stop[[1]]),
          tickformat = "%H:%M:%S",
          showgrid = FALSE,
          zeroline = FALSE,
          rangeslider = list(visible = FALSE)
        ),
        yaxis = list(
          title = "",
          tickmode = "array",
          tickvals = seq_along(recorder_levels),
          ticktext = recorder_levels,
          range = list(0.5, length(recorder_levels) + 0.5),
          showgrid = FALSE,
          zeroline = FALSE,
          fixedrange = TRUE
        ),
        margin = list(l = 85, r = 20, t = 55, b = 55)
      ) %>%
      plotly::config(
        scrollZoom = TRUE,
        displaylogo = FALSE,
        modeBarButtonsToRemove = c("lasso2d", "autoScale2d")
      )
  })

  selected_rows <- reactive({
    selected <- plotly::event_data("plotly_selected", source = "timeline")
    if (is.null(selected) || is.null(selected$key)) {
      return(session_data()[FALSE, , drop = FALSE])
    }

    session_data() %>%
      filter(rec_id %in% selected$key) %>%
      arrange(toa, mic_id)
  })

  output$selected_spectrograms <- renderUI({
    rows <- selected_rows()
    if (nrow(rows) == 0) {
      return(tags$div("Box-select points to show spectrograms. Use the mode bar to switch back to pan when navigating."))
    }

    img_path <- tryCatch(
      ensure_comparison_spectrogram_png(rows, wav_root, spectro_cache_dir),
      error = function(e) e
    )
    if (inherits(img_path, "error")) {
      return(tags$div(
        style = "margin: 10px 0; color: #9d1c1c;",
        tags$strong("Could not create comparison spectrogram"),
        tags$div(conditionMessage(img_path))
      ))
    }

    tags$div(
      tags$strong(paste(nrow(rows), "selected detection(s)")),
      tags$img(
        src = encode_image(img_path),
        style = "width: 100%; margin-top: 8px; border: 1px solid #ddd;"
      )
    )
  })

  output$clicked_detection <- renderText({
    click <- plotly::event_data("plotly_click", source = "timeline")
    if (is.null(click) || is.null(click$key)) {
      return("Click a point to inspect its detection ID.")
    }

    selected <- session_data() %>% filter(rec_id == click$key[[1]])
    if (nrow(selected) == 0) {
      return("Clicked detection is no longer in the current session.")
    }

    paste(
      "rec_id:", selected$rec_id[[1]],
      "\nmic_id:", selected$mic_id[[1]],
      "\ntime:", format(selected$toa[[1]], "%Y-%m-%d %H:%M:%S", tz = "UTC")
    )
  })
}

app <- shinyApp(ui, server)
runApp(app, launch.browser = interactive())
