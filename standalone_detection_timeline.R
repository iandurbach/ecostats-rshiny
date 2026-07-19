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
  tags$script(HTML("
    document.addEventListener('keydown', function(e) {
      if (!(e.ctrlKey || e.metaKey)) return;
      if (e.key === '+' || e.key === '=') {
        e.preventDefault();
        Shiny.setInputValue('spectro_zoom_delta', 1, {priority: 'event'});
      } else if (e.key === '-') {
        e.preventDefault();
        Shiny.setInputValue('spectro_zoom_delta', -1, {priority: 'event'});
      } else if (e.key === '0') {
        e.preventDefault();
        Shiny.setInputValue('spectro_zoom_reset', Math.random(), {priority: 'event'});
      }
    });
  ")),
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
      plotly::plotlyOutput("timeline_plot", height = "420px"),
      div(
        style = "margin-top: 10px;",
        textAreaInput("action_notes", "Notes", value = "", rows = 2),
        div(
          style = "display: flex; align-items: center; gap: 8px;",
          actionButton("group_selected", "Group"),
          actionButton("remove_selected", "Remove"),
          actionButton("clear_selection", "Clear selection"),
          downloadButton("export_groups", "Export RData"),
          tags$span(textOutput("action_summary", inline = TRUE))
        )
      )
    ),
    column(
      6,
      div(
        style = "height: 420px; overflow: auto; border-left: 1px solid #ddd; padding-left: 12px;",
        div(
          style = "display: flex; align-items: center; gap: 6px; margin-bottom: 8px;",
          actionButton("spectro_zoom_out", "Zoom out"),
          actionButton("spectro_zoom_in", "Zoom in"),
          actionButton("spectro_zoom_reset_button", "Reset"),
          tags$span(textOutput("spectro_zoom_label", inline = TRUE))
        ),
        uiOutput("selected_spectrograms")
      )
    )
  )
)

server <- function(input, output, session) {
  current_session_index <- reactiveVal(initial_session_index)
  spectro_zoom <- reactiveVal(1)
  groups_df <- reactiveVal(empty_grouped_detections())
  removed_points_df <- reactiveVal(empty_removed_detections())
  next_group_id <- reactiveVal(1L)
  processed_rec_ids <- reactiveVal(character(0))
  action_selected_rec_ids <- reactiveVal(character(0))
  current_x_range <- reactiveVal(NULL)
  selection_revision <- reactiveVal(0L)

  observeEvent(input$prev_session, {
    current_session_index(max(1L, current_session_index() - 1L))
    action_selected_rec_ids(character(0))
    current_x_range(NULL)
  })

  observeEvent(input$next_session, {
    current_session_index(min(nrow(sessions), current_session_index() + 1L))
    action_selected_rec_ids(character(0))
    current_x_range(NULL)
  })

  update_spectro_zoom <- function(delta) {
    spectro_zoom(min(4, max(0.5, spectro_zoom() + delta * 0.25)))
  }

  observeEvent(input$spectro_zoom_out, update_spectro_zoom(-1))
  observeEvent(input$spectro_zoom_in, update_spectro_zoom(1))
  observeEvent(input$spectro_zoom_reset_button, spectro_zoom(1))
  observeEvent(input$spectro_zoom_delta, update_spectro_zoom(input$spectro_zoom_delta))
  observeEvent(input$spectro_zoom_reset, spectro_zoom(1))

  output$spectro_zoom_label <- renderText({
    paste0(round(spectro_zoom() * 100), "%")
  })

  observeEvent(plotly::event_data("plotly_relayout", source = "timeline"), {
    relayout <- plotly::event_data("plotly_relayout", source = "timeline")
    if (is.null(relayout)) return()
    if (!is.null(relayout[["xaxis.range[0]"]]) && !is.null(relayout[["xaxis.range[1]"]])) {
      current_x_range(list(relayout[["xaxis.range[0]"]], relayout[["xaxis.range[1]"]]))
    } else if (isTRUE(relayout[["xaxis.autorange"]])) {
      current_x_range(NULL)
    }
  }, ignoreNULL = TRUE)

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
    selection_revision()
    processed <- processed_rec_ids()
    action_selected <- action_selected_rec_ids()
    dat_action_selected <- dat[dat$rec_id %in% action_selected & !dat$rec_id %in% processed, , drop = FALSE]
    dat_active <- dat[!dat$rec_id %in% processed & !dat$rec_id %in% action_selected, , drop = FALSE]
    dat_processed <- dat[dat$rec_id %in% processed, , drop = FALSE]
    plot_title <- paste0(
      "Detection timeline | ",
      format(row$real_start[[1]], "%Y-%m-%d", tz = "UTC")
    )
    x_range <- isolate(current_x_range())
    if (is.null(x_range)) {
      x_range <- list(row$real_start[[1]], row$real_stop[[1]])
    }

    p <- plotly::plot_ly(source = "timeline")
    if (nrow(dat_active) > 0) {
      p <- p %>%
        plotly::add_markers(
          data = dat_active,
          x = ~toa,
          y = ~recorder_lane,
          key = ~rec_id,
          text = ~paste0(rec_id, "<br>", mic_id, "<br>", format(toa, "%H:%M:%S", tz = "UTC")),
          hoverinfo = "text",
          marker = list(size = 12, color = "#d7191c", opacity = 0.9),
          showlegend = FALSE
        )
    }
    if (nrow(dat_action_selected) > 0) {
      p <- p %>%
        plotly::add_markers(
          data = dat_action_selected,
          x = ~toa,
          y = ~recorder_lane,
          key = ~rec_id,
          text = ~paste0(rec_id, "<br>", mic_id, "<br>", format(toa, "%H:%M:%S", tz = "UTC")),
          hoverinfo = "text",
          marker = list(
            size = 16,
            color = "#d7191c",
            opacity = 1,
            line = list(color = "black", width = 2)
          ),
          showlegend = FALSE
        )
    }
    if (nrow(dat_processed) > 0) {
      p <- p %>%
        plotly::add_markers(
          data = dat_processed,
          x = ~toa,
          y = ~recorder_lane,
          key = ~rec_id,
          text = ~paste0(rec_id, "<br>", mic_id, "<br>", format(toa, "%H:%M:%S", tz = "UTC")),
          hoverinfo = "text",
          marker = list(size = 12, color = "rgba(128,128,128,0.1)"),
          showlegend = FALSE
        )
    }

    p %>%
      plotly::layout(
        title = list(text = plot_title),
        dragmode = "select",
        uirevision = row$session_id[[1]],
        xaxis = list(
          title = "",
          range = x_range,
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
    ids <- action_selected_rec_ids()
    if (length(ids) == 0) {
      return(session_data()[FALSE, , drop = FALSE])
    }

    session_data() %>%
      filter(rec_id %in% ids) %>%
      arrange(toa, mic_id)
  })

  observeEvent(plotly::event_data("plotly_selected", source = "timeline"), {
    selected <- plotly::event_data("plotly_selected", source = "timeline")
    if (is.null(selected) || is.null(selected$key)) {
      return()
    }
    selected_ids <- setdiff(unique(as.character(selected$key)), processed_rec_ids())
    action_selected_rec_ids(unique(c(action_selected_rec_ids(), selected_ids)))
    selection_revision(selection_revision() + 1L)
  }, ignoreNULL = FALSE)

  observeEvent(plotly::event_data("plotly_click", source = "timeline"), {
    click <- plotly::event_data("plotly_click", source = "timeline")
    if (is.null(click) || is.null(click$key)) return()
    clicked <- click$key[[1]]
    if (clicked %in% processed_rec_ids()) return()
    action_selected_rec_ids(toggle_rec_id_selection(action_selected_rec_ids(), clicked))
    selection_revision(selection_revision() + 1L)
  }, ignoreNULL = TRUE)

  observeEvent(input$group_selected, {
    rows <- selected_rows()
    if (nrow(rows) == 0) {
      showNotification("No detections selected for grouping.", type = "warning")
      return()
    }
    group_id <- next_group_id()
    new_rows <- format_detection_action_rows(rows, notes = input$action_notes, group_id = group_id)
    groups_df(dplyr::bind_rows(groups_df(), new_rows))
    processed_rec_ids(unique(c(processed_rec_ids(), rows$rec_id)))
    action_selected_rec_ids(setdiff(action_selected_rec_ids(), rows$rec_id))
    selection_revision(selection_revision() + 1L)
    next_group_id(group_id + 1L)
    showNotification(paste("Added", nrow(rows), "detection(s) to group", group_id), type = "message")
  })

  observeEvent(input$remove_selected, {
    rows <- selected_rows()
    if (nrow(rows) == 0) {
      showNotification("No detections selected for removal.", type = "warning")
      return()
    }
    new_rows <- format_detection_action_rows(rows, notes = input$action_notes)
    removed_points_df(dplyr::bind_rows(removed_points_df(), new_rows))
    processed_rec_ids(unique(c(processed_rec_ids(), rows$rec_id)))
    action_selected_rec_ids(setdiff(action_selected_rec_ids(), rows$rec_id))
    selection_revision(selection_revision() + 1L)
    showNotification(paste("Marked", nrow(rows), "detection(s) as removed."), type = "message")
  })

  observeEvent(input$clear_selection, {
    action_selected_rec_ids(character(0))
    selection_revision(selection_revision() + 1L)
  })

  output$action_summary <- renderText({
    paste0(
      nrow(groups_df()),
      " grouped rows | ",
      nrow(removed_points_df()),
      " removed rows | ",
      length(action_selected_rec_ids()),
      " selected"
    )
  })

  output$export_groups <- downloadHandler(
    filename = function() {
      paste0("detection_groups_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".RData")
    },
    content = function(file) {
      groups <- groups_df()
      removed_points <- removed_points_df()
      save(groups, removed_points, file = file)
    }
  )

  output$selected_spectrograms <- renderUI({
    rows <- selected_rows()
    if (nrow(rows) == 0) {
      return(tags$div("Box-select points to show spectrograms. Use the mode bar to switch back to pan when navigating."))
    }

    img_path <- tryCatch(
      write_current_comparison_spectrogram_png(rows, wav_root, spectro_cache_dir),
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
        src = paste0(encode_image(img_path), "#v=", as.numeric(Sys.time())),
        style = paste0(
          "width: ",
          round(spectro_zoom() * 100),
          "%; max-width: none; margin-top: 8px; border: 1px solid #ddd;"
        )
      )
    )
  })

}

app <- shinyApp(ui, server)
runApp(app, launch.browser = interactive())
