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
for (package in c("leaflet", "leaflet.extras2", "geosphere")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(
      "Package '",
      package,
      "' is required for this prototype. Install it with: install.packages('",
      package,
      "')"
    )
  }
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
mic_data <- db_data$micData
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
      8,
      plotly::plotlyOutput("timeline_plot", height = "420px"),
      div(
        style = "margin-top: 10px;",
        textAreaInput("action_notes", "Notes", value = "", rows = 2),
        div(
          style = "display: flex; align-items: center; gap: 8px;",
          actionButton("group_selected", "Group"),
          actionButton("remove_selected", "Remove"),
          actionButton("ungroup_selected", "Ungroup"),
          actionButton("clear_selection", "Clear selection"),
          downloadButton("export_groups", "Export RData"),
          tags$span(textOutput("action_summary", inline = TRUE))
        ),
        div(
          style = "display: flex; align-items: center; gap: 8px; margin-top: 8px;",
          actionButton("prev_group", "Previous group"),
          actionButton("next_group", "Next group"),
          tags$span(textOutput("group_review_summary", inline = TRUE))
        )
      ),
      div(
        style = "margin-top: 14px;",
        leaflet::leafletOutput("selection_map", width = "100%", height = 360)
      )
    ),
    column(
      4,
      div(
        style = "height: 800px; overflow: auto; border-left: 1px solid #ddd; padding-left: 12px;",
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
  group_membership <- reactiveVal(empty_group_membership())
  removed_membership <- reactiveVal(empty_removed_membership())
  suspect_bearing_state <- reactiveVal(empty_suspect_bearing_state())
  next_group_id <- reactiveVal(1L)
  current_review_group_id <- reactiveVal(NULL)
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

  groups_df <- reactive({
    export_grouped_detections(group_membership(), timeline_data)
  })

  removed_points_df <- reactive({
    export_removed_detections(removed_membership(), timeline_data)
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
    dat$status <- detection_point_status(dat$rec_id, group_membership(), removed_membership())
    dat$selected_color <- ifelse(
      dat$status == "grouped",
      "rgba(144,238,144,0.2)",
      ifelse(dat$status == "removed", "rgba(128,128,128,0.1)", "#d7191c")
    )
    action_selected <- action_selected_rec_ids()
    dat_action_selected <- dat[dat$rec_id %in% action_selected, , drop = FALSE]
    dat_active <- dat[dat$status == "active" & !dat$rec_id %in% action_selected, , drop = FALSE]
    dat_grouped <- dat[dat$status == "grouped" & !dat$rec_id %in% action_selected, , drop = FALSE]
    dat_removed <- dat[dat$status == "removed" & !dat$rec_id %in% action_selected, , drop = FALSE]
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
            color = dat_action_selected$selected_color,
            opacity = 1,
            line = list(color = "black", width = 2)
          ),
          showlegend = FALSE
        )
    }
    if (nrow(dat_grouped) > 0) {
      p <- p %>%
        plotly::add_markers(
          data = dat_grouped,
          x = ~toa,
          y = ~recorder_lane,
          key = ~rec_id,
          text = ~paste0(rec_id, "<br>", mic_id, "<br>", format(toa, "%H:%M:%S", tz = "UTC")),
          hoverinfo = "text",
          marker = list(size = 12, color = "rgba(144,238,144,0.2)"),
          showlegend = FALSE
        )
    }
    if (nrow(dat_removed) > 0) {
      p <- p %>%
        plotly::add_markers(
          data = dat_removed,
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

    rows <- session_data() %>%
      filter(rec_id %in% ids) %>%
      arrange(toa, mic_id)
    apply_suspect_bearing_state(rows, suspect_bearing_state())
  })

  output$selection_map <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addProviderTiles(
        leaflet::providers$Esri.WorldImagery,
        options = leaflet::providerTileOptions(noWrap = TRUE)
      ) %>%
      leaflet::setView(lng = 0, lat = 0, zoom = 3)
  })

  outputOptions(output, "selection_map", suspendWhenHidden = FALSE)

  observe({
    valid_mics <- mic_data[
      !is.na(mic_data$lat) & !is.na(mic_data$lng),
      ,
      drop = FALSE
    ]
    proxy <- leaflet::leafletProxy("selection_map") %>%
      leaflet::clearMarkers()
    if (nrow(valid_mics) == 0) return()

    proxy %>%
      leaflet::addCircleMarkers(
        lat = valid_mics$lat,
        lng = valid_mics$lng,
        radius = 6,
        label = paste0("Recorder: ", valid_mics$mic_id),
        color = "red",
        stroke = FALSE,
        fillOpacity = 0.5
      ) %>%
      leaflet::fitBounds(
        lng1 = min(valid_mics$lng),
        lat1 = min(valid_mics$lat),
        lng2 = max(valid_mics$lng),
        lat2 = max(valid_mics$lat)
      )
  })

  observe({
    rows <- selected_rows()
    arrows <- tryCatch(
      prepare_bearing_arrows(rows, mic_data, suspect_bearing_state()),
      error = function(e) e
    )

    proxy <- leaflet::leafletProxy("selection_map") %>%
      leaflet::clearGroup("bearings")
    if (inherits(arrows, "error") || nrow(arrows) == 0) return()

    for (i in seq_len(nrow(arrows))) {
      proxy <- proxy %>%
        leaflet.extras2::addArrowhead(
          lng = c(arrows$lng[[i]], arrows$arrow_lng[[i]]),
          lat = c(arrows$lat[[i]], arrows$arrow_lat[[i]]),
          group = "bearings",
          layerId = paste0("arrow_", arrows$rec_id[[i]]),
          label = paste0(arrows$rec_id[[i]], " | ", arrows$mic_id[[i]]),
          color = arrows$color[[i]],
          opacity = 0.9,
          options = leaflet.extras2::arrowheadOptions(yawn = 40, fill = FALSE)
        )
    }
  })

  observeEvent(input$selection_map_shape_click, {
    click <- input$selection_map_shape_click
    if (is.null(click$id)) return()
    rec_id <- bearing_rec_id_from_layer_id(click$id)
    if (!rec_id %in% action_selected_rec_ids()) return()

    updated_state <- toggle_suspect_bearing_state(suspect_bearing_state(), rec_id)
    suspect_bearing_state(updated_state)
    current_value <- suspect_bearing_for_rec_ids(rec_id, updated_state)

    gm <- group_membership()
    if (nrow(gm) > 0 && rec_id %in% gm$rec_id) {
      gm$suspect_bearing[gm$rec_id == rec_id] <- current_value
      group_membership(gm)
    }
    rm <- removed_membership()
    if (nrow(rm) > 0 && rec_id %in% rm$rec_id) {
      rm$suspect_bearing[rm$rec_id == rec_id] <- current_value
      removed_membership(rm)
    }
  })

  observeEvent(plotly::event_data("plotly_selected", source = "timeline"), {
    selected <- plotly::event_data("plotly_selected", source = "timeline")
    if (is.null(selected) || is.null(selected$key)) {
      return()
    }
    selected_ids <- unique(as.character(selected$key))
    action_selected_rec_ids(unique(c(action_selected_rec_ids(), selected_ids)))
    selection_revision(selection_revision() + 1L)
  }, ignoreNULL = FALSE)

  observeEvent(plotly::event_data("plotly_click", source = "timeline"), {
    click <- plotly::event_data("plotly_click", source = "timeline")
    if (is.null(click) || is.null(click$key)) return()
    clicked <- click$key[[1]]
    action_selected_rec_ids(toggle_rec_id_selection(action_selected_rec_ids(), clicked))
    selection_revision(selection_revision() + 1L)
  }, ignoreNULL = TRUE)

  observeEvent(input$group_selected, {
    rows <- selected_rows()
    if (nrow(rows) == 0) {
      showNotification("No detections selected for grouping.", type = "warning")
      return()
    }
    existing_groups <- selected_group_ids(rows$rec_id, group_membership())
    if (length(existing_groups) > 1) {
      showNotification("Select points from only one existing group when adding detections.", type = "warning")
      return()
    }

    group_id <- if (length(existing_groups) == 1) existing_groups[[1]] else next_group_id()
    gm <- group_membership()
    rm <- removed_membership()
    ids_to_add <- setdiff(rows$rec_id, gm$rec_id[gm$group_ID == group_id])
    new_rows <- format_group_membership_rows(
      rows[rows$rec_id %in% ids_to_add, , drop = FALSE],
      notes = input$action_notes,
      group_id = group_id
    )
    group_membership(dplyr::bind_rows(gm, new_rows))
    removed_membership(rm[!rm$rec_id %in% rows$rec_id, , drop = FALSE])
    action_selected_rec_ids(setdiff(action_selected_rec_ids(), rows$rec_id))
    current_review_group_id(group_id)
    selection_revision(selection_revision() + 1L)
    if (length(existing_groups) == 0) {
      next_group_id(group_id + 1L)
    }
    showNotification(paste("Added", nrow(rows), "detection(s) to group", group_id), type = "message")
  })

  observeEvent(input$remove_selected, {
    rows <- selected_rows()
    if (nrow(rows) == 0) {
      showNotification("No detections selected for removal.", type = "warning")
      return()
    }
    gm <- group_membership()
    rm <- removed_membership()
    new_rows <- format_removed_membership_rows(rows, notes = input$action_notes)
    group_membership(gm[!gm$rec_id %in% rows$rec_id, , drop = FALSE])
    removed_membership(dplyr::bind_rows(
      rm[!rm$rec_id %in% rows$rec_id, , drop = FALSE],
      new_rows
    ))
    action_selected_rec_ids(setdiff(action_selected_rec_ids(), rows$rec_id))
    selection_revision(selection_revision() + 1L)
    showNotification(paste("Marked", nrow(rows), "detection(s) as removed."), type = "message")
  })

  observeEvent(input$ungroup_selected, {
    rows <- selected_rows()
    group_ids <- selected_group_ids(rows$rec_id, group_membership())
    if (length(group_ids) == 0) {
      reviewed <- current_review_group_id()
      if (!is.null(reviewed) && reviewed %in% group_membership()$group_ID) {
        group_ids <- reviewed
      }
    }
    if (length(group_ids) == 0) {
      showNotification("No group selected to ungroup.", type = "warning")
      return()
    }
    if (length(group_ids) > 1) {
      showNotification("Select one group before ungrouping.", type = "warning")
      return()
    }

    group_id <- group_ids[[1]]
    gm <- group_membership()
    removed_ids <- gm$rec_id[gm$group_ID == group_id]
    group_membership(gm[gm$group_ID != group_id, , drop = FALSE])
    action_selected_rec_ids(setdiff(action_selected_rec_ids(), removed_ids))
    current_review_group_id(NULL)
    selection_revision(selection_revision() + 1L)
    showNotification(paste("Removed group", group_id), type = "message")
  })

  review_group <- function(direction) {
    gm <- group_membership()
    group_ids <- sort(unique(gm$group_ID))
    if (length(group_ids) == 0) {
      showNotification("No groups have been created yet.", type = "warning")
      return()
    }

    current <- current_review_group_id()
    if (is.null(current) || !current %in% group_ids) {
      group_id <- if (direction > 0) group_ids[[1]] else utils::tail(group_ids, 1)
    } else {
      current_index <- match(current, group_ids)
      next_index <- ((current_index - 1L + direction) %% length(group_ids)) + 1L
      group_id <- group_ids[[next_index]]
    }

    group_rec_ids <- gm$rec_id[gm$group_ID == group_id]
    rows <- timeline_data[timeline_data$rec_id %in% group_rec_ids, , drop = FALSE]
    rows <- rows[order(rows$toa, rows$mic_id), , drop = FALSE]
    if (nrow(rows) == 0) {
      showNotification(paste("Group", group_id, "has no matching detections."), type = "warning")
      return()
    }

    session_id <- rows$session_id[[1]]
    session_index <- match(session_id, sessions$session_id)
    if (!is.na(session_index)) {
      current_session_index(session_index)
      current_x_range(group_review_range(rows, sessions[session_index, , drop = FALSE]))
    }
    action_selected_rec_ids(group_rec_ids)
    current_review_group_id(group_id)
    selection_revision(selection_revision() + 1L)
  }

  observeEvent(input$prev_group, {
    review_group(-1L)
  })

  observeEvent(input$next_group, {
    review_group(1L)
  })

  observeEvent(input$clear_selection, {
    action_selected_rec_ids(character(0))
    selection_revision(selection_revision() + 1L)
  })

  output$group_review_summary <- renderText({
    group_id <- current_review_group_id()
    group_count <- length(unique(group_membership()$group_ID))
    if (is.null(group_id) || group_count == 0) {
      return(paste0(group_count, " group(s)"))
    }
    paste0("Reviewing group ", group_id, " | ", group_count, " group(s)")
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
