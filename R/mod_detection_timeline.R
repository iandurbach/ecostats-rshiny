#' detection_timeline UI Function
#'
#' @noRd
#' @importFrom shiny NS tagList tags HTML div actionButton downloadButton textOutput uiOutput
#' @importFrom shinyFiles shinyFilesButton shinyDirButton
mod_detection_timeline_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(tags$style(HTML("
      html, body, .container-fluid {
        height: 100%;
        overflow: hidden;
      }
      body {
        padding: 10px 14px;
      }
      .container-fluid {
        padding-left: 0;
        padding-right: 0;
      }
      .detection-shell {
        --sidebar-width: 290px;
        display: grid;
        grid-template-columns: var(--sidebar-width) minmax(0, 1fr);
        gap: 12px;
        height: calc(100vh - 20px);
        min-height: 0;
      }
      .detection-shell.sidebar-collapsed {
        --sidebar-width: 42px;
      }
      .detection-sidebar {
        min-width: 0;
        min-height: 0;
        border-right: 1px solid #ddd;
        padding-right: 10px;
        overflow: auto;
      }
      .detection-shell.sidebar-collapsed .sidebar-body {
        display: none;
      }
      .sidebar-toggle-row {
        display: flex;
        justify-content: flex-end;
        margin-bottom: 8px;
      }
      .sidebar-section {
        margin-bottom: 14px;
      }
      .sidebar-section .btn {
        width: 100%;
        margin-bottom: 6px;
        text-align: left;
      }
      .sidebar-label {
        font-weight: 700;
        font-size: 13px;
        margin-bottom: 6px;
      }
      .sidebar-status {
        font-size: 12px;
        line-height: 1.3;
        color: #555;
        overflow-wrap: anywhere;
      }
      .workspace {
        min-width: 0;
        min-height: 0;
        overflow: hidden;
      }
      .app-header {
        display: flex;
        align-items: center;
        gap: 10px;
        height: 40px;
        margin-bottom: 6px;
      }
      .app-title {
        font-size: 22px;
        font-weight: 700;
        margin-right: 8px;
        white-space: nowrap;
      }
      .app-grid {
        --left-pane-width: 54%;
        display: grid;
        grid-template-columns: minmax(520px, var(--left-pane-width)) 8px minmax(420px, 1fr);
        height: calc(100% - 46px);
        min-height: 0;
      }
      .left-workspace,
      .spectro-pane {
        min-height: 0;
        overflow: hidden;
      }
      .left-workspace {
        --top-pane-height: 48%;
        display: grid;
        grid-template-rows: minmax(250px, var(--top-pane-height)) 8px minmax(220px, 1fr);
        min-width: 0;
        min-height: 0;
      }
      .timeline-actions-pane {
        display: grid;
        grid-template-rows: minmax(0, 1fr) auto;
        gap: 8px;
        min-height: 0;
        overflow: hidden;
      }
      .timeline-wrap,
      .actions-wrap,
      .map-wrap,
      .spectro-pane {
        min-width: 0;
      }
      .timeline-wrap,
      .map-wrap {
        min-height: 0;
        overflow: hidden;
      }
      .timeline-wrap .shiny-plot-output,
      .timeline-wrap .plotly,
      .timeline-wrap .html-widget,
      .timeline-wrap .html-widget-output,
      .map-wrap .leaflet,
      .map-wrap .shiny-bound-output {
        height: 100% !important;
        width: 100% !important;
      }
      .actions-wrap {
        display: grid;
        grid-template-columns: 280px 1fr;
        gap: 8px 10px;
        align-items: end;
      }
      .notes-compact .form-group {
        margin-bottom: 0;
      }
      .notes-compact textarea {
        height: 44px;
        resize: none;
      }
      .button-row {
        display: flex;
        align-items: center;
        gap: 6px;
        flex-wrap: wrap;
      }
      .status-row {
        font-size: 13px;
        line-height: 1.2;
        margin-top: 4px;
      }
      .splitter {
        background: #e6e6e6;
        border-radius: 4px;
        transition: background 120ms ease;
      }
      .splitter:hover,
      .splitter.dragging {
        background: #b8b8b8;
      }
      .vertical-splitter {
        cursor: col-resize;
        margin: 0 3px;
      }
      .horizontal-splitter {
        cursor: row-resize;
        margin: 3px 0;
      }
      .spectro-pane {
        border-left: 1px solid #ddd;
        padding-left: 12px;
        display: flex;
        flex-direction: column;
      }
      .spectro-controls {
        display: flex;
        align-items: center;
        gap: 6px;
        margin-bottom: 8px;
        flex: 0 0 auto;
      }
      .spectro-output {
        flex: 1 1 auto;
        overflow: auto;
        min-height: 0;
      }
      .spectro-output img {
        display: block;
      }
    "))),
    tags$script(HTML(sprintf("
      document.addEventListener('keydown', function(e) {
        if (!(e.ctrlKey || e.metaKey)) return;
        if (e.key === '+' || e.key === '=') {
          e.preventDefault();
          Shiny.setInputValue('%s', 1, {priority: 'event'});
        } else if (e.key === '-') {
          e.preventDefault();
          Shiny.setInputValue('%s', -1, {priority: 'event'});
        } else if (e.key === '0') {
          e.preventDefault();
          Shiny.setInputValue('%s', Math.random(), {priority: 'event'});
        }
      });

      function detectionTimelineClamp(value, min, max) {
        return Math.max(min, Math.min(max, value));
      }

      function detectionTimelineNotifyPaneResize() {
        window.dispatchEvent(new Event('resize'));
      }

      document.addEventListener('DOMContentLoaded', function() {
        var root = document.getElementById('%s');
        if (!root) return;
        var shell = root.querySelector('.detection-shell');
        var grid = root.querySelector('.app-grid');
        var leftPane = root.querySelector('.left-workspace');
        var vertical = root.querySelector('.vertical-splitter');
        var horizontal = root.querySelector('.horizontal-splitter');
        var sidebarToggle = root.querySelector('.sidebar-toggle');

        if (sidebarToggle && shell) {
          sidebarToggle.addEventListener('click', function() {
            shell.classList.toggle('sidebar-collapsed');
            setTimeout(detectionTimelineNotifyPaneResize, 50);
          });
        }

        if (!grid || !leftPane || !vertical || !horizontal) return;

        var storedLeft = localStorage.getItem('detectionTimeline.leftPaneWidth');
        var storedTop = localStorage.getItem('detectionTimeline.topPaneHeight');
        if (storedLeft) grid.style.setProperty('--left-pane-width', storedLeft);
        if (storedTop) leftPane.style.setProperty('--top-pane-height', storedTop);
        setTimeout(detectionTimelineNotifyPaneResize, 250);
        setTimeout(detectionTimelineNotifyPaneResize, 1000);

        vertical.addEventListener('pointerdown', function(e) {
          e.preventDefault();
          vertical.classList.add('dragging');
          vertical.setPointerCapture(e.pointerId);

          function move(ev) {
            var rect = grid.getBoundingClientRect();
            var width = detectionTimelineClamp(ev.clientX - rect.left, 520, rect.width - 420);
            grid.style.setProperty('--left-pane-width', width + 'px');
            localStorage.setItem('detectionTimeline.leftPaneWidth', width + 'px');
            detectionTimelineNotifyPaneResize();
          }

          function up(ev) {
            vertical.classList.remove('dragging');
            vertical.releasePointerCapture(ev.pointerId);
            vertical.removeEventListener('pointermove', move);
            vertical.removeEventListener('pointerup', up);
            detectionTimelineNotifyPaneResize();
          }

          vertical.addEventListener('pointermove', move);
          vertical.addEventListener('pointerup', up);
        });

        horizontal.addEventListener('pointerdown', function(e) {
          e.preventDefault();
          horizontal.classList.add('dragging');
          horizontal.setPointerCapture(e.pointerId);

          function move(ev) {
            var rect = leftPane.getBoundingClientRect();
            var height = detectionTimelineClamp(ev.clientY - rect.top, 250, rect.height - 220);
            leftPane.style.setProperty('--top-pane-height', height + 'px');
            localStorage.setItem('detectionTimeline.topPaneHeight', height + 'px');
            detectionTimelineNotifyPaneResize();
          }

          function up(ev) {
            horizontal.classList.remove('dragging');
            horizontal.releasePointerCapture(ev.pointerId);
            horizontal.removeEventListener('pointermove', move);
            horizontal.removeEventListener('pointerup', up);
            detectionTimelineNotifyPaneResize();
          }

          horizontal.addEventListener('pointermove', move);
          horizontal.addEventListener('pointerup', up);
        });

        vertical.addEventListener('dblclick', function() {
          localStorage.removeItem('detectionTimeline.leftPaneWidth');
          grid.style.removeProperty('--left-pane-width');
          detectionTimelineNotifyPaneResize();
        });

        horizontal.addEventListener('dblclick', function() {
          localStorage.removeItem('detectionTimeline.topPaneHeight');
          leftPane.style.removeProperty('--top-pane-height');
          detectionTimelineNotifyPaneResize();
        });
      });
    ", ns("spectro_zoom_delta"), ns("spectro_zoom_delta"), ns("spectro_zoom_reset"), ns("detection_timeline_root")))),
    div(
      id = ns("detection_timeline_root"),
      div(
        class = "detection-shell",
        div(
          class = "detection-sidebar",
          div(class = "sidebar-toggle-row", actionButton(ns("toggle_sidebar"), "Menu", class = "sidebar-toggle btn-sm")),
          div(
            class = "sidebar-body",
            div(
              class = "sidebar-section",
              div(class = "sidebar-label", "Inputs"),
              shinyFilesButton(ns("databases"), "Select SQLite database(s)", "Select SQLite database files", multiple = TRUE),
              shinyFiles::shinyDirButton(ns("wav_root"), "Select WAV root folder", "Select WAV root folder"),
              actionButton(ns("load_data"), "Load data"),
              div(class = "sidebar-status", textOutput(ns("input_status")))
            ),
            div(
              class = "sidebar-section",
              div(class = "sidebar-label", "Export"),
              downloadButton(ns("export_groups"), "Export RData"),
              div(class = "sidebar-status", textOutput(ns("export_status")))
            )
          )
        ),
        div(
          class = "workspace",
          div(
            class = "app-header",
            div(class = "app-title", "vocomatcher"),
            actionButton(ns("prev_session"), "Previous session"),
            actionButton(ns("next_session"), "Next session"),
            tags$span(textOutput(ns("session_summary"), inline = TRUE))
          ),
          div(
            class = "app-grid",
            div(
              class = "left-workspace",
              div(
                class = "timeline-actions-pane",
                div(class = "timeline-wrap", plotly::plotlyOutput(ns("timeline_plot"), height = "100%")),
                div(
                  div(
                    class = "actions-wrap",
                    div(class = "notes-compact", textAreaInput(ns("action_notes"), "Notes", value = "", rows = 1)),
                    div(
                      div(
                        class = "button-row",
                        actionButton(ns("group_selected"), "Group"),
                        actionButton(ns("remove_selected"), "Remove"),
                        actionButton(ns("ungroup_selected"), "Ungroup"),
                        actionButton(ns("clear_selection"), "Clear selection"),
                        actionButton(ns("prev_group"), "Previous group"),
                        actionButton(ns("next_group"), "Next group")
                      ),
                      div(
                        class = "status-row",
                        tags$span(textOutput(ns("action_summary"), inline = TRUE)),
                        tags$span(" | "),
                        tags$span(textOutput(ns("group_review_summary"), inline = TRUE))
                      )
                    )
                  )
                )
              ),
              div(class = "splitter horizontal-splitter", title = "Drag to resize timeline/map panes. Double-click to reset."),
              div(class = "map-wrap", leaflet::leafletOutput(ns("selection_map"), width = "100%", height = "100%"))
            ),
            div(class = "splitter vertical-splitter", title = "Drag to resize left/right panes. Double-click to reset."),
            div(
              class = "spectro-pane",
              div(
                class = "spectro-controls",
                actionButton(ns("spectro_zoom_out"), "Zoom out"),
                actionButton(ns("spectro_zoom_in"), "Zoom in"),
                actionButton(ns("spectro_zoom_reset_button"), "Reset"),
                tags$span(textOutput(ns("spectro_zoom_label"), inline = TRUE))
              ),
              div(class = "spectro-output", uiOutput(ns("selected_spectrograms")))
            )
          )
        )
      )
    )
  )
}

#' detection_timeline Server Function
#'
#' @noRd
#' @importFrom shiny moduleServer reactiveVal reactive observe observeEvent req renderText renderUI showNotification validate need downloadHandler outputOptions
#' @importFrom shinyFiles shinyFileChoose shinyDirChoose getVolumes parseFilePaths parseDirPath
#' @importFrom fs path_home
mod_detection_timeline_server <- function(id, session_gap_minutes = 30) {
  moduleServer(id, function(input, output, session) {
    volumes <- c(Home = fs::path_home(), shinyFiles::getVolumes()())
    shinyFileChoose(input, "databases", session = session, roots = volumes, filetypes = c("sqlite3"))
    shinyFiles::shinyDirChoose(input, "wav_root", session = session, roots = volumes, allowDirCreate = FALSE)

    selected_db_paths <- reactiveVal(character(0))
    selected_wav_root <- reactiveVal(NULL)
    mic_data <- reactiveVal(NULL)
    timeline_data <- reactiveVal(NULL)
    sessions_data <- reactiveVal(NULL)
    recorder_levels <- reactiveVal(character(0))
    current_session_index <- reactiveVal(1L)
    spectro_zoom <- reactiveVal(1)
    group_membership <- reactiveVal(empty_group_membership())
    removed_membership <- reactiveVal(empty_removed_membership())
    suspect_bearing_state <- reactiveVal(empty_suspect_bearing_state())
    next_group_id <- reactiveVal(1L)
    current_review_group_id <- reactiveVal(NULL)
    action_selected_rec_ids <- reactiveVal(character(0))
    current_x_range <- reactiveVal(NULL)
    selection_revision <- reactiveVal(0L)
    spectro_cache_dir <- reactiveVal(file.path(tempdir(), paste0("detection_timeline_spectros_", session$token)))

    observeEvent(input$databases, {
      req(!is.integer(input$databases))
      f <- shinyFiles::parseFilePaths(volumes, input$databases)
      selected_db_paths(normalizePath(f$datapath, mustWork = FALSE))
    })

    observeEvent(input$wav_root, {
      req(!is.null(input$wav_root))
      if (is.integer(input$wav_root)) return()
      dir_path <- shinyFiles::parseDirPath(volumes, input$wav_root)
      if (length(dir_path) == 0 || is.na(dir_path)) {
        showNotification("Could not read WAV folder path.", type = "error")
      } else {
        selected_wav_root(dir_path)
      }
    })

    reset_workspace_state <- function() {
      group_membership(empty_group_membership())
      removed_membership(empty_removed_membership())
      suspect_bearing_state(empty_suspect_bearing_state())
      next_group_id(1L)
      current_review_group_id(NULL)
      action_selected_rec_ids(character(0))
      current_x_range(NULL)
      selection_revision(selection_revision() + 1L)
      spectro_zoom(1)
    }

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

    observeEvent(input$load_data, {
      db_paths <- selected_db_paths()
      wav_root <- selected_wav_root()
      if (length(db_paths) == 0) {
        showNotification("Select at least one SQLite detection database.", type = "warning")
        return()
      }
      if (is.null(wav_root) || !dir.exists(wav_root)) {
        showNotification("Select a valid WAV root folder.", type = "warning")
        return()
      }

      loaded <- tryCatch({
        db_data <- read_detection_databases(db_paths)
        parsed_rec_data <- parse_rec_data(db_data$recData)
        recording_intervals <- read_recording_intervals_from_databases(db_paths)
        sessions <- build_recording_sessions(
          recording_intervals,
          gap_seconds = session_gap_minutes * 60
        )
        timeline <- assign_detections_to_sessions(parsed_rec_data, sessions)
        levels <- attr(timeline, "recorder_levels")
        session_counts <- timeline |>
          dplyr::filter(!is.na(session_id)) |>
          dplyr::count(session_id, name = "n_detections")
        sessions <- sessions |>
          dplyr::left_join(session_counts, by = "session_id") |>
          dplyr::mutate(n_detections = ifelse(is.na(n_detections), 0L, n_detections))
        if (nrow(sessions) == 0) {
          stop("No recording sessions were found in Sound_Acquisition.")
        }
        list(mic_data = db_data$micData, timeline = timeline, sessions = sessions, levels = levels)
      }, error = function(e) e)

      if (inherits(loaded, "error")) {
        showNotification(conditionMessage(loaded), type = "error", duration = 10)
        return()
      }

      dir.create(spectro_cache_dir(), recursive = TRUE, showWarnings = FALSE)
      mic_data(loaded$mic_data)
      timeline_data(loaded$timeline)
      sessions_data(loaded$sessions)
      recorder_levels(loaded$levels)
      first_with_detections <- which(loaded$sessions$n_detections > 0)
      current_session_index(if (length(first_with_detections) > 0) first_with_detections[[1]] else 1L)
      reset_workspace_state()
      showNotification(
        paste("Loaded", nrow(loaded$timeline), "detections from", length(db_paths), "database(s)."),
        type = "message"
      )
    })

    output$input_status <- renderText({
      db_count <- length(selected_db_paths())
      wav_root <- selected_wav_root()
      loaded <- !is.null(timeline_data())
      paste0(
        db_count,
        " database(s) selected",
        if (!is.null(wav_root)) paste0(" | WAV: ", wav_root) else " | WAV: not selected",
        if (loaded) paste0(" | ", nrow(timeline_data()), " detections loaded") else ""
      )
    })

    output$export_status <- renderText({
      paste0(nrow(groups_df()), " grouped rows | ", nrow(removed_points_df()), " removed rows")
    })

    observeEvent(input$prev_session, {
      sessions <- sessions_data()
      req(sessions)
      current_session_index(max(1L, current_session_index() - 1L))
      action_selected_rec_ids(character(0))
      current_x_range(NULL)
    })

    observeEvent(input$next_session, {
      sessions <- sessions_data()
      req(sessions)
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

    timeline_event_data <- function(event) {
      suppressWarnings(plotly::event_data(event, source = session$ns("timeline")))
    }

    observeEvent(timeline_event_data("plotly_relayout"), {
      relayout <- timeline_event_data("plotly_relayout")
      if (is.null(relayout)) return()
      if (!is.null(relayout[["xaxis.range[0]"]]) && !is.null(relayout[["xaxis.range[1]"]])) {
        current_x_range(list(relayout[["xaxis.range[0]"]], relayout[["xaxis.range[1]"]]))
      } else if (isTRUE(relayout[["xaxis.autorange"]])) {
        current_x_range(NULL)
      }
    }, ignoreNULL = TRUE)

    current_session <- reactive({
      sessions <- sessions_data()
      req(sessions)
      sessions[current_session_index(), , drop = FALSE]
    })

    session_data <- reactive({
      timeline <- timeline_data()
      req(timeline)
      timeline |>
        dplyr::filter(session_id == current_session()$session_id[[1]]) |>
        dplyr::arrange(toa, mic_id)
    })

    groups_df <- reactive({
      timeline <- timeline_data()
      if (is.null(timeline)) return(empty_grouped_detections())
      export_grouped_detections(group_membership(), timeline)
    })

    removed_points_df <- reactive({
      timeline <- timeline_data()
      if (is.null(timeline)) return(empty_removed_detections())
      export_removed_detections(removed_membership(), timeline)
    })

    output$session_summary <- renderText({
      sessions <- sessions_data()
      if (is.null(sessions)) return("No data loaded")
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
      validate(need(!is.null(timeline_data()), "Load detection databases to view the timeline."))
      row <- current_session()
      dat <- session_data()
      selection_revision()
      dat$status <- detection_point_status(dat$rec_id, group_membership(), removed_membership())
      dat$selected_color <- ifelse(
        dat$status == "grouped",
        "rgba(144,238,144,0.5)",
        ifelse(dat$status == "removed", "rgba(128,128,128,0.1)", "#d7191c")
      )
      action_selected <- action_selected_rec_ids()
      dat_action_selected <- dat[dat$rec_id %in% action_selected, , drop = FALSE]
      dat_active <- dat[dat$status == "active" & !dat$rec_id %in% action_selected, , drop = FALSE]
      dat_grouped <- dat[dat$status == "grouped" & !dat$rec_id %in% action_selected, , drop = FALSE]
      dat_removed <- dat[dat$status == "removed" & !dat$rec_id %in% action_selected, , drop = FALSE]
      x_range <- isolate(current_x_range())
      if (is.null(x_range)) {
        x_range <- list(row$real_start[[1]], row$real_stop[[1]])
      }

      p <- plotly::plot_ly(source = session$ns("timeline"))
      if (nrow(dat_active) > 0) {
        p <- p |>
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
        p <- p |>
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
        p <- p |>
          plotly::add_markers(
            data = dat_grouped,
            x = ~toa,
            y = ~recorder_lane,
            key = ~rec_id,
            text = ~paste0(rec_id, "<br>", mic_id, "<br>", format(toa, "%H:%M:%S", tz = "UTC")),
            hoverinfo = "text",
            marker = list(size = 12, color = "rgba(144,238,144,0.5)"),
            showlegend = FALSE
          )
      }
      if (nrow(dat_removed) > 0) {
        p <- p |>
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

      p |>
        plotly::layout(
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
            tickvals = seq_along(recorder_levels()),
            ticktext = recorder_levels(),
            range = list(0.5, length(recorder_levels()) + 0.5),
            showgrid = FALSE,
            zeroline = FALSE,
            fixedrange = TRUE
          ),
          margin = list(l = 85, r = 20, t = 55, b = 55)
        ) |>
        plotly::config(
          scrollZoom = TRUE,
          displaylogo = FALSE,
          modeBarButtonsToRemove = c("lasso2d", "autoScale2d")
        ) |>
        plotly::event_register("plotly_relayout") |>
        plotly::event_register("plotly_selected") |>
        plotly::event_register("plotly_click")
    })

    selected_rows <- reactive({
      ids <- action_selected_rec_ids()
      dat <- session_data()
      if (length(ids) == 0) {
        return(dat[FALSE, , drop = FALSE])
      }
      rows <- dat |>
        dplyr::filter(rec_id %in% ids) |>
        dplyr::arrange(toa, mic_id)
      apply_suspect_bearing_state(rows, suspect_bearing_state())
    })

    output$selection_map <- leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldImagery,
          options = leaflet::providerTileOptions(noWrap = TRUE)
        ) |>
        leaflet::setView(lng = 0, lat = 0, zoom = 3)
    })

    outputOptions(output, "selection_map", suspendWhenHidden = FALSE)

    observe({
      mics <- mic_data()
      if (is.null(mics)) return()
      valid_mics <- mics[
        !is.na(mics$lat) & !is.na(mics$lng),
        ,
        drop = FALSE
      ]
      proxy <- leaflet::leafletProxy("selection_map", session = session) |>
        leaflet::clearMarkers()
      if (nrow(valid_mics) == 0) return()

      proxy |>
        leaflet::addCircleMarkers(
          lat = valid_mics$lat,
          lng = valid_mics$lng,
          radius = 6,
          label = paste0("Recorder: ", valid_mics$mic_id),
          color = "red",
          stroke = FALSE,
          fillOpacity = 0.5
        ) |>
        leaflet::fitBounds(
          lng1 = min(valid_mics$lng),
          lat1 = min(valid_mics$lat),
          lng2 = max(valid_mics$lng),
          lat2 = max(valid_mics$lat)
        )
    })

    observe({
      rows <- selected_rows()
      mics <- mic_data()
      req(mics)
      arrows <- tryCatch(
        prepare_bearing_arrows(rows, mics, suspect_bearing_state()),
        error = function(e) e
      )

      proxy <- leaflet::leafletProxy("selection_map", session = session) |>
        leaflet::clearGroup("bearings")
      if (inherits(arrows, "error") || nrow(arrows) == 0) return()

      for (i in seq_len(nrow(arrows))) {
        proxy <- proxy |>
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

    observeEvent(timeline_event_data("plotly_selected"), {
      selected <- timeline_event_data("plotly_selected")
      if (is.null(selected) || is.null(selected$key)) {
        return()
      }
      selected_ids <- unique(as.character(selected$key))
      action_selected_rec_ids(unique(c(action_selected_rec_ids(), selected_ids)))
      selection_revision(selection_revision() + 1L)
    }, ignoreNULL = FALSE)

    observeEvent(timeline_event_data("plotly_click"), {
      click <- timeline_event_data("plotly_click")
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

      timeline <- timeline_data()
      sessions <- sessions_data()
      req(timeline, sessions)
      group_rec_ids <- gm$rec_id[gm$group_ID == group_id]
      rows <- timeline[timeline$rec_id %in% group_rec_ids, , drop = FALSE]
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
        return(NULL)
      }

      img_path <- tryCatch(
        write_current_comparison_spectrogram_png(
          rows,
          selected_wav_root(),
          spectro_cache_dir(),
          width = 900,
          row_height = 230
        ),
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

    session$onSessionEnded(function() {
      cd <- isolate(spectro_cache_dir())
      if (!is.null(cd) && dir.exists(cd)) {
        unlink(cd, recursive = TRUE, force = TRUE)
      }
    })
  })
}
