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
        height: 66px;
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
        height: calc(100% - 72px);
        min-height: 0;
      }
      .header-navigation {
        display: grid;
        gap: 4px;
      }
      .header-navigation-row {
        display: flex;
        gap: 4px;
      }
      .header-navigation-row .btn {
        padding: 3px 8px;
        font-size: 12px;
        line-height: 1.35;
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
        display: flex;
        align-items: center;
        gap: 6px;
        flex-wrap: wrap;
        min-height: 0;
      }
      .notes-compact,
      .notes-compact .form-group {
        width: 90px;
        flex: 0 0 90px;
        margin-bottom: 0;
      }
      .notes-compact textarea {
        width: 100%;
        height: 30px;
        min-height: 30px;
        padding: 3px 7px;
        font-size: 12px;
        resize: none;
      }
      .actions-spacer {
        width: 10px;
        flex: 0 0 10px;
      }
      .button-row {
        display: flex;
        align-items: center;
        gap: 4px;
        flex-wrap: wrap;
      }
      .button-row .btn {
        padding: 3px 8px;
        font-size: 12px;
        line-height: 1.35;
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
              shinyFilesButton(ns("existing_groups"), "Load existing groups", "Select saved RData grouping file", multiple = FALSE),
              actionButton(ns("load_data"), "Load data"),
              div(class = "sidebar-status", textOutput(ns("input_status")))
            ),
            div(
              class = "sidebar-section",
              div(class = "sidebar-label", "Spectrogram settings"),
              shiny::selectInput(
                ns("spectro_detail"),
                "Detail",
                choices = c(
                  "Time detail" = "time",
                  "Balanced" = "balanced",
                  "Frequency detail" = "frequency"
                ),
                selected = "balanced"
              ),
              shiny::sliderInput(
                ns("spectro_frequency_range"),
                "Frequency range (Hz)",
                min = 100,
                max = 8000,
                value = c(100, 8000),
                step = 100,
                sep = ""
              ),
              shiny::numericInput(
                ns("spectro_buffer_seconds"),
                "Buffer (seconds)",
                value = 0,
                min = 0,
                step = 0.1
              ),
              shiny::selectInput(
                ns("spectro_image_quality"),
                "Image quality",
                choices = c(
                  "Standard" = "standard",
                  "High" = "high",
                  "Very high" = "very_high"
                ),
                selected = "standard"
              )
            ),
            div(
              class = "sidebar-section",
              div(class = "sidebar-label", "Export"),
              downloadButton(ns("export_groups"), "Export RData"),
              downloadButton(ns("export_acre_inputs"), "Export acre inputs"),
              downloadButton(ns("export_acre_script"), "Export acre script"),
              div(class = "sidebar-status", textOutput(ns("export_status")))
            )
          )
        ),
        div(
          class = "workspace",
          div(
            class = "app-header",
            div(class = "app-title", "vocomatcher"),
            div(
              class = "header-navigation",
              div(
                class = "header-navigation-row",
                actionButton(ns("prev_cluster"), "Previous cluster", class = "btn-sm"),
                actionButton(ns("next_cluster"), "Next cluster", class = "btn-sm")
              ),
              div(
                class = "header-navigation-row",
                actionButton(ns("prev_session"), "Previous session", class = "btn-sm"),
                actionButton(ns("next_session"), "Next session", class = "btn-sm")
              )
            ),
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
                    div(
                      class = "button-row",
                      div(
                        class = "notes-compact",
                        textAreaInput(
                          ns("action_notes"),
                          label = NULL,
                          value = "",
                          rows = 1,
                          placeholder = "Notes"
                        )
                      ),
                      actionButton(ns("group_selected"), "Group", class = "btn-sm"),
                      actionButton(ns("ungroup_selected"), "Ungroup", class = "btn-sm"),
                      actionButton(ns("remove_selected"), "Remove", class = "btn-sm"),
                      actionButton(ns("clear_selection"), "Clear selection", class = "btn-sm"),
                      div(class = "actions-spacer"),
                      actionButton(ns("prev_group"), "Previous group", class = "btn-sm"),
                      actionButton(ns("next_group"), "Next group", class = "btn-sm"),
                      div(class = "actions-spacer"),
                      actionButton(ns("auto_group"), "Auto group", class = "btn-sm"),
                      actionButton(ns("auto_group_all"), "Auto group all", class = "btn-sm"),
                      div(class = "actions-spacer"),
                      actionButton(ns("clear_session_groups"), "Clear all groups", class = "btn-sm")
                    ),
                    div(
                      class = "status-row",
                      tags$span(textOutput(ns("action_summary"), inline = TRUE)),
                      tags$span(" | "),
                      tags$span(textOutput(ns("group_review_summary"), inline = TRUE))
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
    shinyFileChoose(input, "existing_groups", session = session, roots = volumes, filetypes = c("RData", "rda"))
    shinyFiles::shinyDirChoose(input, "wav_root", session = session, roots = volumes, allowDirCreate = FALSE)

    selected_db_paths <- reactiveVal(character(0))
    selected_wav_root <- reactiveVal(NULL)
    mic_data <- reactiveVal(NULL)
    timeline_data <- reactiveVal(NULL)
    sessions_data <- reactiveVal(NULL)
    cluster_ids_data <- reactiveVal(character(0))
    current_cluster_index <- reactiveVal(1L)
    current_session_index <- reactiveVal(1L)
    spectro_zoom <- reactiveVal(1)
    group_membership <- reactiveVal(empty_group_membership())
    removed_membership <- reactiveVal(empty_removed_membership())
    suspect_bearing_state <- reactiveVal(empty_suspect_bearing_state())
    next_group_id <- reactiveVal(1L)
    current_review_group_id <- reactiveVal(NULL)
    action_selected_rec_ids <- reactiveVal(character(0))
    current_x_range <- reactiveVal(NULL)
    auto_group_cursor <- reactiveVal(NULL)
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

    observeEvent(input$existing_groups, {
      req(!is.integer(input$existing_groups))
      timeline <- timeline_data()
      if (is.null(timeline)) {
        showNotification("Load detection data before loading existing groups.", type = "warning")
        return()
      }

      f <- shinyFiles::parseFilePaths(volumes, input$existing_groups)
      if (nrow(f) == 0 || is.na(f$datapath[[1]])) {
        showNotification("Could not read grouping file path.", type = "error")
        return()
      }

      loaded <- tryCatch({
        env <- new.env(parent = emptyenv())
        load(normalizePath(f$datapath[[1]], mustWork = TRUE), envir = env)
        if (!exists("groups", envir = env, inherits = FALSE)) {
          stop("Grouping file is missing object `groups`.")
        }
        groups <- get("groups", envir = env, inherits = FALSE)
        removed_points <- if (exists("removed_points", envir = env, inherits = FALSE)) {
          get("removed_points", envir = env, inherits = FALSE)
        } else {
          empty_removed_detections()
        }
        gm <- import_group_membership(groups, timeline)
        rm <- import_removed_membership(removed_points, timeline)
        if (any(gm$rec_id %in% rm$rec_id)) {
          stop("Loaded file marks at least one detection as both grouped and removed.")
        }
        list(group_membership = gm, removed_membership = rm)
      }, error = function(e) e)

      if (inherits(loaded, "error")) {
        showNotification(conditionMessage(loaded), type = "error", duration = 10)
        return()
      }

      group_membership(loaded$group_membership)
      removed_membership(loaded$removed_membership)
      suspect_bearing_state(dplyr::bind_rows(
        loaded$group_membership[, c("rec_id", "suspect_bearing"), drop = FALSE],
        loaded$removed_membership[, c("rec_id", "suspect_bearing"), drop = FALSE]
      ))
      next_id <- if (nrow(loaded$group_membership) == 0) 1L else max(loaded$group_membership$group_ID, na.rm = TRUE) + 1L
      next_group_id(as.integer(next_id))
      current_review_group_id(NULL)
      action_selected_rec_ids(character(0))
      current_x_range(NULL)
      auto_group_cursor(NULL)
      selection_revision(selection_revision() + 1L)
      showNotification(
        paste(
          "Loaded",
          length(unique(loaded$group_membership$group_ID)),
          "group(s) and",
          nrow(loaded$removed_membership),
          "removed detection(s)."
        ),
        type = "message"
      )
    })

    reset_workspace_state <- function() {
      group_membership(empty_group_membership())
      removed_membership(empty_removed_membership())
      suspect_bearing_state(empty_suspect_bearing_state())
      next_group_id(1L)
      current_review_group_id(NULL)
      action_selected_rec_ids(character(0))
      current_x_range(NULL)
      auto_group_cursor(NULL)
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
        session_counts <- timeline |>
          dplyr::filter(!is.na(session_id)) |>
          dplyr::count(session_id, name = "n_detections")
        sessions <- sessions |>
          dplyr::left_join(session_counts, by = "session_id") |>
          dplyr::mutate(n_detections = ifelse(is.na(n_detections), 0L, n_detections))
        if (nrow(sessions) == 0) {
          stop("No recording sessions were found in Sound_Acquisition.")
        }
        cluster_ids <- ordered_cluster_ids(sessions)
        if (length(cluster_ids) == 0) {
          stop("No detector clusters were found in the selected databases.")
        }
        list(
          mic_data = db_data$micData,
          timeline = timeline,
          sessions = sessions,
          cluster_ids = cluster_ids
        )
      }, error = function(e) e)

      if (inherits(loaded, "error")) {
        showNotification(conditionMessage(loaded), type = "error", duration = 10)
        return()
      }

      dir.create(spectro_cache_dir(), recursive = TRUE, showWarnings = FALSE)
      mic_data(loaded$mic_data)
      timeline_data(loaded$timeline)
      sessions_data(loaded$sessions)
      cluster_ids_data(loaded$cluster_ids)
      current_cluster_index(1L)
      initial_sessions <- cluster_sessions(loaded$sessions, loaded$cluster_ids[[1]])
      current_session_index(first_session_with_detections(initial_sessions))
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

    reset_navigation_view <- function() {
      current_review_group_id(NULL)
      action_selected_rec_ids(character(0))
      current_x_range(NULL)
      auto_group_cursor(NULL)
    }

    current_cluster_id <- reactive({
      ids <- cluster_ids_data()
      req(length(ids) > 0)
      ids[[current_cluster_index()]]
    })

    current_cluster_sessions <- reactive({
      sessions <- sessions_data()
      req(sessions)
      cluster_sessions(sessions, current_cluster_id())
    })

    recorder_levels <- reactive({
      timeline <- timeline_data()
      if (is.null(timeline) || length(cluster_ids_data()) == 0) return(character(0))
      sort(unique(as.character(
        timeline$mic_id[as.character(timeline$cluster_id) == current_cluster_id()]
      )))
    })

    current_cluster_mics <- reactive({
      mics <- mic_data()
      if (is.null(mics)) return(NULL)
      mics_for_cluster(mics, current_cluster_id())
    })

    change_cluster <- function(direction) {
      ids <- cluster_ids_data()
      req(length(ids) > 0)
      next_index <- bounded_navigation_index(current_cluster_index(), direction, length(ids))
      if (next_index == current_cluster_index()) return()
      current_cluster_index(next_index)
      sessions <- cluster_sessions(sessions_data(), ids[[next_index]])
      current_session_index(first_session_with_detections(sessions))
      reset_navigation_view()
    }

    observeEvent(input$prev_cluster, {
      change_cluster(-1L)
    })

    observeEvent(input$next_cluster, {
      change_cluster(1L)
    })

    observeEvent(input$prev_session, {
      sessions <- current_cluster_sessions()
      req(sessions)
      current_session_index(bounded_navigation_index(current_session_index(), -1L, nrow(sessions)))
      reset_navigation_view()
    })

    observeEvent(input$next_session, {
      sessions <- current_cluster_sessions()
      req(sessions)
      current_session_index(bounded_navigation_index(current_session_index(), 1L, nrow(sessions)))
      reset_navigation_view()
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
        new_range <- list(relayout[["xaxis.range[0]"]], relayout[["xaxis.range[1]"]])
        current_x_range(new_range)
        if (!is.null(sessions_data())) {
          auto_group_cursor(timeline_range_start(new_range, current_session()))
        }
      } else if (isTRUE(relayout[["xaxis.autorange"]])) {
        current_x_range(NULL)
        auto_group_cursor(NULL)
      }
    }, ignoreNULL = TRUE)

    current_session <- reactive({
      sessions <- current_cluster_sessions()
      req(sessions)
      sessions[current_session_index(), , drop = FALSE]
    })

    session_data <- reactive({
      timeline <- timeline_data()
      req(timeline)
      timeline |>
        dplyr::filter(
          cluster_id == current_cluster_id(),
          session_id == current_session()$session_id[[1]]
        ) |>
        dplyr::mutate(recorder_lane = match(mic_id, recorder_levels())) |>
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
      cluster_session_rows <- current_cluster_sessions()
      paste0(
        cluster_display_label(current_cluster_id()),
        " | Session ",
        current_session_index(),
        " of ",
        nrow(cluster_session_rows),
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
      dat <- add_session_group_display(dat, group_membership())
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
            text = ~paste0(
              rec_id,
              "<br>",
              mic_id,
              "<br>",
              format(toa, "%H:%M:%S", tz = "UTC"),
              ifelse(nzchar(group_label), paste0("<br>", group_label), "")
            ),
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
            text = ~paste0(rec_id, "<br>", mic_id, "<br>", format(toa, "%H:%M:%S", tz = "UTC"), "<br>", group_label),
            hoverinfo = "text",
            marker = list(size = 12, color = "rgba(144,238,144,0.2)"),
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
      mics <- current_cluster_mics()
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
      mics <- current_cluster_mics()
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
      grouping_method <- if (length(existing_groups) == 1) {
        unique(gm$grouping_method[gm$group_ID == group_id])[[1]]
      } else {
        "manual"
      }
      ids_to_add <- setdiff(rows$rec_id, gm$rec_id[gm$group_ID == group_id])
      new_rows <- format_group_membership_rows(
        rows[rows$rec_id %in% ids_to_add, , drop = FALSE],
        notes = input$action_notes,
        group_id = group_id,
        grouping_method = grouping_method
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
      dat <- session_data()
      group_id <- next_session_group_id(current_review_group_id(), direction, dat, gm)
      if (length(group_id) == 0) {
        showNotification("No groups in the current session.", type = "warning")
        return()
      }

      group_rec_ids <- gm$rec_id[gm$group_ID == group_id]
      rows <- dat[dat$rec_id %in% group_rec_ids, , drop = FALSE]
      rows <- rows[order(rows$toa, rows$mic_id), , drop = FALSE]
      if (nrow(rows) == 0) {
        showNotification(paste("Group", group_id, "has no matching detections."), type = "warning")
        return()
      }

      current_x_range(group_review_range(rows, current_session()))
      action_selected_rec_ids(rows$rec_id)
      current_review_group_id(group_id)
      selection_revision(selection_revision() + 1L)
    }

    observeEvent(input$prev_group, {
      review_group(-1L)
    })

    observeEvent(input$next_group, {
      review_group(1L)
    })

    run_auto_group <- function(all_groups = FALSE) {
      dat <- apply_suspect_bearing_state(session_data(), suspect_bearing_state())
      start_time <- if (all_groups) {
        NULL
      } else {
        auto_group_cursor() %||% timeline_range_start(current_x_range(), current_session())
      }
      proposals <- tryCatch(
        propose_session_groups(
          session_rows = dat,
          mics = current_cluster_mics(),
          group_membership = group_membership(),
          removed_membership = removed_membership(),
          start_time = start_time
        ),
        error = function(e) e
      )
      if (inherits(proposals, "error")) {
        showNotification(
          paste("Automatic grouping failed:", conditionMessage(proposals)),
          type = "error",
          duration = 10
        )
        return()
      }
      if (nrow(proposals) == 0) {
        showNotification(
          if (all_groups) {
            "No eligible groups were found in the current session."
          } else {
            "No eligible group was found at or after the current timeline start."
          },
          type = "warning"
        )
        return()
      }

      if (!all_groups) {
        proposals <- proposals[proposals$group_ID == min(proposals$group_ID), , drop = FALSE]
      }
      proposals <- remap_proposed_group_ids(proposals, next_group_id())
      added_group_ids <- sort(unique(proposals$group_ID))
      group_membership(dplyr::bind_rows(group_membership(), proposals))
      next_group_id(max(added_group_ids) + 1L)
      selection_revision(selection_revision() + 1L)

      if (all_groups) {
        showNotification(
          paste("Created", length(added_group_ids), "automatic group(s) in the current session."),
          type = "message"
        )
        return()
      }

      group_id <- added_group_ids[[1]]
      rows <- dat[dat$rec_id %in% proposals$rec_id, , drop = FALSE]
      rows <- rows[order(rows$toa, rows$mic_id), , drop = FALSE]
      auto_group_cursor(min(rows$toa))
      current_x_range(automatic_group_review_range(rows, current_session()))
      action_selected_rec_ids(rows$rec_id)
      current_review_group_id(group_id)
      showNotification(paste("Created automatic group", group_id), type = "message")
    }

    observeEvent(input$auto_group, {
      run_auto_group(FALSE)
    })

    observeEvent(input$auto_group_all, {
      run_auto_group(TRUE)
    })

    observeEvent(input$clear_session_groups, {
      group_ids <- session_group_ids(session_data(), group_membership())
      if (length(group_ids) == 0) {
        showNotification("There are no groups to clear in the current session.", type = "warning")
        return()
      }
      showModal(modalDialog(
        "This will clear all groups this session. Are you sure?",
        title = "Clear all groups",
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("confirm_clear_session_groups"), "Clear all groups", class = "btn-danger")
        )
      ))
    })

    observeEvent(input$confirm_clear_session_groups, {
      dat <- session_data()
      group_ids <- session_group_ids(dat, group_membership())
      group_membership(clear_session_group_membership(group_membership(), dat))
      current_review_group_id(NULL)
      action_selected_rec_ids(character(0))
      auto_group_cursor(NULL)
      selection_revision(selection_revision() + 1L)
      removeModal()
      showNotification(
        paste("Cleared", length(group_ids), "group(s) from the current session."),
        type = "message"
      )
    })

    observeEvent(input$clear_selection, {
      action_selected_rec_ids(character(0))
      selection_revision(selection_revision() + 1L)
    })

    output$group_review_summary <- renderText({
      group_id <- current_review_group_id()
      group_ids <- session_group_ids(session_data(), group_membership())
      group_count <- length(group_ids)
      if (is.null(group_id) || group_count == 0) {
        return(paste0(group_count, " group(s) in session"))
      }
      paste0("Reviewing group ", group_id, " | ", group_count, " group(s) in session")
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

    acre_bundle <- reactive({
      timeline <- timeline_data()
      mics <- mic_data()
      sessions <- sessions_data()
      req(timeline, mics, sessions)
      build_acre_export_bundle(
        timeline_data = apply_suspect_bearing_state(timeline, suspect_bearing_state()),
        mic_data = mics,
        sessions = sessions,
        group_membership = group_membership(),
        removed_membership = removed_membership(),
        source_db_paths = selected_db_paths()
      )
    })

    output$export_acre_inputs <- downloadHandler(
      filename = function() {
        paste0("acre_inputs_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".RData")
      },
      content = function(file) {
        save_acre_export_bundle(acre_bundle(), file)
      }
    )

    output$export_acre_script <- downloadHandler(
      filename = function() {
        paste0("fit_acre_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".R")
      },
      content = function(file) {
        writeLines(acre_script_text(), file, useBytes = TRUE)
      }
    )

    output$selected_spectrograms <- renderUI({
      rows <- selected_rows()
      if (nrow(rows) == 0) {
        return(NULL)
      }

      img_path <- tryCatch(
        {
          frequency_range <- input$spectro_frequency_range
          if (is.null(frequency_range) || length(frequency_range) != 2) {
            frequency_range <- c(100, 8000)
          }
          image_scale <- spectrogram_image_scale(input$spectro_image_quality)
          comparison <- prepare_comparison_spectrograms(
            rows,
            selected_wav_root(),
            window_size = spectrogram_window_size(input$spectro_detail),
            overlap = 0.75,
            freq_min_hz = frequency_range[[1]],
            freq_max_hz = frequency_range[[2]],
            buffer_seconds = input$spectro_buffer_seconds
          )
          out_path <- file.path(spectro_cache_dir(), "current_comparison.png")
          write_comparison_spectrogram_png(
            comparison,
            out_path,
            width = 900 * image_scale,
            row_height = 230 * image_scale
          )
          out_path
        },
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
