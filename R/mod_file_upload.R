#' file_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shinyFiles shinyFilesButton shinyDirButton
#'
mod_file_upload_ui <- function(id){
  ns <- NS(id)
  is_upload_mode <- isTRUE(getOption("vocomatcher.force_upload_mode")) ||
    nchar(Sys.getenv("SHINY_PORT", "")) > 0 || nchar(Sys.getenv("SHINYAPPS_PACKAGE_NAME", "")) > 0
  tagList(
    if (!is_upload_mode) {
      tagList(
        fluidRow(
          column(6, shinyFilesButton(
            id=ns("microphones"),
            label="Select microphones coordinates CSV",
    #        accept=c("text/csv"),
            multiple = FALSE,
            title="Select CSV file")),
          column(6, shinyFilesButton(
            id=ns("recordings"),
            label="Select recordings CSV",
    #        accept=c("text/csv"),
            multiple = FALSE,
            title="Select CSV file"))
        ),
        fluidRow(
          column(6, shinyFilesButton(
            id=ns("databases"),
            label="Select detection SQLite database(s)",
            multiple = TRUE,
            title="Select SQLite database files")),
          column(6, shinyFiles::shinyDirButton(
            id=ns("wav_root"),
            label="Select WAV root folder",
            title="Select WAV root folder"))
        ),
        fluidRow(
          column(6, shinyFiles::shinyDirButton(
            id=ns("spectro_dir"),
            label="Select folder containing spectrograms",
            title="Select folder")),
          column(12, shinyFilesButton(
            id=ns("spectro_zip"),
            label="Select spectrograms ZIP to extract",
            multiple = FALSE,
            title="Select ZIP file"))
        )
      )
    } else {
      tagList(
        fluidRow(
          column(6, fileInput(
            ns("microphones_upload"),
            "Upload microphones CSV",
            accept = c(".csv")
          )),
          column(6, fileInput(
            ns("recordings_upload"),
            "Upload recordings CSV",
            accept = c(".csv")
          ))
        ),
        fluidRow(
          column(12, fileInput(
            ns("spectro_zip_upload"),
            "Upload spectrograms ZIP (optional)",
            accept = c(".zip")
          ))
        )
      )
    },
    fluidRow(
      column(4, div(
        id="file_upload_div_mics",
        tableOutput(ns("tblMics")),
      )),
      column(2, div()),
      column(6, uiOutput(ns("recordings_preview_with_selectors")))
    )
    )
}

#' file_upload Server Functions
#'
#' @noRd

#' @importFrom attempt attempt is_try_error try_catch
#' @importFrom shinyFiles shinyFileChoose shinyDirChoose getVolumes parseFilePaths
#' @importFrom fs path_home
mod_file_upload_server <- function(id, r = reactiveValues(
  micData = NULL,
  recData = NULL,
  selectedRecColumns = character(0),
  recDataAbsFilePath = NULL,
  spectroBasePath = NULL,
  spectroTempDir = NULL,
  wavRootPath = NULL,
  spectroCacheDir = NULL,
  inputMode = NULL
)){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    is_upload_mode <- isTRUE(getOption("vocomatcher.force_upload_mode")) ||
      nchar(Sys.getenv("SHINY_PORT", "")) > 0 || nchar(Sys.getenv("SHINYAPPS_PACKAGE_NAME", "")) > 0
    required_cols <- c("recording_ID", "mic_ID", "GPSDatetime2", "measured_bearing", "measured_gender", "spectrogram")

    volumes <- c(Home = path_home(),  getVolumes()())
    if (!is_upload_mode) {
      shinyFileChoose(input, "microphones", session = session, roots = volumes, filetypes = c("csv"))
      shinyFileChoose(input, "recordings", session = session, roots = volumes, filetypes = c("csv"))
      shinyFileChoose(input, "databases", session = session, roots = volumes, filetypes = c("sqlite3"))
      shinyFileChoose(input, "spectro_zip", session = session, roots = volumes, filetypes = c("zip"))
      shinyFiles::shinyDirChoose(input, "spectro_dir", session = session, roots = volumes, allowDirCreate = FALSE)
      shinyFiles::shinyDirChoose(input, "wav_root", session = session, roots = volumes, allowDirCreate = FALSE)
    }

    observeEvent(input$microphones, {
      req(!is.integer(input$microphones))
      f <- parseFilePaths(volumes, input$microphones)
      micData <- attempt(read_csv_vroom(f$datapath))
        if(is_try_error(micData)) {
          golem::invoke_js("erroralert", list(title="Microphones CSV read error!", msg=micData))
        } else {
          output$tblMics <- renderTable(head(micData))
          # Add micData to global reactiveValues
          r$micData <- micData
        }
    })

    observeEvent(input$microphones_upload, {
      req(is_upload_mode, input$microphones_upload)
      micData <- attempt(read_csv_vroom(input$microphones_upload$datapath))
      if(is_try_error(micData)) {
        golem::invoke_js("erroralert", list(title="Microphones CSV read error!", msg=micData))
      } else {
        output$tblMics <- renderTable(head(micData))
        r$micData <- micData
      }
    })

    observeEvent(input$recordings, {
      req(!is.integer(input$recordings))
      f <- parseFilePaths(volumes, input$recordings)
      recData <- attempt(read_csv_vroom(f$datapath))
      if(is_try_error(recData)) {
        golem::invoke_js("erroralert", list(title="Recordings CSV read error!", msg=recData))
      } else {
        output$tblRecs <- renderTable(head(recData))
        # Insert the recordings data to the reactive values
          r$recData <- recData
          r$inputMode <- "csv"
          # Reset selected columns each time a new recordings file is loaded
          r$selectedRecColumns <- intersect(required_cols, names(recData))
        # Save the absolute file path of the loaded recordings csv
        r$recDataAbsFilePath <- f$datapath
      }
    })

    observeEvent(input$recordings_upload, {
      req(is_upload_mode, input$recordings_upload)
      recData <- attempt(read_csv_vroom(input$recordings_upload$datapath))
      if(is_try_error(recData)) {
        golem::invoke_js("erroralert", list(title="Recordings CSV read error!", msg=recData))
      } else {
        output$tblRecs <- renderTable(head(recData))
        r$recData <- recData
        r$inputMode <- "csv"
        r$selectedRecColumns <- intersect(required_cols, names(recData))
        # when uploading, use temp path as base for spectros by default
        r$recDataAbsFilePath <- input$recordings_upload$datapath
      }
    })

    observeEvent(input$databases, {
      req(!is.integer(input$databases))
      f <- parseFilePaths(volumes, input$databases)
      db_paths <- f$datapath
      dbData <- attempt(read_detection_databases(db_paths, offset_sign = 1))
      if(is_try_error(dbData)) {
        golem::invoke_js("erroralert", list(title="Database read error!", msg=dbData))
      } else {
        r$micData <- dbData$micData
        r$recData <- dbData$recData
        r$inputMode <- "database"
        r$selectedRecColumns <- intersect(
          c("cluster_id", "clock_offset", "raw_toa", "Duration", "wav_file"),
          names(dbData$recData)
        )
        if (is.null(r$spectroCacheDir) || !dir.exists(r$spectroCacheDir)) {
          r$spectroCacheDir <- file.path(tempdir(), paste0("spectro_cache_", session$token))
          dir.create(r$spectroCacheDir, recursive = TRUE, showWarnings = FALSE)
        }
        output$tblMics <- renderTable(head(dbData$micData))
        output$tblRecs <- renderTable(head(dbData$recData))
        showNotification(paste("Loaded", nrow(dbData$recData), "detections from", length(db_paths), "database(s)."), type = "message")
      }
    })

    observeEvent(input$wav_root, {
      req(!is.null(input$wav_root))
      if (is.integer(input$wav_root)) return()
      dir_path <- shinyFiles::parseDirPath(volumes, input$wav_root)
      if (length(dir_path) == 0 || is.na(dir_path)) {
        golem::invoke_js("erroralert", list(title="WAV folder error!", msg="Could not read folder path."))
      } else {
        r$wavRootPath <- dir_path
        showNotification(paste("Using WAV files from", dir_path), type = "message")
      }
    })

    observeEvent(input$spectro_zip, {
      req(!is.integer(input$spectro_zip))
      f <- parseFilePaths(volumes, input$spectro_zip)
      zip_path <- f$datapath
      dest_dir <- file.path(tempdir(), paste0("spectros_", session$token))
      if (!is.null(r$spectroTempDir) && dir.exists(r$spectroTempDir)) {
        unlink(r$spectroTempDir, recursive = TRUE, force = TRUE)
      }
      dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
      unzip_res <- attempt(unzip(zip_path, exdir = dest_dir))
      if (is_try_error(unzip_res)) {
        golem::invoke_js("erroralert", list(title="Spectrogram unzip error!", msg=unzip_res))
      } else {
        r$spectroBasePath <- dest_dir
        r$spectroTempDir <- dest_dir
        showNotification(paste("Extracted spectrograms to", dest_dir), type = "message")
      }
    })

    observeEvent(input$spectro_zip_upload, {
      req(is_upload_mode, input$spectro_zip_upload)
      zip_path <- input$spectro_zip_upload$datapath
      dest_dir <- file.path(tempdir(), paste0("spectros_", session$token))
      if (!is.null(r$spectroTempDir) && dir.exists(r$spectroTempDir)) {
        unlink(r$spectroTempDir, recursive = TRUE, force = TRUE)
      }
      dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
      unzip_res <- attempt(unzip(zip_path, exdir = dest_dir))
      if (is_try_error(unzip_res)) {
        golem::invoke_js("erroralert", list(title="Spectrogram unzip error!", msg=unzip_res))
      } else {
        r$spectroBasePath <- dest_dir
        r$spectroTempDir <- dest_dir
        showNotification(paste("Extracted spectrograms to", dest_dir), type = "message")
      }
    })

    observeEvent(input$spectro_dir, {
      req(!is.null(input$spectro_dir))
      if (is.integer(input$spectro_dir)) return()
      dir_path <- shinyFiles::parseDirPath(volumes, input$spectro_dir)
      if (length(dir_path) == 0 || is.na(dir_path)) {
        golem::invoke_js("erroralert", list(title="Spectrogram folder error!", msg="Could not read folder path."))
      } else {
        # Clear any temp dir tracking if user chooses a folder
        r$spectroBasePath <- dir_path
        r$spectroTempDir <- NULL
        showNotification(paste("Using spectrograms from", dir_path), type = "message")
      }
    })

    output$recordings_preview_with_selectors <- renderUI({
      req(r$recData)
      cols <- names(r$recData)
      safe_ids <- make.names(cols, unique = TRUE)

      sample_rows <- lapply(seq_along(cols), function(i) {
        col_name <- cols[[i]]
        is_required <- col_name %in% required_cols
        sample_vals <- r$recData[[col_name]][seq_len(min(3, nrow(r$recData)))]
        chk <- checkboxInput(
          ns(paste0("col_", safe_ids[[i]])),
          label = NULL,
          value = is_required
        )
        tags$tr(
          tags$td(chk),
          tags$td(tags$strong(col_name)),
          tags$td(paste(sample_vals, collapse = ", "))
        )
      })

      tagList(
        div(id = "file_upload_div_recs",
            tags$label("Select columns to show in the unmatched calls table:"),
            tags$table(
              class = "table table-bordered table-sm recording-column-selector-table",
              tags$thead(
                tags$tr(
                  tags$th("Include"),
                  tags$th("Column"),
                  tags$th("Sample values")
                )
              ),
              tags$tbody(!!!sample_rows)
            ),
            tableOutput(ns("tblRecs"))
        )
      )
    })

    observe({
      req(r$recData)
      cols <- names(r$recData)
      safe_ids <- make.names(cols, unique = TRUE)
      selected <- cols[vapply(seq_along(cols), function(i) isTRUE(input[[paste0("col_", safe_ids[[i]])]]), logical(1))]
      r$selectedRecColumns <- selected
    })

    session$onSessionEnded(function() {
      td <- isolate(r$spectroTempDir)
      if (!is.null(td) && dir.exists(td)) {
        unlink(td, recursive = TRUE, force = TRUE)
      }
      cd <- isolate(r$spectroCacheDir)
      if (!is.null(cd) && dir.exists(cd)) {
        unlink(cd, recursive = TRUE, force = TRUE)
      }
    })
  })
}

## To be copied in the UI
# mod_file_upload_ui("file_upload_1")

## To be copied in the server
# mod_file_upload_server("file_upload_1")
