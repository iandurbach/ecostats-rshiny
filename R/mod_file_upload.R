#' file_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shinyFiles shinyFilesButton
#'
mod_file_upload_ui <- function(id){
  ns <- NS(id)
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
#' @importFrom shinyFiles shinyFileChoose getVolumes parseFilePaths
#' @importFrom fs path_home
mod_file_upload_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    required_cols <- c("recording_ID", "mic_ID", "GPSDatetime2", "measured_bearing", "measured_gender", "spectrogram")

    volumes <- c(Home = path_home(),  getVolumes()())
    shinyFileChoose(input, "microphones", session = session, roots = volumes, filetypes = c("csv"))
    shinyFileChoose(input, "recordings", session = session, roots = volumes, filetypes = c("csv"))

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
        # Reset selected columns each time a new recordings file is loaded
        r$selectedRecColumns <- intersect(required_cols, names(recData))
        # Save the absolute file path of the loaded recordings csv
        r$recDataAbsFilePath <- f$datapath
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
  })
}

## To be copied in the UI
# mod_file_upload_ui("file_upload_1")

## To be copied in the server
# mod_file_upload_server("file_upload_1")
