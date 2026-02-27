#' export_calls UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_export_calls_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6, textInput(ns("session_name"), "Please enter a session name: ", "demo"), align="center"),
#      column(6, actionButton(ns("generate"), "Generate capture history"), align="center")
      ),
    fluidRow(
      column(12, hr()),
      column(12, tableOutput(ns("testcapt")), align="center")
    ),
    fluidRow(column(6,
                    downloadButton(
                      ns("download_csv"),
                      label = "Export CSV",
                      class = "btn btn-primary",
                      icon = shiny::icon("download")
                    ),
                    align = "center"
             ),
             column(6,
                    downloadButton(
                      ns("download_txt"),
                      label = "Export TXT",
                      class = "btn btn-info",
                      icon = shiny::icon("download")
                    ),
                    align = "center"
             ),

    )
  )
}

#' export_calls Server Functions
#'
#' @importFrom dplyr mutate
#' @noRd
mod_export_calls_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    capture_history <- reactive({
      req(r$call_groups, (length(r$call_groups) > 0))
      out <- create_capture_history(r$call_groups, r$selectedRecColumns)
      golem::print_dev(out)
      return(out)
    })

    sessioned_capture_history <- reactive({
      req(input$session_name)
      if (grepl("\\s", input$session_name)) { # Show error if session name contains whitespace
        stop("Session name must not contain spaces!")
      }
      capture_history() %>%
        mutate(Session = input$session_name)
      })

    output$testcapt <- renderTable({sessioned_capture_history() %>% mutate(toa = as.character(lubridate::as_datetime(toa, tz="UTC")))}, digits=5)

    output$download_csv <- downloadHandler(
       filename = function() {
         # Use the selected dataset as the suggested file name
         paste0("demo", ".csv")
       },
       content = function(file) {
         # Write the dataset to the `file` that will be downloaded
         write.csv(sessioned_capture_history(), file, row.names = FALSE)
       }
     )

    output$download_txt <- downloadHandler(
      filename = function() {
        # Use the selected dataset as the suggested file name
        paste0("demo", ".txt")
      },
      content = function(file) {
        # Write the capture history to a txt file
        # According to secr/ascr documentation, the first line of the text file is ignored.
        # Therefore, it doesn't matter if we don't prepend the column names by '#'
        write.table(sessioned_capture_history(), file, quote = FALSE, row.names = FALSE)
      }
    )
  })
}

## To be copied in the UI
# mod_export_calls_ui("export_calls_1")

## To be copied in the server
# mod_export_calls_server("export_calls_1")
