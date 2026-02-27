#' date_select UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_date_select_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12, align="center", h3("Unmatched/Remaining calls:"))
    ),
    fluidRow(
      column(12, align="center", tableOutput(ns("availableDates")))
    )
  )
}

#' date_select Server Functions
#'
#' @importFrom attempt attempt is_try_error try_catch
#' @noRd
mod_date_select_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(list(r$recData, r$selectedRecColumns), {
      req(r$recData)
      optData <- attempt(parse_rec_data(r$recData, r$selectedRecColumns))
      if (is_try_error(optData)) {
        golem::invoke_js("erroralert", list(title="Parse error!", msg="Failed to parse recording data. Check stacktrace!"))
      } else {
        r$recParsedData <- optData
      }
    })

    num_calls_by_day <- reactive({
      req(r$recParsedData)
      optSelectedCalls <- attempt(calc_remaining_calls_by_day(r$recParsedData))
      if (is_try_error(optSelectedCalls)) {
          stop("Failed to calculate number of calls by day!")
      } else {
          return(optSelectedCalls)
      }
      })
    output$availableDates <- renderTable(num_calls_by_day())
    })
}
## To be copied in the UI
# mod_date_select_ui("date_select_1")

## To be copied in the server
# mod_date_select_server("date_select_1")
