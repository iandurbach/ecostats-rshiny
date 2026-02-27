#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Global reactive values to communicate data between modules.
  r <- reactiveValues(
    micData = NULL,
    recData = NULL,
    selectedRecColumns = character(0), # columns from recordings csv to show in the main table
    recDataAbsFilePath = NULL, # the absolute file path of the selected recordings csv
    recParsedData = NULL,
    frontendData = NULL,
    arrows_state = NULL,
    call_groups = NULL,
    checked_rows = NULL
  )
  mod_file_upload_server("file_upload_1", r)
  mod_date_select_server("date_select_1", r)
  mod_match_calls_server("match_calls_1", r)
  mod_export_calls_server("export_calls_1", r)
  mod_wizard_server("vocostep", 4)
}
