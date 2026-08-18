#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib bs_theme
#' @noRd
app_ui <- function(request) {
  resource_dir <- system.file("www", package = "vocomatcher")
  if (!nzchar(resource_dir)) {
    stop("Installed app resources could not be found.")
  }
  shiny::addResourcePath("vocomatcher-www", resource_dir)

  tagList(
    tags$head(
      tags$title("vocomatcher"),
      tags$link(
        rel = "shortcut icon",
        type = "image/x-icon",
        href = "vocomatcher-www/favicon.ico"
      )
    ),
    fluidPage(
      theme = bs_theme(version = 5),
      mod_detection_timeline_ui("detection_timeline")
    )
  )
}
