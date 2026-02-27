#' bearings_vis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bearings_vis_ui <- function(id){
  ns <- NS(id)
  tagList(
    #verbatimTextOutput(ns("debug")),
    actionButton(ns("home_map_view"), "Home Map View"),
    leaflet::leafletOutput(ns("map"), width = "100%", height = 400)

  )
}

# Create a function to work with a dplyr pipeline
destpoint_v_dplyr <- function(df, d) {
  #For each recording, mic pair, calculate the arrow head using longitude, latitude, bearing and fixed distance
  res <- purrr::pmap(df, \(lng, lat, bearing, ...) geosphere::destPoint(c(lng,lat), bearing, d))
  tmp_dfs <- purrr::map(res, \(m) data.frame(arrow_lng = m[1,1], arrow_lat = m[1,2], row.names=NULL))
  # Bind all dataframe to one datafram
  arrow_df <- purrr::list_rbind(tmp_dfs)

  golem::print_dev(arrow_df)
  # Mutate the original tibble to include the new columns: arrow_lat, arrow_lng
  df %>%
    mutate(arrow_lat = arrow_df$arrow_lat,
           arrow_lng = arrow_df$arrow_lng)
}

arrow_color <- function(sex, suspect_bearing = FALSE) {
  if (isTRUE(suspect_bearing)) {
    return("grey")
  }
  if (is.null(sex) || is.na(sex)) "red"
  else if (sex == "F") "yellow"
  else if (sex == "M") "cyan"
  else "red"
}


#' bearings_vis Server Functions
#'
#' @import leaflet
#' @import leaflet.extras
#' @import leaflet.extras2
#' @noRd
mod_bearings_vis_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Render the leaflet map
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        addProviderTiles(providers$Esri.WorldImagery,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        setView(lng = 0, lat = 0, zoom = 3)
    })

    ## Prevent Shiny from suspending communication with leaflet if the calls tab is not visible yet.
    outputOptions(output, "map", suspendWhenHidden = FALSE)

    # Plot the microphone coordinates
    observeEvent(r$micData, {
      golem::print_dev("rMicData observer called")
      leafletProxy("map") %>%
        clearMarkers() %>%
        addCircleMarkers(
          lat = r$micData$lat,
          lng = r$micData$lng,
          radius = 6,
          label = paste0("Mic ID: ", r$micData$mic_id),
          color = "red",
          stroke = FALSE, fillOpacity = 0.5
        ) %>%
        fitBounds(lng1 = min(r$micData$lng), lat1 = min(r$micData$lat),
                  lng2 = max(r$micData$lng), lat2 = max(r$micData$lat))
    })

    output$debug <- renderPrint(print(cat("class : ", class(r$checked_rows), "val: ", r$checked_rows)))

    observeEvent(r$checked_rows, {
      if (is.null(r$checked_rows)) {
        r$arrows_state <- NULL
        return()
      }
      req(r$frontendData, r$recParsedData)
      row_ids <- data.frame(row_id = r$checked_rows)

      # Find which mic_ids these rows correspond to
      backend_rows <- get_backend_rows_by_frontend_id(r$frontendData, r$recParsedData, r$checked_rows)

      # Reset suspect_bearing to FALSE for rows not currently checked
      rec_ids_checked <- backend_rows$rec_id
      r$recParsedData <- r$recParsedData %>%
        mutate(suspect_bearing = if_else(rec_id %in% rec_ids_checked, suspect_bearing, FALSE))

      radius = 500 # meters. todo: calculate the optimum radius given mic coordinates
      recordings_bearing_info <- backend_rows %>%
        inner_join(r$micData, by="mic_id") %>%
        select(rec_id, mic_id, bearing, sex, suspect_bearing, lat, lng) %>%
        destpoint_v_dplyr(d=radius)

      r$arrows_state <- cbind(recordings_bearing_info, row_ids)

      #golem::print_dev(r$arrows_state)

    }, ignoreNULL = FALSE)

    observeEvent(r$arrows_state, {
      proxy <- leafletProxy("map")
      proxy %>%
        clearGroup("all")


      # prevents plotting arrows on declaration
      if (!is.null(r$arrows_state)) {
        for (i in 1:dim(r$arrows_state)[1]) { # for each row in arrow state df
          leafletProxy("map") %>%
            addArrowhead(
              lng = c(r$arrows_state[i,"lng"], r$arrows_state[i,"arrow_lng"]),
              lat = c(r$arrows_state[i,"lat"], r$arrows_state[i,"arrow_lat"]),
              group = "all",
              layerId = paste0("arrow_", r$arrows_state[i, "rec_id"]),
              label = paste0("Row ID: ", r$arrows_state[i, "row_id"]),
              # Color the arrow according to sex or suspect status; If no sex saved, resort to red.
              color = arrow_color(r$arrows_state[i,"sex"], r$arrows_state[i,"suspect_bearing"]),
              opacity = 50,
              options = arrowheadOptions(yawn = 40, fill = FALSE)
            )
        }
      }
    }, ignoreNULL = FALSE)

    observeEvent(input$map_shape_click, {
      click <- input$map_shape_click
      req(click$id, r$recParsedData)
      # Expect layerId format "arrow_<rec_id>"
      rec_id <- suppressWarnings(as.numeric(gsub("^arrow_", "", click$id)))
      if (is.na(rec_id)) return()

      updated <- r$recParsedData %>%
        mutate(suspect_bearing = if_else(rec_id == !!rec_id, !suspect_bearing, suspect_bearing))
      r$recParsedData <- updated

      # Refresh arrows for current selection to update colors
      if (!is.null(r$checked_rows) && !is.null(r$frontendData)) {
        backend_rows <- get_backend_rows_by_frontend_id(r$frontendData, r$recParsedData, r$checked_rows)
        radius = 500
        recordings_bearing_info <- backend_rows %>%
          inner_join(r$micData, by="mic_id") %>%
          select(rec_id, mic_id, bearing, sex, suspect_bearing, lat, lng) %>%
          destpoint_v_dplyr(d=radius)
        row_ids <- data.frame(row_id = r$checked_rows)
        r$arrows_state <- cbind(recordings_bearing_info, row_ids)
      }
    })

  })
}

## To be copied in the UI
# mod_bearings_vis_ui("bearings_vis_1")

## To be copied in the server
# mod_bearings_vis_server("bearings_vis_1")
