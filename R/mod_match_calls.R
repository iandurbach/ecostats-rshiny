#' match_calls UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @importFrom shiny NS tagList
#' @importFrom DT DTOutput
#' @importFrom lubridate ymd_hms
#' @noRd
mod_match_calls_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12, verbatimTextOutput(ns("debug"))), # DEBUG
      column(12, align="center",
             div(id=ns("datetime_slider_container"),
                 style="visibility: hidden;",
                 sliderInput(
                   ns("datetime_slider"),
                   "Filter time: ",
                   min = ymd_hms("2024-07-09 08:54:36", tz="UTC"),
                   max = ymd_hms("2024-07-09 23:59:20", tz="UTC"),
                   value = c(ymd_hms("2024-07-09 08:54:36", tz="UTC"), ymd_hms("2024-07-09 23:59:20", tz="UTC")),
                   timeFormat = "%H:%M:%S",
                   timezone = "+0000" # we assume that all times are UTC
                 )
             )
      )
    ),
    fluidRow(
      #actionButton(ns("show_spectrograms2"), "Show spectrograms", class = "btn btn-info")
      column(4, align="center", tags$button("Show spectrograms" ,id="show_spectrograms", class="btn btn-info")),
      column(4, align="center", actionButton(ns("create_group2"), "Create new call group", class = "btn btn-primary")),
      column(4, align="center", div(class="btn-group",
                                    actionButton(ns("add_to_group"), "Add", class = "btn btn-secondary btn-spacing"),
                                    selectInput(ns("existing_group"), "Add calls to existing group:", choices = NULL)
      )),
    ),
    fluidRow(
      column(6,
             # First Column: DataTable Output
             div(
               id = "calls_container",
               tags$label(`for`="calls_container", "Unmatched calls"),
               DTOutput(ns("unmatched_calls"))
             )
      ),
      column(1, div()),
      # Third Column: leaflet map and matched calls
      column(5,
             fluidRow(mod_bearings_vis_ui(ns("bearings_vis_1"))),
             fluidRow(hr()),
             fluidRow(tags$h3("Grouped Calls: ", style="text-align: center;")),
             fluidRow(uiOutput(ns("matched_calls")))
      )
    )
  )
}

#' match_calls Server Functions
#' @import dplyr
#' @importFrom DT renderDT datatable JS
#' @importFrom lubridate format_ISO8601
#' @importFrom purrr map
#' @importFrom fs path path_dir path_real
#' @importFrom attempt attempt is_try_error try_catch
#' @noRd
mod_match_calls_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    group_display_cols <- reactive({
      req(r$recParsedData)
      required_raw <- c("recording_ID", "mic_ID", "GPSDatetime2", "measured_bearing", "measured_gender", "spectrogram")
      selected <- intersect(unique(r$selectedRecColumns), names(r$recParsedData))
      cols <- unique(c(required_raw, selected, "suspect_bearing"))
      intersect(cols, names(r$recParsedData))
    })

    # This is the main view of the unmatched calls
    frontendData <- reactive({
      req(r$recParsedData)
      base_cols <- c("rec_id", "tick", "mic_id", "toa", "timediff", "sex", "spectrogram")
      required_raw <- c("recording_ID", "mic_ID", "GPSDatetime2", "measured_bearing", "measured_gender", "bearing", "spectrogram")
      extra_cols <- intersect(unique(r$selectedRecColumns), names(r$recParsedData))
      extra_cols <- setdiff(extra_cols, c(base_cols, required_raw))
      r$recParsedData %>%
        mutate(
          toa = format_ISO8601(toa),
          timediff = "-",
          tick = checkboxColumn(nrow(r$recParsedData)),
          spectrogram = spectroImageColumn(nrow(r$recParsedData))
          ) %>%
        select(rec_id, tick, mic_id, toa, timediff, all_of(extra_cols), sex, spectrogram)
      # we need to null-check because renderDT isolates the reactive frontend data on init and the observer
      # waits for changes to update the table through the datatable proxy.
      # Therefore, we cannot req(input$) because the table won't render at all on init.
    })


    # Update the reactive checked rows and frontend data
    observeEvent(input$checked_rows, r$checked_rows <- input$checked_rows, ignoreNULL = FALSE)
    observe({r$frontendData <- frontendData()})

    # Call the bearings visualization module server and pass-in all reactive values
    mod_bearings_vis_server("bearings_vis_1", r)

    output$unmatched_calls <- renderDT({
      datatable(
        isolate(frontendData()),
        # Specify datatable options
        options = list(
          paging = TRUE,    ## paginate the output
          pageLength = 15,  ## number of rows to output for each page
          scrollX = TRUE,   ## enable scrolling on X axis
          scrollY = TRUE,   ## enable scrolling on Y axis
          autoWidth = TRUE, ## use smart column width handling
          scrollY =  20,
          scrollCollapse = TRUE,
          searching = TRUE,
          columnDefs = list(
            list(visible=FALSE, targets=1)
            ), # hide rec_id
          select = list(style = "single", items = "row") # configure single row selection.
        ),
        escape = FALSE, # allow html in data.
        selection = "none", # disable DT own's select implementation.
        extensions = c('Select', 'Buttons'), # Enable Select extension.
        callback = JS("onTableLoadFinish(table)") # call custom js function of ours on end.
      )
    },
    server = FALSE)  ## use client-side processing

    observeEvent(frontendData(), {

      # Calculate min and max date values
      date_limits <- r$recParsedData %>%
        select(toa) %>%
        summarise(min_date = min(toa), max_date=max(toa))

      # Update the slider with read-in date values
      updateSliderInput(
        session,
        "datetime_slider",
        min = date_limits$min_date,
        max = date_limits$max_date,
        value = c(date_limits$min_date, date_limits$max_date),
        timeFormat = "%H:%M:%S",
        timezone = "+0000" # we assume that all times are UTC
      )

      #Notify the client to display the slider input
      session$sendCustomMessage("showDateTimeSlider", list())
    })


    ## Hack: Listen on the server-side for changes on the datetime slider
    ## On any change, notify the client via custom function such that the table gets filtered.
    ## Ideally filtering should be handely completely on the client side.
    ## However 1) crosstalk's filter_slider did not work with DT (an empty input box was shown in the UI - no slider. Bug?)
    ## 2) Filtering the reactive frontendData() using dplyr and rendering it on the client side is not supported without
    ## server = TRUE. We need server=FALSE to utilize the Select extension such that the checkbox column is not selectable.
    observeEvent(input$datetime_slider, {
      session$sendCustomMessage(
        "filterTableByDate",
        # Send as JSON the user selected range to filter the dates on.
        list(from = format_ISO8601(input$datetime_slider[[1]]),
             to = format_ISO8601(input$datetime_slider[[2]]))
        )

    })

    ## Observe changes to the row ids shown on the current page

    #observe({
    #  req(input$unmatched_calls_rows_current)
    observeEvent(input$unmatched_calls_rows_current, {
      req(frontendData())

      # array of base64 images and the frontend row index they correspond to
      encodedImages <- list()
      idx <- 1
      ## For each row that is visible on the current page of the table
      for (i in input$unmatched_calls_rows_current) {
          # Get row from backend using frontend row id.
          backendRecID <- frontendData() %>%
            slice(i) %>%
            pull(rec_id)

          # Filter backend datatable by record ID
          backendRow <- filter(r$recParsedData, rec_id == backendRecID)

          #Get relative path of spectrogram from backend
          spectroRelativePath <- path(backendRow$spectrogram)

          # Get the absolute path to the image according to csv file path provided by shinyFiles
          if(is.null(r$recDataAbsFilePath)) {
            golem::invoke_js("erroralert", list(title="Failed to read file path", msg="The recordings csv file path was not provided."))
            break;
          }
          else {
            # Construct an absolute path to the spectrogram image
            # fs::path_real should make sure that this works both on UNIX and Windows and that the file exists
            # If the file does not exist, throw error and break out of loop
            spectroAbsPath <- attempt(path_real(path(path_dir(r$recDataAbsFilePath), spectroRelativePath)))
            if (is_try_error(spectroAbsPath)) {
              golem::invoke_js("erroralert", list(title="Failed to read file path", msg=spectroAbsPath))
              break;
            }
            ## Read the input image file and base64 encode it
            b64data <- encode_image(spectroAbsPath)

            #i minus one since array indexing in javascript begins at zero (like any other normal language!)
            encodedImages[[idx]] <- list(rowId = i-1, src=b64data)

            idx = idx + 1;
          }
      }
      # Send JSON array to client of format [ {rowId: <num>, src: <base64string>} ]
      session$sendCustomMessage("updateTableSpectrogramImages", encodedImages)
    })


    # On "Create new call group" button click
    # Create a call group of the checked calls and add it to the reactive call_groups list
    # If successful, hide the checked calls from the frontend table.
    observeEvent(input$create_group2, {
      req(input$checked_rows) # Only run if there are checked rows
      req(frontendData())

      ## Get backend rows
      backend_rows <- get_backend_rows_by_frontend_id(frontendData(), r$recParsedData, input$checked_rows)

      ## Get rec id of first recording
      ## Given that record ids are unique, we will use this as the group id.
      group_id <- backend_rows$rec_id[[1]]
      group_toa <- lubridate::format_ISO8601(backend_rows$toa[[1]])


      call_group <- list(
        frontend_row_ids = input$checked_rows,
        backend_rows = backend_rows,
        group_id = group_id,
        group_toa = group_toa
      )

      ## Append call group to list of call groups
      if (is.null(r$call_groups) || (length(r$call_groups) == 0)) {
        r$call_groups <- list()
        r$call_groups[[1]] <- call_group
      } else {
        golem::print_dev("appended")
        r$call_groups[[length(r$call_groups)+1]] <- call_group
      }

    })


  #' Generate a table row for each of the calls in a call group.
  #' The table row contains:
  #' - frontend row id
  #' - mic id
  #' - toa
  #' - sex
  #' and a remove button, to remove a given call from a group.
  generate_rows <- function(call_group, display_cols) {
    rows <- list()
    golem::print_dev(nrow(call_group$backend_rows))
    for (i in seq_len(nrow(call_group$backend_rows))) { # For each backend row
      cells <- lapply(display_cols, function(col) {
        val <- call_group$backend_rows[[col]][[i]]
        tags$td(as.character(val))
      })
      rows[[i]] <- tags$tr(
        tags$td(tags$button(
          type="button",
          # id: <rm_call_<row_id>_group_<group_id>
          id=paste0("rm_call_", call_group$frontend_row_ids[[i]], "_group_", call_group$group_id),
          class="btn-close",
          `aria-label`="Remove",
          style="padding-top: 0.5em; padding-bottom: 0em;",
          onclick="onRemoveCallFromGroupBtnClick(this)"
          )),
        tags$th(scope="row", call_group$frontend_row_ids[[i]]),
        !!!cells
      )
    }
    #golem::print_dev(rows)
    return(rows)
  }

  #' Each time the call_groups list changes
    #' Re-render the accordion containing tables of grouped calls
    observeEvent(r$call_groups, {
      panels <- list()
      rows_to_hide <- c()
      display_cols <- group_display_cols()
      for (i in seq_along(r$call_groups)) {
        call_group <- r$call_groups[[i]]
        rows_to_hide <- c(rows_to_hide, call_group$frontend_row_ids)
        panels[[i]] <- bslib::accordion_panel(
          paste0("Call Group #", i, " | ", "TOA : ", call_group$group_toa),
          tags$table(
            class="table table-bordered table-hover",
            tags$thead(tags$tr(
              tags$th(scope="col", ""),
              tags$th(scope="col", "row id"),
              lapply(display_cols, function(col) tags$th(scope="col", col))
            )
            ),
            tags$tbody(!!!generate_rows(call_group, display_cols))
        )
        )
      }
      # Render the accordion and make sure it is centered inside the fluidRow
      out <- bslib::accordion(!!!panels, style="max-width: fit-content; margin-left: auto; margin-right: auto;")
      #golem::print_dev(out)
      output$matched_calls <- renderUI({out})

      # Minus one all rows since javascript/datatable starts index at 0.
      rows_to_hide <- purrr::map(rows_to_hide, \(row_num) row_num-1)
      ## Hide the rows which are in call groups from the main table
      golem::print_dev(paste("called hide table rows", rows_to_hide))
      session$sendCustomMessage("hideTableRows", rows_to_hide)
    }, priority = 2)


    # Each time the call_groups list changes,
    # Update the dropdown of existing groups on the client
    observeEvent(r$call_groups, {
      out <- list()
      for (i in seq_along(r$call_groups)) {
        call_group <- r$call_groups[[i]]
        #name <- paste0("Call Group #", i, " | ", "TOA : ", call_group$group_toa)
        name <- paste0("Call Group #", i) # Only show call group num
        # A choice is a key-pair value where :
        # key is the name of the group displayed on the frontend
        # value is what is sent to the server when the user selects something.
        # in this case we send the group id.
        out[[name]] <- call_group$group_id
      }
      updateSelectInput(session, "existing_group", choices = out)
    }, priority = 1)


    # Add call to existing group logic
    observeEvent(input$add_to_group, {
      req(input$existing_group, input$checked_rows, r$call_groups, frontendData())
      group_id <- as.numeric(input$existing_group)

      for (i in seq_along(r$call_groups)) {
        call_group <- r$call_groups[[i]]
        if (call_group$group_id == group_id) {
          ## Get backend rows
          backend_rows <- get_backend_rows_by_frontend_id(frontendData(), r$recParsedData, input$checked_rows)

          ## Append the checked row ids to the stored row ids of call_group list
          call_group$frontend_row_ids <- c(call_group$frontend_row_ids, input$checked_rows)

          ##Append backend rows of new checked calls to stored backend rows of call_group list
          call_group$backend_rows <- call_group$backend_rows %>% bind_rows(backend_rows)

          ## Insert modified call_group to the reactive values list
          #golem::print_dev(call_group)
          r$call_groups[[i]] <- call_group
          break
        }
      }
    })


    # Remove call from group logic
    observeEvent(input$remove_call_from_group, {
      req(r$call_groups)
      row_id = input$remove_call_from_group$rows[[1]]
      group_id = input$remove_call_from_group$rows[[2]]
      removed = FALSE
      # Find the call group via linear search
      for(i in seq_along(r$call_groups)) {

        if ( r$call_groups[[i]]$group_id == group_id) {
          # Find index of the row_id in the frontend_row_ids vector
          j <- which(r$call_groups[[i]]$frontend_row_ids == row_id)
          # If found
          if (length(j) != 0) {
            #Remove it from frontend row ids
            r$call_groups[[i]]$frontend_row_ids <-  r$call_groups[[i]]$frontend_row_ids[-j]
            r$call_groups[[i]]$backend_rows <-  r$call_groups[[i]]$backend_rows %>% slice(-j)
            removed = TRUE

            # If all calls removed
            if (nrow(r$call_groups[[i]]$backend_rows) == 0) {
              golem::print_dev(paste("Removing entire row", i))
              # Remove the entire call group
              r$call_groups <- r$call_groups[-i]
            }
            else {
              ## Update the group id and toa
              ## We must update the group id because if the removed call is added to another group,
              ## the two groups may end up having the same id.
              r$call_groups[[i]]$group_id <- r$call_groups[[i]]$backend_rows$rec_id[[1]]
              r$call_groups[[i]]$group_toa <- lubridate::format_ISO8601(r$call_groups[[i]]$backend_rows$toa[[1]])
            }
          }
          break
        }
      }

      if (removed) { # If the removal was successful, unhide the row from the main table.
        session$sendCustomMessage("showTableRows", row_id-1)
      }

    })






#    proxy = dataTableProxy('unmatched_calls')

#    observeEvent(on_row_select_data(), {
#      replaceData(proxy, on_row_select_data(), resetPaging = FALSE, clearSelection="none")
#    }, ignoreInit = TRUE)


  })
}

## To be copied in the UI
# mod_match_calls_ui("match_calls_1")

## To be copied in the server
# mod_match_calls_server("match_calls_1")
