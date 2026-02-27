#' helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @importFrom attempt attempt
#' @importFrom tools file_ext
read_csv_vroom <- function(datapath, ...) {

  if(tolower(tools::file_ext(datapath)) != "csv") {
    stop("Invalid file. Please upload a .csv file!")
  }
  # Save arguments to list
  args <- list(datapath, ...)
  # If only datapath argument
  if (length(args) == 1) {
    # Append default arguments
#    args <- list(datapath, delim = ",", col_names = TRUE, na = c("NA", "NULL", ""))
    args <- list(datapath, tryLogical = F, stringsAsFactors = F)
  }
  # Attempt to call vroom with arguments.
#  return(attempt(do.call(vroom, args)))
  return(attempt(do.call(read.csv, args)))

}


#' Remaining/Unmatched calls calculation Function
#'
#' @description
#' This function returns a tibble of the number of unmatched calls per day.
#'
#' @details
#' This function uses the datetime column of the passed-in recordings tibble to group calls by day
#' and count how many calls exist per day. This allows to quickly pick a day of calls they
#' would like to work on.
#'
#' @param recordings A tibble containing the recorded calls read from a csv.
#' @returns standardised recordings data with parsed time of arrival as datetime object.
#' @noRd
#'
#' @importFrom dplyr mutate group_by summarise n rename select arrange
#' @importFrom lubridate ymd_hms date stamp

parse_rec_data <- function(recordings, extra_cols = NULL) {
  required_cols <- c("recording_ID", "mic_ID", "GPSDatetime2", "measured_bearing", "measured_gender", "spectrogram")
  missing_required <- setdiff(required_cols, names(recordings))
  if (length(missing_required) > 0) {
    stop(paste("Missing required column(s):", paste(missing_required, collapse = ", ")))
  }

  selected_extra <- intersect(if (is.null(extra_cols)) character(0) else extra_cols, names(recordings))
  selected_cols <- unique(c(required_cols, selected_extra))

  recordings %>%
    # Keep required columns plus any extras the user opted in to show later
    select(all_of(selected_cols)) %>%
    # standardise column names used internally while keeping the originals
    mutate(
      rec_id = recording_ID,
      mic_id = mic_ID,
      toa = ymd_hms(GPSDatetime2, tz="UTC"),
      bearing = measured_bearing,
      sex = measured_gender,
      suspect_bearing = FALSE
    ) %>%
    # order by toa ascending
    arrange(toa)
}

#' Get backend row by frontend row id
#'
#' @description
#' This function returns the row from recParsedData corresponding to the one in frontendData.
#'
#'
#' @param frontendData The tibble the client sees on the frontend
#' @param backendData The tibble the server holds.
#' @returns the backend rows with the same rec_ids as the frontend rows.
#' @importFrom dplyr slice pull filter
#' @noRd
get_backend_rows_by_frontend_id <- function(frontendData, backendData, frontendRowIDs) {
  rec_ids <- frontendData %>%
    slice(frontendRowIDs) %>%
    pull(rec_id)

  out <- backendData %>%
    filter(rec_id %in% rec_ids)
  return(out)
}

show_alert <- function(msg, title) {
  golem::invoke_js("erroralert", list(title=title, msg=msg))
}
