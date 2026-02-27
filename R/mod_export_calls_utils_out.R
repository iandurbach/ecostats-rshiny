#'
#' @description
#' This function reads all the
#'
#' @importFrom dplyr select mutate
#' @importFrom purrr map list_rbind
#' @noRd
create_capture_history <- function(call_groups, selected_cols = character(0)) {
  if (is.null(call_groups) || length(call_groups) == 0) {
    return(dplyr::tibble())
  }

  base_cols <- c("Session", "ID", "Occasion", "Detector", "bearing", "toa")
  default_cols <- c("recording_ID", "mic_ID", "GPSDatetime2", "measured_bearing", "measured_gender", "spectrogram", "suspect_bearing")
  append_cols <- setdiff(unique(c(default_cols, selected_cols)), base_cols)

  map(call_groups, \(call_group) call_group$backend_rows %>%
      # Occasion is hard-coded since ascr does not support multi-occasion data.
      # Session will eventually be supplied by user input; Hard-coded for now.
      mutate(Session = "demo", ID = call_group$group_id, Occasion = as.integer(1), Detector = mic_id, toa = as.numeric(toa)) %>%
      select(dplyr::all_of(base_cols), dplyr::any_of(append_cols))
  ) %>%
    purrr::list_rbind()
}
