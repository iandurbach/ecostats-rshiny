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
#' @param x Character vector of datetimes to parse.
#' @param tz Timezone to assign to the parsed datetimes.
#' @returns parsed POSIXct vector.
#' @noRd
parse_datetime_column <- function(x, tz = "UTC") {
  raw_values <- trimws(as.character(x))
  non_missing <- !(is.na(raw_values) | raw_values == "")
  values <- raw_values[non_missing]

  if (length(values) == 0) {
    stop("Datetime column is empty.")
  }

  formats <- c(
    "%Y-%m-%d %H:%M:%OS",
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%d %H:%M",
    "%Y/%m/%d %H:%M:%OS",
    "%Y/%m/%d %H:%M:%S",
    "%Y/%m/%d %H:%M",
    "%Y-%m-%dT%H:%M:%OS",
    "%Y-%m-%dT%H:%M:%S",
    "%Y-%m-%dT%H:%M",
    "%d/%m/%Y %H:%M:%OS",
    "%d/%m/%Y %H:%M:%S",
    "%d/%m/%Y %H:%M",
    "%m/%d/%Y %H:%M:%OS",
    "%m/%d/%Y %H:%M:%S",
    "%m/%d/%Y %H:%M"
  )

  has_fractional_seconds <- grepl(":\\d{2}\\.\\d+$", values)
  if (any(has_fractional_seconds)) {
    formats <- formats[grepl("%OS", formats, fixed = TRUE)]
  }

  successful_formats <- character(0)
  successful_parses <- list()

  for (fmt in formats) {
    parsed <- as.POSIXct(strptime(values, format = fmt, tz = tz))
    if (all(!is.na(parsed))) {
      successful_formats <- c(successful_formats, fmt)
      successful_parses[[fmt]] <- parsed
    }
  }

  if (length(successful_formats) == 0) {
    stop(
      paste0(
        "Could not parse GPSDatetime2. Supported formats are: ",
        paste(formats, collapse = ", ")
      )
    )
  }

  if (length(successful_formats) > 1) {
    first_parse <- successful_parses[[successful_formats[[1]]]]
    equivalent_formats <- vapply(
      successful_formats,
      function(fmt) identical(unclass(successful_parses[[fmt]]), unclass(first_parse)),
      logical(1)
    )

    if (!all(equivalent_formats)) {
      stop(
        paste0(
          "Ambiguous datetime format in GPSDatetime2. Multiple formats matched: ",
          paste(successful_formats, collapse = ", "),
          ". Please use an unambiguous datetime format."
        )
      )
    }
  }

  parsed_all <- as.POSIXct(rep(NA_real_, length(raw_values)), origin = "1970-01-01", tz = tz)
  parsed_all[non_missing] <- successful_parses[[successful_formats[[1]]]]
  parsed_all
}

make_internal_rec_ids <- function(recording_ids) {
  ids <- as.character(recording_ids)
  if (anyDuplicated(ids)) {
    ids <- make.unique(ids, sep = "_")
  }
  ids
}

#'
#' @param recordings A tibble containing the recorded calls read from a csv.
#' @returns standardised recordings data with parsed time of arrival as datetime object.
#' @noRd
#'
#' @importFrom dplyr mutate select arrange

parse_rec_data <- function(recordings, extra_cols = NULL) {
  required_cols <- c("recording_ID", "mic_ID", "GPSDatetime2", "measured_bearing", "measured_gender", "spectrogram")
  missing_required <- setdiff(required_cols, names(recordings))
  if (length(missing_required) > 0) {
    stop(paste("Missing required column(s):", paste(missing_required, collapse = ", ")))
  }

  selected_extra <- intersect(if (is.null(extra_cols)) character(0) else extra_cols, names(recordings))
  backend_cols <- intersect(
    c(
      "database_path", "detection_table", "detection_id", "cluster_id", "raw_toa",
      "clock_offset", "Duration", "relative_bearing_rad", "vertical_bearing_rad",
      "recorder_heading", "wav_file", "recording_start_utc", "recording_stop_utc",
      "samples", "sample_rate", "start_frame", "end_frame"
    ),
    names(recordings)
  )
  selected_cols <- unique(c(required_cols, selected_extra, backend_cols))

  recordings |>
    # Keep required columns plus any extras the user opted in to show later
    dplyr::select(dplyr::all_of(selected_cols)) |>
    # standardise column names used internally while keeping the originals
    dplyr::mutate(
      rec_id = make_internal_rec_ids(recording_ID),
      mic_id = mic_ID,
      toa = parse_datetime_column(GPSDatetime2, tz = "UTC"),
      bearing = measured_bearing,
      sex = measured_gender,
      suspect_bearing = FALSE
    ) |>
    # order by toa ascending
    dplyr::arrange(toa)
}

#' Encode an image as a browser-ready data URI.
#' @importFrom base64enc base64encode
#' @importFrom tools file_ext
#' @noRd
encode_image <- function(img_path) {
  ext <- tolower(tools::file_ext(img_path))
  mime <- switch(
    ext,
    jpg = "image/jpeg",
    jpeg = "image/jpeg",
    png = "image/png",
    gif = "image/gif",
    "image/png"
  )
  paste0("data:", mime, ";base64,", base64enc::base64encode(img_path))
}
