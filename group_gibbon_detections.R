#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  for (pkg in c("dplyr", "lubridate", "DBI", "RSQLite", "geosphere")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Required package is not installed: ", pkg, call. = FALSE)
    }
    library(pkg, character.only = TRUE)
  }
})

script_path <- function() {
  frames <- sys.frames()
  ofiles <- vapply(frames, function(frame) {
    if (is.null(frame$ofile)) NA_character_ else frame$ofile
  }, character(1))
  ofiles <- ofiles[!is.na(ofiles)]
  if (length(ofiles) > 0) {
    return(normalizePath(ofiles[[length(ofiles)]], mustWork = FALSE))
  }
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    return(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = FALSE))
  }
  normalizePath("group_gibbon_detections.R", mustWork = FALSE)
}

script_dir <- dirname(script_path())
helper_paths <- file.path(
  script_dir,
  "R",
  c(
    "fct_database_inputs.R",
    "fct_helpers.R",
    "fct_detection_timeline.R",
    "fct_gibbon_clustering.R"
  )
)
missing_helpers <- helper_paths[!file.exists(helper_paths)]
if (length(missing_helpers) > 0) {
  stop("Missing clustering helper file(s): ", paste(missing_helpers, collapse = ", "), call. = FALSE)
}
invisible(lapply(helper_paths, source, local = globalenv()))

if (sys.nframe() == 0) {
  run_cli()
}
