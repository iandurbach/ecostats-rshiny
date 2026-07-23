#!/usr/bin/env Rscript

# User settings for interactive use.
#
# In R/RStudio, edit these values and source this file. Set list_sessions_only
# to TRUE first if you want to inspect the available morning/evening bouts,
# then set session_to_group to "all", a session number such as "2", or a
# session ID such as "session_2".
database_stem <- "NCNX06"
session_to_group <- "all"
database_directory <- file.path("data", "db")
output_file <- "NCNX06_detections.Rdata"
session_gap_minutes <- 30
solver <- "auto"
list_sessions_only <- FALSE
run_when_sourced <- TRUE

script_file <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = FALSE))
  }
  frame_files <- vapply(sys.frames(), function(frame) {
    file <- frame$ofile
    if (is.null(file)) "" else file
  }, character(1))
  frame_files <- frame_files[nzchar(frame_files)]
  if (length(frame_files) > 0) {
    return(normalizePath(frame_files[[length(frame_files)]], mustWork = FALSE))
  }
  normalizePath("group_database_detections.R", mustWork = FALSE)
}

script_dir <- dirname(script_file())
repo_root <- dirname(script_dir)
source(file.path(script_dir, "group_gibbon_detections.R"))

discover_stem_databases <- function(stem, db_dir = file.path(repo_root, "data", "db")) {
  if (length(stem) != 1 || is.na(stem) || !nzchar(trimws(stem))) {
    stop("Database stem must be one non-empty string.", call. = FALSE)
  }
  paths <- Sys.glob(file.path(db_dir, paste0(stem, "*_database.sqlite3")))
  paths <- sort(normalizePath(paths, mustWork = FALSE))
  if (length(paths) == 0) {
    stop(
      "No databases found for stem '", stem, "' in ", normalizePath(db_dir, mustWork = FALSE), ".",
      call. = FALSE
    )
  }
  paths
}

format_session_table <- function(sessions, counts = NULL) {
  out <- sessions
  if (!is.null(counts)) {
    out <- dplyr::left_join(out, counts, by = "session_id")
    out$n_detections[is.na(out$n_detections)] <- 0L
  }
  data.frame(
    session = seq_len(nrow(out)),
    session_id = out$session_id,
    start_utc = format(out$real_start, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    stop_utc = format(out$real_stop, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    n_detections = if ("n_detections" %in% names(out)) out$n_detections else NA_integer_,
    stringsAsFactors = FALSE
  )
}

resolve_session_id <- function(session, sessions) {
  session <- trimws(as.character(session))
  if (length(session) != 1 || is.na(session) || !nzchar(session)) {
    stop("Session must be 'all', a session number, or a session ID.", call. = FALSE)
  }
  if (tolower(session) == "all") return("all")
  if (grepl("^[0-9]+$", session)) {
    index <- as.integer(session)
    if (index >= 1 && index <= nrow(sessions)) return(sessions$session_id[[index]])
  }
  if (session %in% sessions$session_id) return(session)
  stop(
    "Unknown session '", session, "'. Use --list-sessions to see valid sessions.",
    call. = FALSE
  )
}

load_stem_data <- function(stem, db_dir, session_gap_minutes = 30) {
  db_paths <- discover_stem_databases(stem, db_dir)
  input <- read_grouping_databases(db_paths)
  detections <- prepare_grouping_inputs(input$recData, input$micData)$detections
  intervals <- read_recording_intervals_from_databases(db_paths)
  sessions <- build_recording_sessions(intervals, gap_seconds = session_gap_minutes * 60)
  if (nrow(sessions) == 0) {
    stop("No recording sessions were found in Sound_Acquisition.", call. = FALSE)
  }
  timeline <- assign_detections_to_sessions(detections, sessions)
  counts <- timeline |>
    dplyr::filter(!is.na(session_id)) |>
    dplyr::count(session_id, name = "n_detections")
  list(
    db_paths = db_paths,
    input = input,
    timeline = timeline,
    sessions = sessions,
    session_table = format_session_table(sessions, counts)
  )
}

group_database_stem <- function(stem = database_stem, session = session_to_group,
                                db_dir = database_directory, out = output_file,
                                session_gap_minutes = 30, solver = "auto") {
  if (!grepl("^(/|[A-Za-z]:[/\\\\])", db_dir)) {
    db_dir <- file.path(repo_root, db_dir)
  }
  loaded <- load_stem_data(stem, db_dir, session_gap_minutes)
  selected_session <- resolve_session_id(session, loaded$sessions)
  timeline <- loaded$timeline[!is.na(loaded$timeline$session_id), , drop = FALSE]
  if (selected_session != "all") {
    timeline <- timeline[timeline$session_id == selected_session, , drop = FALSE]
  }
  if (nrow(timeline) == 0) {
    stop("The selected session contains no detections.", call. = FALSE)
  }

  recordings <- loaded$input$recData[
    loaded$input$recData$recording_ID %in% timeline$rec_id,
    ,
    drop = FALSE
  ]
  session_by_detection <- stats::setNames(timeline$session_id, timeline$rec_id)
  original_cluster <- as.character(recordings$cluster_id)
  original_cluster[is.na(original_cluster)] <- "unknown"
  recordings$cluster_id <- paste(
    original_cluster,
    unname(session_by_detection[as.character(recordings$recording_ID)]),
    sep = "::"
  )

  mic_data <- loaded$input$micData
  # Composite cluster/session IDs partition detections by bout. Keep every
  # recorder location available to the bearing fit within each partition.
  mic_data$cluster_id <- NA_character_
  result <- group_gibbon_detections(recordings, mic_data, solver = solver)
  result$database_paths <- loaded$db_paths
  result$sessions <- loaded$session_table
  result$session_selection <- selected_session

  if (is.null(out)) {
    label <- if (selected_session == "all") "all_sessions" else selected_session
    out <- file.path(repo_root, paste0(stem, "_", label, "_auto_groups.RData"))
  } else if (!grepl("^(/|[A-Za-z]:[/\\\\])", out)) {
    out <- file.path(repo_root, out)
  }
  save_app_loadable_groups(result, out)
  cat(
    "Grouped ", nrow(timeline), " detections from ", length(loaded$db_paths),
    " databases across ",
    if (selected_session == "all") nrow(loaded$sessions) else 1L,
    " session(s).\n",
    "Selected ", length(unique(result$group_membership$group_ID)),
    " groups; left ", nrow(result$ungrouped_detections), " detections ungrouped.\n",
    "Wrote ", normalizePath(out, mustWork = FALSE), "\n",
    sep = ""
  )
  invisible(result)
}

parse_args <- function(args) {
  opts <- list()
  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg %in% c("--help", "--list-sessions")) {
      opts[[sub("^--", "", arg)]] <- TRUE
      i <- i + 1L
    } else if (startsWith(arg, "--") && i < length(args)) {
      opts[[sub("^--", "", arg)]] <- args[[i + 1L]]
      i <- i + 2L
    } else {
      stop("Unexpected or incomplete argument: ", arg, call. = FALSE)
    }
  }
  opts
}

print_usage <- function() {
  cat(
    "Usage:\n",
    "  Rscript group_database_detections.R [options]\n\n",
    "Interactive use:\n",
    "  Edit the settings at the top of this file, then source it in R/RStudio.\n\n",
    "Options:\n",
    "  --stem STEM             Database filename stem. Default: NCNX06\n",
    "  --db-dir PATH           Database directory. Default: data/db\n",
    "  --session VALUE         all, a 1-based session number, or session_ID. Default: all\n",
    "  --list-sessions         Print sessions and detection counts, then exit.\n",
    "  --session-gap MINUTES   Gap used to separate sessions. Default: 30\n",
    "  --solver VALUE          auto, ompr, or lpsolve. Default: auto\n",
    "  --out PATH              Output RData path.\n",
    "  --help                  Show this help.\n",
    sep = ""
  )
}

run_from_settings <- function() {
  if (isTRUE(list_sessions_only)) {
    db_dir <- database_directory
    if (!grepl("^(/|[A-Za-z]:[/\\\\])", db_dir)) {
      db_dir <- file.path(repo_root, db_dir)
    }
    loaded <- load_stem_data(database_stem, db_dir, session_gap_minutes)
    print(loaded$session_table, row.names = FALSE)
    return(invisible(loaded$session_table))
  }
  group_database_stem(
    stem = database_stem,
    session = session_to_group,
    db_dir = database_directory,
    out = output_file,
    session_gap_minutes = session_gap_minutes,
    solver = solver
  )
}

run_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  opts <- parse_args(args)
  if (isTRUE(opts$help)) {
    print_usage()
    return(invisible(NULL))
  }
  stem <- opts$stem %||% database_stem
  db_dir <- opts[["db-dir"]] %||% file.path(repo_root, database_directory)
  gap <- as.numeric(opts[["session-gap"]] %||% 30)
  if (isTRUE(opts[["list-sessions"]])) {
    loaded <- load_stem_data(stem, db_dir, gap)
    print(loaded$session_table, row.names = FALSE)
    return(invisible(loaded$session_table))
  }
  group_database_stem(
    stem = stem,
    session = opts$session %||% session_to_group,
    db_dir = db_dir,
    out = opts$out %||% output_file,
    session_gap_minutes = gap,
    solver = opts$solver %||% "auto"
  )
}

if (sys.nframe() == 0 && !interactive()) {
  run_cli()
} else if (interactive() && isTRUE(run_when_sourced)) {
  run_from_settings()
}
