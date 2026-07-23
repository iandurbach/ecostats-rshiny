#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  for (pkg in c("dplyr", "lubridate", "DBI", "RSQLite", "geosphere")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Required package is not installed: ", pkg, call. = FALSE)
    }
    library(pkg, character.only = TRUE)
  }
})

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

script_path <- function() {
  frames <- sys.frames()
  ofiles <- vapply(frames, function(frame) frame$ofile %||% NA_character_, character(1))
  ofiles <- ofiles[!is.na(ofiles)]
  if (length(ofiles) > 0) {
    own_file <- ofiles[basename(ofiles) == "group_gibbon_detections.R"]
    if (length(own_file) > 0) {
      return(normalizePath(own_file[[length(own_file)]], mustWork = FALSE))
    }
    return(normalizePath(ofiles[[length(ofiles)]], mustWork = FALSE))
  }
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = FALSE))
  }
  normalizePath("group_gibbon_detections.R", mustWork = FALSE)
}

.group_gibbon_script_dir <- dirname(script_path())

source_repo_helpers <- function(repo_root = .group_gibbon_script_dir) {
  helper_dir <- if (dir.exists(file.path(repo_root, "R"))) {
    file.path(repo_root, "R")
  } else {
    file.path(repo_root, "ecostats-rshiny", "R")
  }
  helper_paths <- file.path(helper_dir, c("fct_database_inputs.R", "fct_helpers.R", "fct_detection_timeline.R"))
  existing <- helper_paths[file.exists(helper_paths)]
  invisible(lapply(existing, source, local = globalenv()))
}

normalize_degrees <- function(x) {
  ((x %% 360) + 360) %% 360
}

circular_diff_degrees <- function(a, b) {
  abs(((a - b + 180) %% 360) - 180)
}

require_columns <- function(data, cols, data_name) {
  missing <- setdiff(cols, names(data))
  if (length(missing) > 0) {
    stop(data_name, " is missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
}

empty_cluster_result <- function(detections) {
  list(
    group_membership = data.frame(
      group_ID = integer(0),
      rec_id = character(0),
      Notes = character(0),
      suspect_bearing = logical(0),
      stringsAsFactors = FALSE
    ),
    candidates = data.frame(
      candidate_id = integer(0),
      rec_ids = character(0),
      group_size = integer(0),
      temporal_span_seconds = numeric(0),
      temporal_rmse_seconds = numeric(0),
      temporal_compactness = numeric(0),
      temporal_score = numeric(0),
      bearing_miss_degrees = numeric(0),
      bearing_score = numeric(0),
      score = numeric(0),
      stringsAsFactors = FALSE
    ),
    selected_candidate_ids = integer(0),
    ungrouped_detections = detections
  )
}

validate_grouping_inputs <- function(detections, mics = NULL) {
  require_columns(detections, c("rec_id", "mic_id", "toa"), "detections")
  if (!inherits(detections$toa, "POSIXct")) {
    stop("detections$toa must be POSIXct.", call. = FALSE)
  }
  if (anyDuplicated(as.character(detections$rec_id))) {
    stop("detections$rec_id must be unique.", call. = FALSE)
  }
  if (!is.null(mics)) {
    require_columns(mics, c("mic_id", "lat", "lng"), "mics")
  }
  invisible(TRUE)
}

prepare_grouping_inputs <- function(recordings, mics = NULL) {
  if (!exists("parse_rec_data", mode = "function")) {
    source_repo_helpers()
  }
  detections <- parse_rec_data(recordings)
  detections$rec_id <- as.character(detections$rec_id)
  detections$mic_id <- as.character(detections$mic_id)
  detections$bearing <- suppressWarnings(as.numeric(detections$bearing))
  if (!"Duration" %in% names(detections)) detections$Duration <- NA_real_
  if (!"cluster_id" %in% names(detections)) detections$cluster_id <- NA_character_
  if (!is.null(mics)) {
    mics$mic_id <- as.character(mics$mic_id)
  }
  validate_grouping_inputs(detections, mics)
  list(detections = detections, mics = mics)
}

read_grouping_databases <- function(db_paths, offset_sign = 1) {
  source_repo_helpers()
  read_detection_databases(db_paths, offset_sign = offset_sign)
}

read_grouping_csv <- function(recording_csv, mic_csv = NULL) {
  recordings <- utils::read.csv(recording_csv, stringsAsFactors = FALSE, tryLogical = FALSE)
  mics <- if (is.null(mic_csv)) NULL else utils::read.csv(mic_csv, stringsAsFactors = FALSE, tryLogical = FALSE)
  list(recData = recordings, micData = mics)
}

robust_angular_loss <- function(miss_degrees, transition_degrees = 30) {
  scaled <- miss_degrees / transition_degrees
  ifelse(scaled <= 1, 0.5 * scaled^2, scaled - 0.5)
}

validate_scoring_parameters <- function(time_sigma_seconds, chance_compactness,
                                        size_bonus, bearing_weight) {
  values <- c(
    time_sigma_seconds = time_sigma_seconds,
    chance_compactness = chance_compactness,
    size_bonus = size_bonus,
    bearing_weight = bearing_weight
  )
  if (any(!is.finite(values))) {
    stop("Scoring parameters must be finite.", call. = FALSE)
  }
  if (time_sigma_seconds <= 0) {
    stop("time_sigma_seconds must be greater than zero.", call. = FALSE)
  }
  if (chance_compactness < 0 || chance_compactness > 1) {
    stop("chance_compactness must be between zero and one.", call. = FALSE)
  }
  if (size_bonus < 0 || bearing_weight < 0) {
    stop("size_bonus and bearing_weight must be non-negative.", call. = FALSE)
  }
  invisible(TRUE)
}

fit_bearing_location <- function(joined, max_source_distance_m = 5000) {
  center <- c(lng = mean(joined$lng), lat = mean(joined$lat))
  latitude_scale <- cos(center[["lat"]] * pi / 180)
  to_xy <- function(lng, lat) {
    cbind(
      x = (lng - center[["lng"]]) * 111320 * latitude_scale,
      y = (lat - center[["lat"]]) * 110540
    )
  }
  mic_xy <- to_xy(joined$lng, joined$lat)
  direction_xy <- cbind(
    x = sin(joined$bearing * pi / 180),
    y = cos(joined$bearing * pi / 180)
  )
  starts <- rbind(
    colMeans(mic_xy),
    do.call(rbind, lapply(c(250, 1000, 3000), function(distance) {
      mic_xy + distance * direction_xy
    }))
  )
  lower <- apply(mic_xy, 2, min) - max_source_distance_m
  upper <- apply(mic_xy, 2, max) + max_source_distance_m
  objective <- function(source_xy) {
    predicted <- normalize_degrees(atan2(
      source_xy[[1]] - mic_xy[, "x"],
      source_xy[[2]] - mic_xy[, "y"]
    ) * 180 / pi)
    sum(robust_angular_loss(circular_diff_degrees(joined$bearing, predicted)))
  }
  fits <- lapply(seq_len(nrow(starts)), function(i) {
    stats::optim(
      starts[i, ],
      objective,
      method = "L-BFGS-B",
      lower = lower,
      upper = upper
    )
  })
  best <- fits[[which.min(vapply(fits, `[[`, numeric(1), "value"))]]
  predicted <- normalize_degrees(atan2(
    best$par[[1]] - mic_xy[, "x"],
    best$par[[2]] - mic_xy[, "y"]
  ) * 180 / pi)
  list(
    xy = best$par,
    miss_degrees = sqrt(mean(circular_diff_degrees(joined$bearing, predicted)^2))
  )
}

candidate_bearing_score <- function(rows, mics, max_bearing_miss_degrees = 45,
                                    weight = 0.2, max_source_distance_m = 5000) {
  no_bearing <- list(score = 0, miss_degrees = NA_real_)
  if (is.null(mics) || nrow(rows) < 2) {
    return(no_bearing)
  }
  if (!all(c("mic_id", "bearing") %in% names(rows)) || !all(c("mic_id", "lat", "lng") %in% names(mics))) {
    return(no_bearing)
  }
  joined <- dplyr::left_join(rows, unique(mics[, c("mic_id", "lat", "lng")]), by = "mic_id")
  joined <- joined[
    !is.na(joined$lat) & !is.na(joined$lng) & !is.na(joined$bearing),
    ,
    drop = FALSE
  ]
  if (nrow(joined) < 2) return(no_bearing)

  fit <- fit_bearing_location(joined, max_source_distance_m = max_source_distance_m)
  if (!is.finite(fit$miss_degrees)) return(no_bearing)
  list(
    score = weight * max(-1, 1 - fit$miss_degrees / max_bearing_miss_degrees),
    miss_degrees = fit$miss_degrees
  )
}

score_candidate_group <- function(rows, mics = NULL, time_sigma_seconds = 2.5,
                                  chance_compactness = 0.25, size_bonus = 0.05,
                                  bearing_weight = 0.2) {
  times <- as.numeric(rows$toa)
  span <- max(times) - min(times)
  residuals <- times - mean(times)
  temporal_rmse <- sqrt(mean(residuals^2))
  compactness <- exp(-(temporal_rmse / time_sigma_seconds)^2)
  temporal_score <- (nrow(rows) - 1) * (compactness - chance_compactness) +
    max(0, nrow(rows) - 2) * size_bonus
  bearing <- candidate_bearing_score(rows, mics, weight = bearing_weight)
  list(
    temporal_span_seconds = span,
    temporal_rmse_seconds = temporal_rmse,
    temporal_compactness = compactness,
    temporal_score = temporal_score,
    bearing_miss_degrees = bearing$miss_degrees,
    bearing_score = bearing$score,
    score = temporal_score + bearing$score
  )
}

generate_candidate_groups <- function(detections, mics = NULL, max_group_span_seconds = 8, max_group_size = 5,
                                      min_score = 0.1, time_sigma_seconds = 2.5,
                                      chance_compactness = 0.25, size_bonus = 0.05,
                                      bearing_weight = 0.2) {
  validate_grouping_inputs(detections, mics)
  validate_scoring_parameters(time_sigma_seconds, chance_compactness, size_bonus, bearing_weight)
  if (!is.finite(max_group_span_seconds) || max_group_span_seconds < 0) {
    stop("max_group_span_seconds must be finite and non-negative.", call. = FALSE)
  }
  if (length(max_group_size) != 1 || is.na(max_group_size) || max_group_size < 2) {
    stop("max_group_size must be at least two.", call. = FALSE)
  }
  detections <- detections[order(detections$toa, detections$mic_id, detections$rec_id), , drop = FALSE]
  rownames(detections) <- NULL
  n <- nrow(detections)
  if (n < 2) {
    return(data.frame(
      candidate_id = integer(0),
      rec_ids = character(0),
      group_size = integer(0),
      temporal_span_seconds = numeric(0),
      temporal_rmse_seconds = numeric(0),
      temporal_compactness = numeric(0),
      temporal_score = numeric(0),
      bearing_miss_degrees = numeric(0),
      bearing_score = numeric(0),
      score = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  candidates <- list()
  seen <- new.env(parent = emptyenv())
  for (i in seq_len(n)) {
    start_time <- detections$toa[[i]]
    in_window <- which(detections$toa >= start_time & detections$toa <= start_time + max_group_span_seconds)
    in_window <- in_window[in_window >= i]
    other <- in_window[in_window != i & detections$mic_id[in_window] != detections$mic_id[[i]]]
    if (length(other) == 0) next

    other_mics <- split(other, detections$mic_id[other])
    possible_size <- min(max_group_size, length(other_mics) + 1L)
    for (group_size in seq.int(2L, possible_size)) {
      mic_sets <- utils::combn(names(other_mics), group_size - 1L, simplify = FALSE)
      for (mic_set in mic_sets) {
        choices <- c(list(i), unname(other_mics[mic_set]))
        grids <- do.call(expand.grid, c(choices, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE))
        for (g in seq_len(nrow(grids))) {
          idx <- as.integer(grids[g, ])
          if (min(idx) != i) next
          rows <- detections[idx, , drop = FALSE]
          rec_ids <- sort(as.character(rows$rec_id))
          key <- paste(rec_ids, collapse = "\r")
          if (exists(key, envir = seen, inherits = FALSE)) next
          assign(key, TRUE, envir = seen)
          parts <- score_candidate_group(
            rows,
            mics = mics,
            time_sigma_seconds = time_sigma_seconds,
            chance_compactness = chance_compactness,
            size_bonus = size_bonus,
            bearing_weight = bearing_weight
          )
          if (is.finite(parts$score) && parts$score >= min_score) {
            candidates[[length(candidates) + 1L]] <- data.frame(
              candidate_id = length(candidates) + 1L,
              rec_ids = paste(rec_ids, collapse = "|"),
              group_size = length(rec_ids),
              temporal_span_seconds = parts$temporal_span_seconds,
              temporal_rmse_seconds = parts$temporal_rmse_seconds,
              temporal_compactness = parts$temporal_compactness,
              temporal_score = parts$temporal_score,
              bearing_miss_degrees = parts$bearing_miss_degrees,
              bearing_score = parts$bearing_score,
              score = parts$score,
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
  }

  if (length(candidates) == 0) {
    return(data.frame(
      candidate_id = integer(0),
      rec_ids = character(0),
      group_size = integer(0),
      temporal_span_seconds = numeric(0),
      temporal_rmse_seconds = numeric(0),
      temporal_compactness = numeric(0),
      temporal_score = numeric(0),
      bearing_miss_degrees = numeric(0),
      bearing_score = numeric(0),
      score = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  dplyr::bind_rows(candidates)
}

candidate_members <- function(candidates) {
  strsplit(as.character(candidates$rec_ids), "|", fixed = TRUE)
}

solve_set_packing_lpsolve <- function(candidates, detections) {
  if (!requireNamespace("lpSolve", quietly = TRUE)) {
    stop("lpSolve is not installed.", call. = FALSE)
  }
  members <- candidate_members(candidates)
  detection_ids <- as.character(detections$rec_id)
  constraints <- matrix(0, nrow = length(detection_ids), ncol = nrow(candidates))
  for (j in seq_along(members)) {
    constraints[detection_ids %in% members[[j]], j] <- 1
  }
  result <- lpSolve::lp(
    direction = "max",
    objective.in = candidates$score,
    const.mat = constraints,
    const.dir = rep("<=", nrow(constraints)),
    const.rhs = rep(1, nrow(constraints)),
    all.bin = TRUE
  )
  if (result$status != 0) {
    stop("lpSolve did not find an optimal solution. Status: ", result$status, call. = FALSE)
  }
  which(result$solution > 0.5)
}

solve_set_packing_ompr <- function(candidates, detections) {
  required <- c("ompr", "ompr.roi", "ROI", "ROI.plugin.glpk")
  if (!all(vapply(required, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1)))) {
    stop("ompr solver stack is not installed.", call. = FALSE)
  }
  members <- candidate_members(candidates)
  detection_ids <- as.character(detections$rec_id)
  model <- ompr::MIPModel() |>
    ompr::add_variable(x[j], j = seq_len(nrow(candidates)), type = "binary") |>
    ompr::set_objective(ompr::sum_expr(candidates$score[j] * x[j], j = seq_len(nrow(candidates))), "max")
  for (rec_id in detection_ids) {
    containing <- which(vapply(members, function(ids) rec_id %in% ids, logical(1)))
    if (length(containing) > 0) {
      model <- model |> ompr::add_constraint(ompr::sum_expr(x[j], j = containing) <= 1)
    }
  }
  result <- ompr.roi::solve_model(model, ompr.roi::with_ROI(solver = "glpk"))
  solution <- ompr::get_solution(result, x[j])
  solution$j[solution$value > 0.5]
}

solve_set_packing <- function(candidates, detections, solver = c("auto", "ompr", "lpsolve")) {
  solver <- match.arg(solver)
  if (nrow(candidates) == 0) return(integer(0))
  if (solver %in% c("auto", "ompr") &&
      all(vapply(c("ompr", "ompr.roi", "ROI", "ROI.plugin.glpk"), requireNamespace, quietly = TRUE, FUN.VALUE = logical(1)))) {
    return(solve_set_packing_ompr(candidates, detections))
  }
  if (solver %in% c("auto", "lpsolve") && requireNamespace("lpSolve", quietly = TRUE)) {
    return(solve_set_packing_lpsolve(candidates, detections))
  }
  stop("No supported ILP solver is installed. Install ompr/ROI.plugin.glpk or lpSolve.", call. = FALSE)
}

format_selected_groups <- function(selected_candidates, detections, notes_prefix = "auto") {
  if (nrow(selected_candidates) == 0) {
    return(data.frame(
      group_ID = integer(0),
      rec_id = character(0),
      Notes = character(0),
      suspect_bearing = logical(0),
      stringsAsFactors = FALSE
    ))
  }
  pieces <- lapply(seq_len(nrow(selected_candidates)), function(i) {
    ids <- candidate_members(selected_candidates[i, , drop = FALSE])[[1]]
    data.frame(
      group_ID = rep(i, length(ids)),
      rec_id = ids,
      Notes = rep(
        sprintf(
          "%s candidate=%s score=%.3f span=%.2fs",
          notes_prefix,
          selected_candidates$candidate_id[[i]],
          selected_candidates$score[[i]],
          selected_candidates$temporal_span_seconds[[i]]
        ),
        length(ids)
      ),
      suspect_bearing = rep(FALSE, length(ids)),
      stringsAsFactors = FALSE
    )
  })
  out <- dplyr::bind_rows(pieces)
  out[order(match(out$rec_id, detections$rec_id)), , drop = FALSE]
}

group_detections_for_partition <- function(detections, mics = NULL, solver = "auto", ...) {
  if (nrow(detections) < 2 || length(unique(detections$mic_id)) < 2) {
    return(empty_cluster_result(detections))
  }
  candidates <- generate_candidate_groups(detections, mics = mics, ...)
  selected_idx <- solve_set_packing(candidates, detections, solver = solver)
  selected <- candidates[selected_idx, , drop = FALSE]
  group_membership <- format_selected_groups(selected, detections)
  ungrouped <- detections[!detections$rec_id %in% group_membership$rec_id, , drop = FALSE]
  list(
    group_membership = group_membership,
    candidates = candidates,
    selected_candidate_ids = candidates$candidate_id[selected_idx],
    ungrouped_detections = ungrouped
  )
}

group_gibbon_detections <- function(recordings, mics = NULL, solver = "auto",
                                    max_group_span_seconds = 8, max_group_size = 5,
                                    min_score = 0.1, time_sigma_seconds = 2.5,
                                    chance_compactness = 0.25, size_bonus = 0.05,
                                    bearing_weight = 0.2) {
  source_repo_helpers()
  prepared <- prepare_grouping_inputs(recordings, mics)
  detections <- prepared$detections
  mics <- prepared$mics
  partition <- if ("cluster_id" %in% names(detections) && any(!is.na(detections$cluster_id))) {
    ifelse(is.na(detections$cluster_id), "unknown", as.character(detections$cluster_id))
  } else {
    rep("all", nrow(detections))
  }
  split_detections <- split(detections, partition)
  results <- lapply(names(split_detections), function(key) {
    mic_subset <- if (is.null(mics) || !"cluster_id" %in% names(mics)) {
      mics
    } else {
      mics[is.na(mics$cluster_id) | as.character(mics$cluster_id) == key, , drop = FALSE]
    }
    group_detections_for_partition(
      split_detections[[key]],
      mics = mic_subset,
      solver = solver,
      max_group_span_seconds = max_group_span_seconds,
      max_group_size = max_group_size,
      min_score = min_score,
      time_sigma_seconds = time_sigma_seconds,
      chance_compactness = chance_compactness,
      size_bonus = size_bonus,
      bearing_weight = bearing_weight
    )
  })

  next_group <- 0L
  memberships <- lapply(results, function(result) {
    gm <- result$group_membership
    if (nrow(gm) == 0) return(gm)
    old_ids <- sort(unique(gm$group_ID))
    map <- stats::setNames(seq_along(old_ids) + next_group, old_ids)
    gm$group_ID <- unname(map[as.character(gm$group_ID)])
    next_group <<- max(gm$group_ID)
    gm
  })
  group_membership <- dplyr::bind_rows(memberships)
  grouped_detections <- export_grouped_detections(group_membership, detections)
  selected_ids <- unlist(lapply(results, `[[`, "selected_candidate_ids"), use.names = FALSE)
  all_candidates <- dplyr::bind_rows(lapply(seq_along(results), function(i) {
    candidates <- results[[i]]$candidates
    if (nrow(candidates) == 0) return(candidates)
    candidates$partition <- names(split_detections)[[i]]
    candidates
  }))
  list(
    group_membership = group_membership,
    grouped_detections = grouped_detections,
    candidates = all_candidates,
    selected_candidate_ids = selected_ids,
    ungrouped_detections = detections[!detections$rec_id %in% group_membership$rec_id, , drop = FALSE],
    parameters = list(
      solver = solver,
      max_group_span_seconds = max_group_span_seconds,
      max_group_size = max_group_size,
      min_score = min_score,
      time_sigma_seconds = time_sigma_seconds,
      chance_compactness = chance_compactness,
      size_bonus = size_bonus,
      bearing_weight = bearing_weight
    )
  )
}

save_app_loadable_groups <- function(result, file) {
  if (!is.list(result) || !"grouped_detections" %in% names(result)) {
    stop("result must be the list returned by group_gibbon_detections().", call. = FALSE)
  }
  groups <- result$grouped_detections
  removed_points <- empty_removed_detections()
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  save(groups, removed_points, file = file)
  invisible(file)
}

print_usage <- function() {
  cat(
    "Usage:\n",
    "  Rscript group_gibbon_detections.R --db-glob 'data/db/NCNX06*_database.sqlite3' --out detection_groups.RData\n",
    "  Rscript group_gibbon_detections.R --recordings data/gibbons/recordings.csv --mics data/gibbons/mic.csv --out detection_groups.RData\n",
    "\nOptions:\n",
    "  --db-glob PATH_GLOB       SQLite database glob. Default: data/db/NCNX06*_database.sqlite3\n",
    "  --recordings PATH         App-style recordings CSV.\n",
    "  --mics PATH               App-style mic CSV.\n",
    "  --out PATH                RData output path.\n",
    "  --csv-out PATH            Optional grouped detections CSV output path.\n",
    "  --max-span SECONDS        Candidate maximum temporal span. Default: 8\n",
    "  --solver auto|ompr|lpsolve Default: auto\n",
    "  --help                    Show this message.\n",
    sep = ""
  )
}

parse_cli_args <- function(args) {
  out <- list()
  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg == "--help") {
      out$help <- TRUE
      i <- i + 1L
    } else if (startsWith(arg, "--")) {
      key <- sub("^--", "", arg)
      if (i == length(args)) stop("Missing value for ", arg, call. = FALSE)
      out[[key]] <- args[[i + 1L]]
      i <- i + 2L
    } else {
      stop("Unexpected argument: ", arg, call. = FALSE)
    }
  }
  out
}

run_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  opts <- parse_cli_args(args)
  if (isTRUE(opts$help)) {
    print_usage()
    return(invisible(NULL))
  }
  source_repo_helpers()
  if (!is.null(opts$recordings)) {
    input <- read_grouping_csv(opts$recordings, opts$mics)
  } else {
    db_glob <- opts[["db-glob"]] %||% file.path("data", "db", "NCNX06*_database.sqlite3")
    db_paths <- Sys.glob(db_glob)
    if (length(db_paths) == 0) stop("No databases matched --db-glob: ", db_glob, call. = FALSE)
    input <- read_grouping_databases(db_paths)
  }
  result <- group_gibbon_detections(
    input$recData,
    input$micData,
    solver = opts$solver %||% "auto",
    max_group_span_seconds = as.numeric(opts[["max-span"]] %||% 8)
  )
  out_path <- opts$out %||% paste0("gibbon_detection_groups_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".RData")
  save_app_loadable_groups(result, out_path)
  csv_out <- opts[["csv-out"]]
  if (!is.null(csv_out)) {
    utils::write.csv(result$grouped_detections, csv_out, row.names = FALSE)
  }
  cat("Wrote ", out_path, "\n", sep = "")
  cat("Selected ", length(unique(result$group_membership$group_ID)), " group(s); left ",
      nrow(result$ungrouped_detections), " detection(s) ungrouped.\n", sep = "")
  invisible(result)
}

if (sys.nframe() == 0) {
  run_cli()
}
