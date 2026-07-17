#!/usr/bin/env Rscript

# Standalone reproduction of the app's WAV-backed spectrogram rendering path.
# Run from the ecostats-rshiny folder with:
#   Rscript standalone_spectro.R

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(dplyr)
  library(magrittr)
})

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "."
script_dir <- normalizePath(dirname(script_path), mustWork = TRUE)
repo_root <- normalizePath(file.path(script_dir, ".."), mustWork = TRUE)

source(file.path(script_dir, "R", "fct_database_inputs.R"))
source(file.path(script_dir, "R", "fct_helpers.R"))

db_path <- file.path(repo_root, "data", "db", "NCNX06e_database.sqlite3")
wav_path <- "/Users/id52/Documents/Gibbon2026/20260710/Set1_wav/NCNX06e/R03260128220001.wav"
wav_root <- dirname(dirname(wav_path))
out_path <- file.path(script_dir, "standalone_spectro.png")
plot_freq_min_hz <- 100
plot_freq_max_hz <- 8000

if (!file.exists(db_path)) {
  stop("Database not found: ", db_path)
}
if (!file.exists(wav_path)) {
  stop("WAV file not found: ", wav_path)
}

db_data <- read_single_detection_database(db_path)

# parse_rec_data adds rec_id exactly as the app does before spectrogram caching.
rec_parsed <- parse_rec_data(db_data$recData)

first_detection <- rec_parsed %>%
  arrange(raw_toa, detection_id) %>%
  slice(1)

expected_wav_path <- resolve_detection_wav_path(first_detection, wav_root)
if (!identical(normalizePath(expected_wav_path, mustWork = FALSE), normalizePath(wav_path, mustWork = FALSE))) {
  stop(
    "The first detection resolved to a different WAV file.\n",
    "Resolved: ", expected_wav_path, "\n",
    "Expected: ", wav_path
  )
}

segment <- read_wav_pcm_segment(
  wav_path = wav_path,
  start_frame = first_detection$start_frame[[1]],
  end_frame = first_detection$end_frame[[1]]
)

write_spectrogram_png_inferno <- function(
    samples,
    sample_rate,
    out_path,
    detection_time,
    title,
    window_size = 1024,
    overlap = 0.75,
    freq_min_hz = 100,
    freq_max_hz = 8000
) {
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  hop <- max(1, as.integer(window_size * (1 - overlap)))
  if (length(samples) < window_size) {
    samples <- c(samples, rep(0, window_size - length(samples)))
  }
  starts <- seq(1, length(samples) - window_size + 1, by = hop)
  window <- 0.5 - 0.5 * cos(2 * pi * (seq_len(window_size) - 1) / (window_size - 1))
  spec <- vapply(starts, function(start) {
    chunk <- samples[start:(start + window_size - 1)] * window
    fft_values <- abs(stats::fft(chunk))[seq_len(window_size / 2)]
    20 * log10(fft_values + 1e-8)
  }, numeric(window_size / 2))

  freqs <- seq(0, sample_rate / 2, length.out = nrow(spec))
  keep <- freqs >= freq_min_hz & freqs <= freq_max_hz
  grDevices::png(out_path, width = 900, height = 320, bg = "white")
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mar = c(4, 4, 3, 1))
  plot_times <- seq_along(starts) * hop / sample_rate
  axis_at <- pretty(plot_times)
  axis_at <- axis_at[axis_at >= min(plot_times) & axis_at <= max(plot_times)]
  graphics::image(
    x = plot_times,
    y = freqs[keep] / 1000,
    z = t(spec[keep, , drop = FALSE]),
    col = hcl.colors(128, "Inferno"),
    xlab = "Time",
    ylab = "Frequency (kHz)",
    main = title,
    xaxt = "n",
    useRaster = TRUE
  )
  graphics::axis(
    side = 1,
    at = axis_at,
    labels = format(detection_time + axis_at, "%H:%M:%S", tz = "UTC")
  )
  invisible(out_path)
}

plot_title <- paste(
  first_detection$mic_id[[1]],
  format(first_detection$raw_toa[[1]], "%Y-%m-%d", tz = "UTC"),
  sep = " | "
)

write_spectrogram_png_inferno(
  samples = segment$samples,
  sample_rate = segment$sample_rate,
  out_path = out_path,
  detection_time = first_detection$raw_toa[[1]],
  title = plot_title,
  freq_min_hz = plot_freq_min_hz,
  freq_max_hz = plot_freq_max_hz
)

message("Detection metadata:")
print(first_detection %>%
  select(
    rec_id,
    detection_id,
    raw_toa,
    toa,
    Duration,
    wav_file,
    start_frame,
    end_frame,
    sample_rate
  ))
message("Wrote spectrogram: ", out_path)
