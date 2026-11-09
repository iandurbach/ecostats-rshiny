test_that("database readers use the preferred available detection table", {
  for (tables in list("Gibbon_A", "Gibbon_Detector",
                      c("Gibbon_A", "Gibbon_Detector"))) {
    db_path <- tempfile(fileext = ".sqlite3")
    conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    tryCatch({
      for (table in tables) {
        DBI::dbWriteTable(conn, table, data.frame(
          Id = 1L, UTC = "2026-01-28 22:58:02", Duration = 1,
          BearingAngle1 = 0, Call_Type = table
        ))
      }
      DBI::dbWriteTable(conn, "Trex_Location", data.frame(Longitude = 20, Latitude = -30))
      DBI::dbWriteTable(conn, "Trex_Orientation", data.frame(True_Head = 0))
      DBI::dbWriteTable(conn, "Trex_Time_Offset", data.frame(
        UTC = "2026-01-28 22:58:00", TimeOffset = 0
      ))
      DBI::dbWriteTable(conn, "Sound_Acquisition", data.frame(
        UTC = c("2026-01-28 22:58:00", "2026-01-28 22:59:00"),
        Status = c("Start", "Stop"), SystemName = "test.wav",
        Samples = c(0, 48000 * 60), sampleRate = 48000
      ))

      expected <- if ("Gibbon_Detector" %in% tables) "Gibbon_Detector" else tables[[1]]
      expect_equal(resolve_detection_table(conn), expected)
      expect_equal(read_database_call_types(db_path), expected)
      out <- read_single_detection_database(db_path)
      expect_equal(out$recData$detection_table, expected)
      expect_equal(out$recData$call_type, expected)
      expect_equal(out$recData$wav_file, "test.wav")
    }, finally = {
      DBI::dbDisconnect(conn)
      unlink(db_path)
    })
  }
})

test_that("recorder id and cluster id are parsed from database paths", {
  expect_equal(parse_recorder_id_from_database_path("NCNX06a_database.sqlite3"), "NCNX06a")
  expect_equal(parse_cluster_id_from_mic_id("NCNX06a"), "06")
})

test_that("database ingestion returns app-compatible mic and recording data", {
  candidates <- c(
    file.path(getwd(), "..", "data", "db", "NCNX06a_database.sqlite3"),
    file.path(getwd(), "..", "..", "data", "db", "NCNX06a_database.sqlite3"),
    test_path("..", "..", "..", "data", "db", "NCNX06a_database.sqlite3")
  )
  existing <- candidates[file.exists(candidates)]
  skip_if(length(existing) == 0)
  db_path <- existing[[1]]
  skip_if_not(file.exists(db_path))

  out <- read_single_detection_database(db_path, offset_sign = 1)

  expect_named(out, c("micData", "recData"))
  expect_true(all(c("mic_id", "lng", "lat") %in% names(out$micData)))
  expect_true(all(c("recording_ID", "mic_ID", "GPSDatetime2", "measured_bearing", "measured_gender", "spectrogram") %in% names(out$recData)))
  expect_true(all(c("wav_file", "start_frame", "end_frame", "clock_offset", "call_type") %in% names(out$recData)))
  expect_equal(out$micData$mic_id[[1]], "NCNX06a")
  expect_true(nrow(out$recData) > 0)
  expect_true(all(out$recData$measured_bearing >= 0 & out$recData$measured_bearing < 360))
})

test_that("parse_rec_data preserves database backend metadata", {
  recordings <- data.frame(
    recording_ID = "NCNX06a_1",
    mic_ID = "NCNX06a",
    GPSDatetime2 = "2026-01-28 22:58:02.777",
    measured_bearing = 10,
    measured_gender = NA_character_,
    spectrogram = "NCNX06a_1.png",
    wav_file = "R08260128220001.wav",
    start_frame = 100,
    end_frame = 200,
    clock_offset = 0.174,
    stringsAsFactors = FALSE
  )

  parsed <- parse_rec_data(recordings)

  expect_true(all(c("wav_file", "start_frame", "end_frame", "clock_offset") %in% names(parsed)))
  expect_equal(parsed$rec_id[[1]], "NCNX06a_1")
  expect_equal(parsed$mic_id[[1]], "NCNX06a")
})

test_that("database call types are read as distinct sorted choices", {
  candidates <- c(
    file.path(getwd(), "..", "data", "db", "NCNX06a_database.sqlite3"),
    file.path(getwd(), "..", "..", "data", "db", "NCNX06a_database.sqlite3"),
    test_path("..", "..", "..", "data", "db", "NCNX06a_database.sqlite3")
  )
  existing <- candidates[file.exists(candidates)]
  skip_if(length(existing) == 0)

  call_types <- read_database_call_types(existing[[1]])
  expect_true("Female" %in% call_types)
  expect_equal(call_types, sort(unique(call_types)))
})

test_that("parse_rec_data creates unique internal rec ids for repeated recording ids", {
  recordings <- data.frame(
    recording_ID = c(1, 1, 2),
    mic_ID = c(11, 12, 13),
    GPSDatetime2 = c("31/01/2024 23:11", "31/01/2024 23:11", "31/01/2024 23:12"),
    measured_bearing = c(5.7, 0.2, 3.1),
    measured_gender = c("F", "F", "F"),
    spectrogram = c("a.jpeg", "b.jpeg", "c.jpeg"),
    stringsAsFactors = FALSE
  )

  parsed <- parse_rec_data(recordings)

  expect_equal(parsed$recording_ID, c(1, 1, 2))
  expect_equal(parsed$rec_id, c("1", "1_1", "2"))
  expect_equal(anyDuplicated(parsed$rec_id), 0L)
})

test_that("WAV path resolution uses wav root, mic id, and recording filename", {
  row <- data.frame(
    mic_id = "NCNX06a",
    wav_file = "R08260128220001.wav",
    stringsAsFactors = FALSE
  )

  expect_equal(
    resolve_detection_wav_path(row, "/tmp/Set1_wav"),
    file.path("/tmp/Set1_wav", "NCNX06a", "R08260128220001.wav")
  )
})

test_that("legacy JPEG spectrograms encode with the correct MIME type", {
  candidates <- c(
    file.path(getwd(), "..", "data", "nepaltest", "spectros.zip"),
    file.path(getwd(), "..", "..", "data", "nepaltest", "spectros.zip"),
    test_path("..", "..", "..", "data", "nepaltest", "spectros.zip")
  )
  existing <- candidates[file.exists(candidates)]
  skip_if(length(existing) == 0)
  spectro_zip <- existing[[1]]

  tmp <- tempfile("nepal_spectros_")
  dir.create(tmp)
  unzip(spectro_zip, exdir = tmp)
  img_path <- file.path(tmp, "20240131_231129_r16_Det001.jpeg")

  expect_true(file.exists(img_path))
  expect_match(encode_image(img_path), "^data:image/jpeg;base64,")
})
