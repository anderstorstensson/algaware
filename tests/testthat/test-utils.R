test_that("default_settings returns expected structure", {
  s <- algaware:::default_settings()
  expect_type(s, "list")
  expect_true("dashboard_url" %in% names(s))
  expect_true("dashboard_dataset" %in% names(s))
  expect_true("classification_path" %in% names(s))
  expect_true("raw_data_path" %in% names(s))
  expect_true("ferrybox_path" %in% names(s))
  expect_true("local_storage_path" %in% names(s))
  expect_true("db_folder" %in% names(s))
  expect_true("non_biological_classes" %in% names(s))
  expect_true("annotator" %in% names(s))
  expect_true("pixels_per_micron" %in% names(s))
  expect_true("n_mosaic_taxa" %in% names(s))
  expect_true("n_mosaic_images" %in% names(s))
})

test_that("load_settings returns defaults when no saved file", {
  withr::with_envvar(c(R_USER_CONFIG_DIR = tempdir()), {
    # Use a unique temp dir that has no settings file
    tmp <- file.path(tempdir(), paste0("algaware_test_", Sys.getpid()))
    withr::with_options(list(), {
      s <- load_settings()
      expect_type(s, "list")
      expect_true("dashboard_url" %in% names(s))
    })
  })
})

test_that("save_settings and load_settings roundtrip", {
  tmp_dir <- file.path(tempdir(), paste0("test_settings_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  settings_path <- file.path(tmp_dir, "settings.json")

  # Mock the settings path
  mockery::stub(save_settings, "get_settings_path", settings_path)
  mockery::stub(load_settings, "get_settings_path", settings_path)

  test_settings <- algaware:::default_settings()
  test_settings$dashboard_url <- "https://test.example.com"
  test_settings$annotator <- "test_user"

  save_settings(test_settings)
  loaded <- load_settings()

  expect_equal(loaded$dashboard_url, "https://test.example.com")
  expect_equal(loaded$annotator, "test_user")
})

test_that("parse_non_bio_classes splits correctly", {
  result <- algaware:::parse_non_bio_classes("detritus,Air_bubbles, Beads ,mix")
  expect_equal(result, c("detritus", "Air_bubbles", "Beads", "mix"))
})

test_that("parse_non_bio_classes handles empty string", {
  result <- algaware:::parse_non_bio_classes("")
  expect_length(result, 0)
})

test_that("parse_non_bio_classes handles single class", {
  result <- algaware:::parse_non_bio_classes("detritus")
  expect_equal(result, "detritus")
})

test_that("get_config_dir creates directory", {
  tmp <- file.path(tempdir(), paste0("config_test_", Sys.getpid()))
  mockery::stub(algaware:::get_config_dir, "tools::R_user_dir",
                file.path(tmp, "algaware"))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  result <- algaware:::get_config_dir()
  expect_true(dir.exists(result))
})

test_that("read_roi_dimensions returns empty data.frame for missing file", {
  result <- algaware:::read_roi_dimensions("/nonexistent/path.adc")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("roi_number", "width", "height", "roi_area"))
})

test_that("read_roi_dimensions reads CSV correctly", {
  tmp <- tempfile(fileext = ".adc")
  on.exit(unlink(tmp), add = TRUE)

  # ADC has many columns; V16 = width, V17 = height
  lines <- c(
    paste(c(rep(0, 15), 100, 50, rep(0, 3)), collapse = ","),
    paste(c(rep(0, 15), 200, 80, rep(0, 3)), collapse = ",")
  )
  writeLines(lines, tmp)

  result <- algaware:::read_roi_dimensions(tmp)
  expect_equal(nrow(result), 2)
  expect_equal(result$width, c(100, 200))
  expect_equal(result$height, c(50, 80))
  expect_equal(result$roi_area, c(5000, 16000))
  expect_equal(result$roi_number, c(1L, 2L))
})

test_that("load_settings merges saved values over defaults", {
  tmp_dir <- file.path(tempdir(), paste0("test_merge_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  settings_path <- file.path(tmp_dir, "settings.json")
  jsonlite::write_json(
    list(dashboard_url = "https://custom.example.com", annotator = "bob"),
    settings_path, auto_unbox = TRUE
  )

  mockery::stub(load_settings, "get_settings_path", settings_path)
  result <- load_settings()

  expect_equal(result$dashboard_url, "https://custom.example.com")
  expect_equal(result$annotator, "bob")
  # Defaults still present for unset keys

  expect_equal(result$pixels_per_micron, 2.77)
})

test_that("extra_stations survive a save/load roundtrip as a list of lists", {
  tmp_dir <- file.path(tempdir(), paste0("test_extra_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  settings_path <- file.path(tmp_dir, "settings.json")
  mockery::stub(save_settings, "get_settings_path", settings_path)
  mockery::stub(load_settings, "get_settings_path", settings_path)

  s <- algaware:::default_settings()
  s$extra_stations <- list(
    list(STATION_NAME = "BY31 LANDSORTSDJ", COAST = "EAST",
         STATION_NAME_SHORT = "BY31"),
    list(STATION_NAME = "SLÄGGÖ", COAST = "WEST",
         STATION_NAME_SHORT = "SLÄGGÖ")
  )
  save_settings(s)
  loaded <- load_settings()

  expect_false(is.data.frame(loaded$extra_stations))
  expect_length(loaded$extra_stations, 2)
  expect_equal(loaded$extra_stations[[1]]$STATION_NAME, "BY31 LANDSORTSDJ")
  expect_equal(loaded$extra_stations[[2]]$COAST, "WEST")

  # The loaded value must be consumable by load_algaware_stations()
  stations <- load_algaware_stations(loaded$extra_stations)
  expect_true("BY31 LANDSORTSDJ" %in% stations$STATION_NAME)
})

test_that("normalize_extra_stations recovers a data.frame shape", {
  df <- data.frame(STATION_NAME = c("A", "B"), COAST = c("EAST", "WEST"),
                   STATION_NAME_SHORT = c("A", "B"), stringsAsFactors = FALSE)
  result <- algaware:::normalize_extra_stations(df)
  expect_false(is.data.frame(result))
  expect_length(result, 2)
  expect_equal(result[[2]]$STATION_NAME, "B")

  expect_equal(algaware:::normalize_extra_stations(NULL), list())
  expect_equal(algaware:::normalize_extra_stations(list()), list())
})

test_that("load_settings handles corrupt JSON gracefully", {
  tmp_dir <- file.path(tempdir(), paste0("test_corrupt_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  settings_path <- file.path(tmp_dir, "settings.json")
  writeLines("not valid json{{{", settings_path)

  mockery::stub(load_settings, "get_settings_path", settings_path)
  expect_warning(result <- load_settings(), "Failed to load settings")
  expect_type(result, "list")
  expect_equal(result$dashboard_url, "")
})
