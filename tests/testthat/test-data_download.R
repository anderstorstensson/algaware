test_that("filter_metadata by cruise", {
  metadata <- data.frame(
    pid = c("s1", "s2", "s3"),
    cruise = c("C001", "C001", "C002"),
    sample_time = as.POSIXct(c("2022-01-01", "2022-01-02", "2022-01-03")),
    stringsAsFactors = FALSE
  )

  result <- filter_metadata(metadata, cruise = "C001")
  expect_equal(nrow(result), 2)
  expect_true(all(result$cruise == "C001"))
})

test_that("filter_metadata by date range", {
  metadata <- data.frame(
    pid = c("s1", "s2", "s3"),
    sample_time = as.POSIXct(c("2022-01-01", "2022-01-15", "2022-02-01")),
    stringsAsFactors = FALSE
  )

  result <- filter_metadata(metadata, date_from = "2022-01-10",
                             date_to = "2022-01-20")
  expect_equal(nrow(result), 1)
  expect_equal(result$pid, "s2")
})

test_that("filter_metadata returns all when no filters", {
  metadata <- data.frame(
    pid = c("s1", "s2"),
    sample_time = as.POSIXct(c("2022-01-01", "2022-01-02")),
    stringsAsFactors = FALSE
  )

  result <- filter_metadata(metadata)
  expect_equal(nrow(result), 2)
})

test_that("filter_metadata handles empty cruise string", {
  metadata <- data.frame(
    pid = c("s1", "s2"),
    cruise = c("C001", "C002"),
    sample_time = as.POSIXct(c("2022-01-01", "2022-01-02")),
    stringsAsFactors = FALSE
  )

  result <- filter_metadata(metadata, cruise = "")
  expect_equal(nrow(result), 2)
})

test_that("read_h5_classifications returns empty df for empty dir", {
  tmp_dir <- file.path(tempdir(), paste0("h5_empty_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  result <- read_h5_classifications(tmp_dir)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(names(result),
               c("sample_name", "roi_number", "class_name", "score",
                 "cell_count"))
})

test_that("download_raw_data creates dest_dir", {
  tmp_dir <- file.path(tempdir(), paste0("raw_test_", Sys.getpid()))
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  mockery::stub(download_raw_data, "iRfcb::ifcb_download_dashboard_data", NULL)

  download_raw_data("https://example.com",
                    c("D20220101T000000_IFCB134"), tmp_dir)
  expect_true(dir.exists(tmp_dir))
})

test_that("download_raw_data skips existing files", {
  tmp_dir <- file.path(tempdir(), paste0("raw_skip_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Create a complete existing file set (.roi/.adc/.hdr)
  for (ext in c("roi", "adc", "hdr")) {
    writeLines("", file.path(tmp_dir,
                             paste0("D20220101T000000_IFCB134.", ext)))
  }

  callback_called <- FALSE
  callback <- function(current, total, msg) {
    callback_called <<- TRUE
  }

  download_raw_data("https://example.com",
                    c("D20220101T000000_IFCB134"), tmp_dir,
                    progress_callback = callback)
  expect_true(callback_called)
})

test_that("download_raw_data rejects invalid sample IDs", {
  tmp_dir <- file.path(tempdir(), paste0("raw_invalid_", Sys.getpid()))
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  expect_error(
    download_raw_data("https://example.com", c("../evil"), tmp_dir),
    "Invalid IFCB sample IDs"
  )
})

test_that("download_features skips existing files", {
  tmp_dir <- file.path(tempdir(), paste0("feat_skip_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  writeLines("", file.path(tmp_dir, "D20220101T000000_IFCB134.csv"))

  callback_msgs <- character(0)
  callback <- function(current, total, msg) {
    callback_msgs <<- c(callback_msgs, msg)
  }

  download_features("https://example.com",
                    c("D20220101T000000_IFCB134"), tmp_dir,
                    progress_callback = callback)
  expect_true(any(grepl("already downloaded", callback_msgs)))
})

test_that("copy_classification_files skips existing files", {
  tmp_src <- file.path(tempdir(), paste0("class_src_", Sys.getpid()))
  tmp_dest <- file.path(tempdir(), paste0("class_dest_", Sys.getpid()))
  dir.create(tmp_dest, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(c(tmp_src, tmp_dest), recursive = TRUE), add = TRUE)

  writeLines("", file.path(tmp_dest, "D20220101T000000_IFCB134_class.h5"))

  callback_msgs <- character(0)
  callback <- function(current, total, msg) {
    callback_msgs <<- c(callback_msgs, msg)
  }

  copy_classification_files(
    tmp_src,
    c("D20220101T000000_IFCB134"),
    tmp_dest,
    progress_callback = callback
  )
  expect_true(any(grepl("already copied", callback_msgs)))
})

test_that("validate_sample_ids accepts valid IDs", {
  expect_invisible(algaware:::validate_sample_ids(
    c("D20220101T000000_IFCB134", "D20231215T123456_IFCB123")
  ))
})

test_that("validate_sample_ids rejects invalid IDs", {
  expect_error(
    algaware:::validate_sample_ids(c("bad_id")),
    "Invalid IFCB sample IDs"
  )
})

test_that("validate_sample_ids accepts empty input", {
  expect_invisible(algaware:::validate_sample_ids(character(0)))
})

test_that("read_classifier_name returns NULL for empty dir", {
  tmp_dir <- file.path(tempdir(), paste0("h5_class_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  result <- read_classifier_name(tmp_dir)
  expect_null(result)
})

test_that("download_features creates dest_dir", {
  tmp_dir <- file.path(tempdir(), paste0("feat_test_", Sys.getpid()))
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  mockery::stub(download_features, "iRfcb::ifcb_download_dashboard_data", NULL)

  download_features("https://example.com",
                     c("D20220101T000000_IFCB134"), tmp_dir)
  expect_true(dir.exists(tmp_dir))
})

test_that("download_raw_data calls progress for all-skipped", {
  tmp_dir <- file.path(tempdir(), paste0("raw_allskip_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # A sample only counts as downloaded when .roi, .adc AND .hdr are present
  for (pid in c("D20220101T000000_IFCB134", "D20220101T000001_IFCB134")) {
    for (ext in c("roi", "adc", "hdr")) {
      writeLines("", file.path(tmp_dir, paste0(pid, ".", ext)))
    }
  }

  msgs <- character(0)
  callback <- function(current, total, msg) msgs <<- c(msgs, msg)

  download_raw_data("https://example.com",
                    c("D20220101T000000_IFCB134", "D20220101T000001_IFCB134"),
                    tmp_dir, progress_callback = callback)
  expect_true(any(grepl("already downloaded", msgs)))
})

test_that("download_raw_data retries samples with incomplete file sets", {
  tmp_dir <- file.path(tempdir(), paste0("raw_partial_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Regression: .roi present but .hdr/.adc missing (interrupted download)
  # used to be treated as complete, so ml_analyzed stayed unavailable.
  writeLines("", file.path(tmp_dir, "D20220101T000000_IFCB134.roi"))

  requested <- NULL
  mockery::stub(download_raw_data, "iRfcb::ifcb_download_dashboard_data",
                function(dashboard_url, samples, ...) requested <<- samples)

  download_raw_data("https://example.com",
                    c("D20220101T000000_IFCB134"), tmp_dir)
  expect_equal(requested, "D20220101T000000_IFCB134")
})

test_that("download_features recognises suffixed feature files as downloaded", {
  tmp_dir <- file.path(tempdir(), paste0("feat_suffix_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Regression: the "_features"/"_fea_vN" suffix meant the existing-file
  # check never matched the bare sample IDs, re-downloading every reload.
  writeLines("", file.path(tmp_dir, "D20220101T000000_IFCB134_features.csv"))
  writeLines("", file.path(tmp_dir, "D20220101T000001_IFCB134_fea_v2.csv"))

  msgs <- character(0)
  callback <- function(current, total, msg) msgs <<- c(msgs, msg)

  download_features("https://example.com",
                    c("D20220101T000000_IFCB134", "D20220101T000001_IFCB134"),
                    tmp_dir, progress_callback = callback)
  expect_true(any(grepl("already downloaded", msgs)))
})

test_that("download_features calls progress for all-skipped", {
  tmp_dir <- file.path(tempdir(), paste0("feat_allskip_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  writeLines("", file.path(tmp_dir, "D20220101T000000_IFCB134.csv"))

  msgs <- character(0)
  callback <- function(current, total, msg) msgs <<- c(msgs, msg)

  download_features("https://example.com",
                    c("D20220101T000000_IFCB134"),
                    tmp_dir, progress_callback = callback)
  expect_true(any(grepl("already downloaded", msgs)))
})

test_that("copy_classification_files returns empty for all-skipped", {
  tmp_dir <- file.path(tempdir(), paste0("class_allskip_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  writeLines("", file.path(tmp_dir, "D20220101T000000_IFCB134_class.h5"))

  msgs <- character(0)
  callback <- function(current, total, msg) msgs <<- c(msgs, msg)

  result <- copy_classification_files(
    "/some/src", c("D20220101T000000_IFCB134"), tmp_dir,
    progress_callback = callback
  )
  expect_true(any(grepl("already copied", msgs)))
})

test_that("copy_classification_files handles missing source file", {
  tmp_src <- file.path(tempdir(), paste0("class_missing_", Sys.getpid()))
  tmp_dest <- file.path(tempdir(), paste0("class_dest_m_", Sys.getpid()))
  dir.create(tmp_src, recursive = TRUE, showWarnings = FALSE)
  dir.create(tmp_dest, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(c(tmp_src, tmp_dest), recursive = TRUE), add = TRUE)

  result <- copy_classification_files(
    tmp_src, c("D20220101T000000_IFCB134"), tmp_dest
  )
  # No h5 file found, so nothing copied
  expect_false(file.exists(
    file.path(tmp_dest, "D20220101T000000_IFCB134_class.h5")
  ))
})

test_that("copy_classification_files finds files in yearly subdirs", {
  tmp_src <- file.path(tempdir(), paste0("class_src2_", Sys.getpid()))
  tmp_dest <- file.path(tempdir(), paste0("class_dest2_", Sys.getpid()))
  year_dir <- file.path(tmp_src, "class2022_v3")
  dir.create(year_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(tmp_dest, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(c(tmp_src, tmp_dest), recursive = TRUE), add = TRUE)

  h5_file <- file.path(year_dir, "D20220101T000000_IFCB134_class.h5")
  writeLines("fake h5", h5_file)

  result <- copy_classification_files(
    tmp_src,
    c("D20220101T000000_IFCB134"),
    tmp_dest
  )

  expect_true(file.exists(
    file.path(tmp_dest, "D20220101T000000_IFCB134_class.h5")
  ))
})

test_that("resolve_classification_path returns normalized existing dir", {
  tmp <- tempdir()
  result <- algaware:::resolve_classification_path(tmp)
  expect_true(dir.exists(result))
})

test_that("resolve_classification_path returns input for non-existent path", {
  result <- algaware:::resolve_classification_path("/nonexistent/path/xyz")
  expect_type(result, "character")
  expect_true(nzchar(result))
})

test_that("resolve_classification_path returns empty string for empty input", {
  result <- algaware:::resolve_classification_path("")
  expect_equal(result, "")
})

test_that("resolve_classification_path converts backslashes to forward slashes", {
  tmp <- tempdir()
  # Build a path with backslashes pointing to the same real dir
  backslash_path <- gsub("/", "\\\\", tmp)
  result <- algaware:::resolve_classification_path(backslash_path)
  expect_false(grepl("\\\\", result))
})

test_that("resolve_classification_path strips trailing slash", {
  tmp <- tempdir()
  result <- algaware:::resolve_classification_path(paste0(tmp, "/"))
  expect_false(endsWith(result, "/"))
})

test_that("resolve_classification_path strips leading/trailing whitespace", {
  tmp <- tempdir()
  result <- algaware:::resolve_classification_path(paste0("  ", tmp, "  "))
  expect_true(dir.exists(result))
})

test_that("download_raw_data triggers progress callbacks for new download", {
  tmp_dir <- file.path(tempdir(), paste0("raw_progress_", Sys.getpid()))
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  msgs <- character(0)
  callback <- function(current, total, msg) msgs <<- c(msgs, msg)

  mockery::stub(download_raw_data, "iRfcb::ifcb_download_dashboard_data", NULL)

  download_raw_data("https://example.com",
                    c("D20220101T000000_IFCB134"), tmp_dir,
                    progress_callback = callback)
  expect_true(any(grepl("Downloading", msgs)))
  expect_true(any(grepl("downloaded", msgs)))
})

test_that("download_features triggers progress callbacks for new download", {
  tmp_dir <- file.path(tempdir(), paste0("feat_progress_", Sys.getpid()))
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  msgs <- character(0)
  callback <- function(current, total, msg) msgs <<- c(msgs, msg)

  mockery::stub(download_features, "iRfcb::ifcb_download_dashboard_data", NULL)

  download_features("https://example.com",
                    c("D20220101T000000_IFCB134"), tmp_dir,
                    progress_callback = callback)
  expect_true(any(grepl("Downloading", msgs)))
  expect_true(any(grepl("downloaded", msgs)))
})

test_that("copy_classification_files triggers progress callbacks when copying", {
  tmp_src <- file.path(tempdir(), paste0("ccp_src_", Sys.getpid()))
  tmp_dest <- file.path(tempdir(), paste0("ccp_dest_", Sys.getpid()))
  year_dir <- file.path(tmp_src, "class2022_v3")
  dir.create(year_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(tmp_dest, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(c(tmp_src, tmp_dest), recursive = TRUE), add = TRUE)
  writeLines("fake h5", file.path(year_dir, "D20220101T000000_IFCB134_class.h5"))

  msgs <- character(0)
  callback <- function(current, total, msg) msgs <<- c(msgs, msg)

  copy_classification_files(
    tmp_src, c("D20220101T000000_IFCB134"), tmp_dest,
    progress_callback = callback
  )
  expect_true(any(grepl("Copying|ready", msgs)))
})

test_that("read_h5_classifications reads real H5 file correctly", {
  h5_path <- testthat::test_path("test_data",
                                  "D20250714T110535_IFCB134_class.h5")
  skip_if_not(file.exists(h5_path), "Test H5 file not available")
  skip_if_not_installed("hdf5r")

  tmp_dir <- file.path(tempdir(), paste0("h5_real_", Sys.getpid()))
  dir.create(tmp_dir, showWarnings = FALSE)
  file.copy(h5_path, tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  result <- read_h5_classifications(tmp_dir)
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_equal(names(result),
               c("sample_name", "roi_number", "class_name", "score",
                 "cell_count"))
  expect_equal(unique(result$sample_name), "D20250714T110535_IFCB134")
  expect_type(result$roi_number, "integer")
  expect_type(result$score, "double")
  # The real fixture predates the chain counter: the column must exist and be
  # all NA, never dropped or zero-filled.
  expect_true(all(is.na(result$cell_count)))
})

test_that("read_h5_classifications filters by sample_ids", {
  h5_path <- testthat::test_path("test_data",
                                  "D20250714T110535_IFCB134_class.h5")
  skip_if_not(file.exists(h5_path), "Test H5 file not available")
  skip_if_not_installed("hdf5r")

  tmp_dir <- file.path(tempdir(), paste0("h5_filter_", Sys.getpid()))
  dir.create(tmp_dir, showWarnings = FALSE)
  file.copy(h5_path, tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  result_all <- read_h5_classifications(tmp_dir)
  result_filtered <- read_h5_classifications(tmp_dir,
                                              sample_ids = "D20250714T110535_IFCB134")
  result_none <- read_h5_classifications(tmp_dir,
                                          sample_ids = "D99991231T999999_IFCB999")

  expect_equal(nrow(result_all), nrow(result_filtered))
  expect_equal(nrow(result_none), 0)
})

test_that("read_classifier_name reads classifier name from H5 file", {
  h5_path <- testthat::test_path("test_data",
                                  "D20250714T110535_IFCB134_class.h5")
  skip_if_not(file.exists(h5_path), "Test H5 file not available")
  skip_if_not_installed("hdf5r")

  tmp_dir <- file.path(tempdir(), paste0("h5_clf_", Sys.getpid()))
  dir.create(tmp_dir, showWarnings = FALSE)
  file.copy(h5_path, tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  result <- read_classifier_name(tmp_dir)
  expect_type(result, "character")
  expect_true(nzchar(result))
  expect_match(result, "ResNet50|SMHI", ignore.case = TRUE)
})

test_that("read_h5_classifications warns and skips invalid h5 files", {
  tmp_dir <- file.path(tempdir(), paste0("h5_bad_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Create a fake .h5 file that is not a valid HDF5 file
  writeLines("not a real h5 file", file.path(tmp_dir, "D20220101T000000_IFCB134_class.h5"))

  expect_warning(
    result <- read_h5_classifications(tmp_dir),
    "Failed to read H5"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("read_h5_classifications reads the optional cell_count dataset", {
  skip_if_not_installed("hdf5r")

  tmp_dir <- file.path(tempdir(), paste0("h5_cells_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  write_test_class_h5(tmp_dir, "D20250101T000000_IFCB134",
                      class_names = c("Pseudo-nitzschia_spp",
                                      "Skeletonema_marinoi"),
                      cell_count = c(6L, -1L))

  result <- read_h5_classifications(tmp_dir)
  expect_equal(nrow(result), 2)
  expect_equal(result$cell_count, c(6L, -1L))
  expect_equal(result$class_name,
               c("Pseudo-nitzschia_spp", "Skeletonema_marinoi"))
})

test_that("read_h5_classifications keeps pre-YOLO files with NA cell_count", {
  skip_if_not_installed("hdf5r")

  tmp_dir <- file.path(tempdir(), paste0("h5_noyolo_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  write_test_class_h5(tmp_dir, "D20250101T000000_IFCB134",
                      class_names = c("Skeletonema_marinoi",
                                      "Dinophysis_acuta"))

  # A file without the dataset must be read in full, without the "Failed to
  # read H5 file" warning that would silently drop it from the dataset.
  expect_no_warning(result <- read_h5_classifications(tmp_dir))
  expect_equal(nrow(result), 2)
  expect_true(all(is.na(result$cell_count)))
})

test_that("read_h5_classifications combines chain-counted and pre-YOLO files", {
  skip_if_not_installed("hdf5r")

  tmp_dir <- file.path(tempdir(), paste0("h5_mixed_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  write_test_class_h5(tmp_dir, "D20250101T000000_IFCB134",
                      class_names = c("Pseudo-nitzschia_spp",
                                      "Chaetoceros_spp_chain"),
                      cell_count = c(6L, 8L))
  write_test_class_h5(tmp_dir, "D20240101T000000_IFCB134",
                      class_names = c("Pseudo-nitzschia_spp",
                                      "Dinophysis_acuta"))

  result <- read_h5_classifications(tmp_dir)
  expect_equal(nrow(result), 4)
  new_rows <- result$sample_name == "D20250101T000000_IFCB134"
  expect_equal(result$cell_count[new_rows], c(6L, 8L))
  expect_true(all(is.na(result$cell_count[!new_rows])))
})

test_that("copy_classification_files works when root is already a year dir", {
  tmp_src <- file.path(tempdir(), paste0("class_src_yearroot_", Sys.getpid()))
  tmp_dest <- file.path(tempdir(), paste0("class_dest_yearroot_", Sys.getpid()))
  dir.create(tmp_src, recursive = TRUE, showWarnings = FALSE)
  dir.create(tmp_dest, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(c(tmp_src, tmp_dest), recursive = TRUE), add = TRUE)

  # Simulate user selecting "class2026_v3" directly in settings.
  year_root <- file.path(tmp_src, "class2026_v3")
  dir.create(year_root, recursive = TRUE, showWarnings = FALSE)
  writeLines("fake h5",
             file.path(year_root, "D20260107T222955_IFCB134_class.h5"))

  copy_classification_files(
    year_root,
    c("D20260107T222955_IFCB134"),
    tmp_dest
  )

  expect_true(file.exists(
    file.path(tmp_dest, "D20260107T222955_IFCB134_class.h5")
  ))
})

test_that("filter_metadata errors when cruise column is missing", {
  # Regression: a requested cruise against metadata without a cruise column
  # silently returned the entire unfiltered dataset.
  metadata <- data.frame(pid = c("a", "b"), stringsAsFactors = FALSE)
  expect_error(filter_metadata(metadata, cruise = "C001"), "no cruise column")
})

test_that("filter_metadata drops NA cruise and NA date rows", {
  metadata <- data.frame(
    pid = c("a", "b", "c"),
    cruise = c("C001", NA, "C002"),
    sample_time = as.POSIXct(c("2022-01-05 10:00", NA, "2022-01-20 10:00"),
                             tz = "UTC"),
    stringsAsFactors = FALSE
  )
  by_cruise <- filter_metadata(metadata, cruise = "C001")
  expect_equal(by_cruise$pid, "a")

  by_date <- filter_metadata(metadata, date_from = "2022-01-01",
                             date_to = "2022-01-31")
  expect_equal(by_date$pid, c("a", "c"))
})
