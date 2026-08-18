# Tests for R/ctd_plots.R

# -- deduplicate_casts ---------------------------------------------------------

test_that("deduplicate_casts returns input unchanged for NULL or 0 rows", {
  expect_null(algaware:::deduplicate_casts(NULL))

  empty <- data.frame(file_path = character(0), pressure_dbar = numeric(0),
                      sample_date = as.Date(character(0)),
                      stringsAsFactors = FALSE)
  result <- algaware:::deduplicate_casts(empty)
  expect_equal(nrow(result), 0)
})

test_that("deduplicate_casts drops upcast files", {
  ctd_data <- data.frame(
    file_path = c("uABC.cnv", "downcast.cnv"),
    pressure_dbar = c(10, 10),
    sample_date = as.Date(c("2024-06-01", "2024-06-01")),
    stringsAsFactors = FALSE
  )
  result <- algaware:::deduplicate_casts(ctd_data)
  expect_false(any(grepl("^uABC", basename(result$file_path))))
  expect_true("downcast.cnv" %in% result$file_path)
})

test_that("deduplicate_casts keeps deepest cast per date", {
  ctd_data <- data.frame(
    file_path = c(rep("cast1.cnv", 3), rep("cast2.cnv", 3)),
    pressure_dbar = c(5, 10, 15, 5, 10, 50),
    sample_date = as.Date(rep("2024-06-01", 6)),
    stringsAsFactors = FALSE
  )
  result <- algaware:::deduplicate_casts(ctd_data)
  expect_true(all(result$file_path == "cast2.cnv"))
})

test_that("deduplicate_casts keeps separate casts on different dates", {
  ctd_data <- data.frame(
    file_path = c(rep("cast1.cnv", 2), rep("cast2.cnv", 2)),
    pressure_dbar = c(5, 20, 5, 20),
    sample_date = as.Date(c("2024-06-01", "2024-06-01",
                             "2024-06-15", "2024-06-15")),
    stringsAsFactors = FALSE
  )
  result <- algaware:::deduplicate_casts(ctd_data)
  expect_equal(length(unique(result$file_path)), 2)
})

# -- create_fluorescence_profile -----------------------------------------------

test_that("create_fluorescence_profile returns a ggplot", {
  skip_if_not_installed("ggplot2")

  station_ctd <- data.frame(
    chl_fluorescence = c(1.0, 2.0, 3.0, 1.5),
    pressure_dbar = c(5, 10, 20, 40),
    file_path = c("a.cnv", "a.cnv", "a.cnv", "a.cnv"),
    stringsAsFactors = FALSE
  )
  p <- algaware:::create_fluorescence_profile(station_ctd, "BY5")
  expect_s3_class(p, "ggplot")
})

test_that("create_fluorescence_profile clamps depth to 50 m", {
  skip_if_not_installed("ggplot2")

  station_ctd <- data.frame(
    chl_fluorescence = c(1.0, 2.0, 5.0),
    pressure_dbar = c(10, 30, 100),
    file_path = rep("a.cnv", 3),
    stringsAsFactors = FALSE
  )
  p <- algaware:::create_fluorescence_profile(station_ctd, "BY5")
  # Only 2 rows should be in the underlying data (<=50 m)
  expect_s3_class(p, "ggplot")
  plot_data <- p$data
  expect_true(all(plot_data$pressure_dbar <= 50))
})

test_that("create_fluorescence_profile accepts xlim and date_label", {
  skip_if_not_installed("ggplot2")

  station_ctd <- data.frame(
    chl_fluorescence = c(1.0, 2.0),
    pressure_dbar = c(5, 15),
    file_path = rep("a.cnv", 2),
    stringsAsFactors = FALSE
  )
  p <- algaware:::create_fluorescence_profile(station_ctd, "BY5",
                                               xlim = c(0, 10),
                                               date_label = "2024-06-01",
                                               show_x_axis = FALSE)
  expect_s3_class(p, "ggplot")
})

# -- create_chl_timeseries -----------------------------------------------------

test_that("create_chl_timeseries returns NULL when both inputs are NULL", {
  result <- algaware:::create_chl_timeseries(NULL, NULL, "BY5", 2024)
  expect_null(result)
})

test_that("create_chl_timeseries returns ggplot from stats only", {
  skip_if_not_installed("ggplot2")

  stats_df <- data.frame(
    MONTH = 1:12,
    CHLA_mean = c(1.5, 1.8, 2.5, 3.0, 2.8, 2.0, 1.5, 1.6, 1.8, 2.0, 1.9, 1.7),
    CHLA_std = rep(0.3, 12),
    CHLA_n = rep(10L, 12),
    stringsAsFactors = FALSE
  )
  p <- algaware:::create_chl_timeseries(NULL, stats_df, "BY5", 2024)
  expect_s3_class(p, "ggplot")
})

test_that("create_chl_timeseries returns ggplot from lims only", {
  skip_if_not_installed("ggplot2")

  lims_df <- data.frame(
    sample_date = as.Date(c("2024-03-15", "2024-06-20")),
    CPHL = c(2.5, 1.8),
    stringsAsFactors = FALSE
  )
  p <- algaware:::create_chl_timeseries(lims_df, NULL, "BY5", 2024)
  expect_s3_class(p, "ggplot")
})

test_that("create_chl_timeseries returns ggplot with both stats and lims", {
  skip_if_not_installed("ggplot2")

  stats_df <- data.frame(
    MONTH = 1:12,
    CHLA_mean = rep(2.0, 12),
    CHLA_std = rep(0.4, 12),
    CHLA_n = rep(8L, 12),
    stringsAsFactors = FALSE
  )
  lims_df <- data.frame(
    sample_date = as.Date("2024-04-10"),
    CPHL = 3.1,
    stringsAsFactors = FALSE
  )
  p <- algaware:::create_chl_timeseries(lims_df, stats_df, "BY5", 2024,
                                         show_x_axis = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("deduplicate_casts keeps casts whose sample_date is NA", {
  # Regression: split() drops NA groups, so casts without a parsable date
  # were all discarded and the station silently vanished.
  ctd <- data.frame(
    file_path = rep(c("a.cnv", "b.cnv"), each = 2),
    pressure_dbar = c(1, 30, 1, 20),
    sample_date = as.Date(NA),
    stringsAsFactors = FALSE
  )
  result <- algaware:::deduplicate_casts(ctd)
  # Each NA-date file is its own group, so both files survive
  expect_setequal(unique(result$file_path), c("a.cnv", "b.cnv"))

  # Mixed NA and real dates: real-date files still deduplicate per date
  ctd$sample_date <- as.Date(c(NA, NA, "2026-08-01", "2026-08-01"))
  result <- algaware:::deduplicate_casts(ctd)
  expect_setequal(unique(result$file_path), c("a.cnv", "b.cnv"))
})

test_that("deduplicate_casts tolerates all-NA pressure for a file", {
  ctd <- data.frame(
    file_path = rep(c("a.cnv", "b.cnv"), each = 2),
    pressure_dbar = c(NA, NA, 1, 20),
    sample_date = as.Date("2026-08-01"),
    stringsAsFactors = FALSE
  )
  expect_no_warning(result <- algaware:::deduplicate_casts(ctd))
  # The file with real depths wins the per-date dedup
  expect_equal(unique(result$file_path), "b.cnv")
})

# -- compute_profile_xlim ------------------------------------------------------

test_that("compute_profile_xlim uses fixed 0-10 scale for typical values", {
  ctd <- data.frame(
    chl_fluorescence = c(1.2, 4.5, 8.9),
    pressure_dbar    = c(5, 10, 20),
    sample_date      = as.Date("2026-04-01")
  )
  expect_equal(algaware:::compute_profile_xlim(ctd), c(0, 10))
})

test_that("compute_profile_xlim grows dynamically when CTD exceeds 10", {
  ctd <- data.frame(
    chl_fluorescence = c(2, 14),
    pressure_dbar    = c(5, 10),
    sample_date      = as.Date("2026-04-01")
  )
  expect_equal(algaware:::compute_profile_xlim(ctd), c(0, 14 * 1.05))
})

test_that("compute_profile_xlim ignores CTD values deeper than 50 m", {
  ctd <- data.frame(
    chl_fluorescence = c(3, 25),
    pressure_dbar    = c(5, 60),
    sample_date      = as.Date("2026-04-01")
  )
  expect_equal(algaware:::compute_profile_xlim(ctd), c(0, 10))
})

test_that("compute_profile_xlim includes same-cruise bottle CPHL", {
  ctd <- data.frame(
    chl_fluorescence = c(2, 3),
    pressure_dbar    = c(5, 10),
    sample_date      = as.Date("2026-04-01")
  )
  lims <- data.frame(
    CPHL        = c(12, 40),
    DEPH        = c(10, 10),
    sample_date = as.Date(c("2026-04-01", "2025-01-01"))
  )
  # 40 is from another cruise date and must not affect the scale
  expect_equal(algaware:::compute_profile_xlim(ctd, lims), c(0, 12 * 1.05))
})

test_that("compute_profile_xlim falls back to 0-10 for all-NA data", {
  ctd <- data.frame(
    chl_fluorescence = c(NA_real_, NA_real_),
    pressure_dbar    = c(5, NA),
    sample_date      = as.Date(c(NA, NA))
  )
  expect_equal(algaware:::compute_profile_xlim(ctd), c(0, 10))
})
