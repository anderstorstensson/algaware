make_metadata <- function(pids, dates, cruise = NA_character_) {
  data.frame(
    pid = pids,
    sample_time = as.POSIXct(paste(dates, "10:00:00"), tz = "UTC"),
    latitude = 57.0,
    longitude = 11.0,
    cruise = cruise,
    stringsAsFactors = FALSE
  )
}

# Cache load/save ------------------------------------------------------------

test_that("metadata cache round-trips and validates url/dataset", {
  dir <- withr::local_tempdir()
  cache_file <- algaware:::metadata_cache_path(dir)
  md <- make_metadata("s1", "2025-07-01")

  algaware:::save_metadata_cache(cache_file, "https://ifcb.example.com",
                                 "RV_Svea", md)
  expect_equal(
    algaware:::load_metadata_cache(cache_file, "https://ifcb.example.com",
                                   "RV_Svea"),
    md
  )
  # Different URL or dataset invalidates the cache
  expect_null(algaware:::load_metadata_cache(cache_file,
                                             "https://other.example.com",
                                             "RV_Svea"))
  expect_null(algaware:::load_metadata_cache(cache_file,
                                             "https://ifcb.example.com",
                                             "Other"))
})

test_that("load_metadata_cache handles missing and corrupt files", {
  dir <- withr::local_tempdir()
  cache_file <- algaware:::metadata_cache_path(dir)
  expect_null(algaware:::load_metadata_cache(cache_file, "u", "d"))
  writeLines("not an rds", cache_file)
  expect_null(algaware:::load_metadata_cache(cache_file, "u", "d"))
})

# bind/merge ------------------------------------------------------------------

test_that("bind_metadata_rows unions columns and coerces types", {
  cached <- make_metadata(c("s1", "s2"), c("2025-07-01", "2025-07-02"),
                          cruise = c("C1", "C1"))
  fresh <- data.frame(
    pid = "s3",
    sample_time = as.POSIXct("2025-07-03 10:00:00", tz = "UTC"),
    latitude = 57.0,
    longitude = 11.0,
    cruise = NA,           # all-NA increment column inferred as logical
    extra_col = "x",
    stringsAsFactors = FALSE
  )

  combined <- algaware:::bind_metadata_rows(cached, fresh)
  expect_equal(nrow(combined), 3)
  expect_type(combined$cruise, "character")
  expect_true("extra_col" %in% names(combined))
  expect_true(all(is.na(combined$extra_col[1:2])))
  expect_s3_class(combined$sample_time, "POSIXct")
})

test_that("bind_metadata_rows with empty increment returns cache unchanged", {
  cached <- make_metadata("s1", "2025-07-01")
  fresh <- cached[0, ]
  expect_equal(algaware:::bind_metadata_rows(cached, fresh), cached)
})

test_that("merge_metadata_increment replaces the overlap window", {
  cached <- make_metadata(c("s1", "s2", "s3"),
                          c("2025-07-01", "2025-07-02", "2025-07-02"))
  # Fresh fetch for 2025-07-02 onwards: s3 was edited, s4 is new; s2 gone
  # from the dashboard (e.g. skipped)
  fresh <- make_metadata(c("s3", "s4"), c("2025-07-02", "2025-07-03"))
  fresh$cruise <- c("C9", "C9")

  merged <- algaware:::merge_metadata_increment(cached, fresh,
                                                as.Date("2025-07-02"))
  expect_setequal(merged$pid, c("s1", "s3", "s4"))
  expect_equal(merged$cruise[merged$pid == "s3"], "C9")
})

test_that("merge_metadata_increment keeps rows with unparseable times", {
  cached <- make_metadata(c("s1", "s2"), c("2025-07-01", "2025-07-02"))
  cached$sample_time[1] <- NA
  fresh <- make_metadata("s3", "2025-07-03")
  merged <- algaware:::merge_metadata_increment(cached, fresh,
                                                as.Date("2025-07-02"))
  expect_setequal(merged$pid, c("s1", "s3"))
})

# fetch_dashboard_metadata ----------------------------------------------------

test_that("first fetch downloads full export and writes the cache", {
  dir <- withr::local_tempdir()
  md <- make_metadata(c("s1", "s2"), c("2025-07-01", "2025-07-02"),
                      cruise = c("C1", "C1"))
  mockery::stub(fetch_dashboard_metadata,
                "iRfcb::ifcb_download_dashboard_metadata", md)
  mockery::stub(fetch_dashboard_metadata, "fetch_metadata_window",
                function(...) stop("no incremental fetch on first run"))

  result <- fetch_dashboard_metadata("https://ifcb.example.com", "RV_Svea",
                                     cache_dir = dir)
  expect_false(result$incremental)
  expect_equal(result$n_new, 2)
  expect_equal(result$cruise_numbers, "C1")
  expect_true(file.exists(algaware:::metadata_cache_path(dir)))
})

test_that("second fetch is incremental from the newest cached day", {
  dir <- withr::local_tempdir()
  cached <- make_metadata(c("s1", "s2"), c("2025-07-01", "2025-07-02"),
                          cruise = c("C1", "C1"))
  algaware:::save_metadata_cache(algaware:::metadata_cache_path(dir),
                                 "https://ifcb.example.com", "RV_Svea",
                                 cached)

  window_args <- NULL
  mockery::stub(fetch_dashboard_metadata,
                "iRfcb::ifcb_download_dashboard_metadata",
                function(...) stop("full download should not run"))
  mockery::stub(fetch_dashboard_metadata, "fetch_metadata_window",
                function(dashboard_url, dataset_name, start_date, ...) {
                  window_args <<- list(url = dashboard_url,
                                       dataset = dataset_name,
                                       start = start_date)
                  make_metadata(c("s2", "s3"),
                                c("2025-07-02", "2025-07-03"),
                                cruise = c("C1", "C2"))
                })

  result <- fetch_dashboard_metadata("https://ifcb.example.com", "RV_Svea",
                                     cache_dir = dir)
  expect_true(result$incremental)
  expect_equal(result$n_new, 2)
  expect_equal(window_args$start, as.Date("2025-07-02"))
  expect_setequal(result$metadata$pid, c("s1", "s2", "s3"))
  expect_setequal(result$cruise_numbers, c("C1", "C2"))

  # Cache now contains the merged result
  updated <- algaware:::load_metadata_cache(
    algaware:::metadata_cache_path(dir),
    "https://ifcb.example.com", "RV_Svea"
  )
  expect_setequal(updated$pid, c("s1", "s2", "s3"))
})

test_that("force_full ignores the cache and downloads everything", {
  dir <- withr::local_tempdir()
  algaware:::save_metadata_cache(algaware:::metadata_cache_path(dir),
                                 "https://ifcb.example.com", "RV_Svea",
                                 make_metadata("s1", "2025-07-01"))

  md <- make_metadata(c("s1", "s2"), c("2025-07-01", "2025-07-02"))
  mockery::stub(fetch_dashboard_metadata,
                "iRfcb::ifcb_download_dashboard_metadata", md)
  mockery::stub(fetch_dashboard_metadata, "fetch_metadata_window",
                function(...) stop("incremental fetch should not run"))

  result <- fetch_dashboard_metadata("https://ifcb.example.com", "RV_Svea",
                                     cache_dir = dir, force_full = TRUE)
  expect_false(result$incremental)
  expect_equal(nrow(result$metadata), 2)
})

test_that("fetch without cache_dir behaves like before", {
  md <- make_metadata("s1", "2025-07-01")
  mockery::stub(fetch_dashboard_metadata,
                "iRfcb::ifcb_download_dashboard_metadata", md)
  result <- fetch_dashboard_metadata("https://ifcb.example.com", "RV_Svea")
  expect_false(result$incremental)
  expect_equal(result$metadata, md)
})

test_that("clear_metadata_cache removes the cache file", {
  dir <- withr::local_tempdir()
  expect_false(clear_metadata_cache(dir))
  algaware:::save_metadata_cache(algaware:::metadata_cache_path(dir),
                                 "u", "d", make_metadata("s1", "2025-07-01"))
  expect_true(clear_metadata_cache(dir))
  expect_false(file.exists(algaware:::metadata_cache_path(dir)))
})
