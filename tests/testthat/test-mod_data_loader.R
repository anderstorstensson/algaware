test_that("build_cruise_info returns expected format", {
  times <- as.POSIXct(c("2024-06-03 08:00:00", "2024-06-07 14:00:00",
                         "2024-06-05 11:00:00"))
  result <- build_cruise_info(times)
  expect_match(result, "^RV Svea June cruise, 2024-06-03 to 2024-06-07$")
})

test_that("build_cruise_info picks dominant month across month boundary", {
  # 3 samples in June, 1 in July — use noon to avoid midnight timezone shifts
  times <- as.POSIXct(c("2024-06-28 12:00:00", "2024-06-29 12:00:00",
                         "2024-06-30 12:00:00", "2024-07-01 12:00:00"))
  result <- build_cruise_info(times)
  expect_match(result, "June")
  expect_match(result, "2024-06-28 to 2024-07-01")
})

test_that("build_cruise_info handles single sample", {
  times <- as.POSIXct("2024-03-15 10:00:00")
  result <- build_cruise_info(times)
  expect_match(result, "^RV Svea March cruise, 2024-03-15 to 2024-03-15$")
})

test_that("sanitize_error_msg strips leading prefix", {
  expect_equal(sanitize_error_msg("Error in foo: something went wrong"),
               "something went wrong")
})

test_that("sanitize_error_msg returns message unchanged when no colon-space", {
  expect_equal(sanitize_error_msg("no colon here"), "no colon here")
})

test_that("sanitize_error_msg keeps messages with inner colons intact", {
  # Only R's own "Error in <call>: " prefix is stripped; a greedy match
  # used to cut everything up to the last colon, discarding the context
  # (URL, failing operation) from the only error surface in the app.
  expect_equal(sanitize_error_msg("outer: inner: actual message"),
               "outer: inner: actual message")
  expect_equal(
    sanitize_error_msg("Error in download(x): cannot open URL 'u': 404"),
    "cannot open URL 'u': 404"
  )
})

test_that("detect_near_empty_bins flags bins below the absolute threshold", {
  matched <- data.frame(
    pid = c("D20240601T080000_IFCB134", "D20240601T090000_IFCB134",
            "D20240601T100000_IFCB134"),
    STATION_NAME = c("BY31 LANDSORTSDJ", "BY31 LANDSORTSDJ", "SLAGGO"),
    n_images = c(5000, 12, 4800),
    stringsAsFactors = FALSE
  )
  result <- detect_near_empty_bins(matched)
  expect_equal(nrow(result), 1)
  expect_equal(result$pid, "D20240601T090000_IFCB134")
  expect_equal(result$n_images, 12)
})

test_that("detect_near_empty_bins does not flag small-but-legitimate bins", {
  # Low-biomass bins (e.g. few cells on the west coast while the Baltic
  # blooms) must not be flagged: the threshold is absolute and strict,
  # not relative to the cruise median.
  matched <- data.frame(
    pid = c("a", "b", "c", "d"),
    STATION_NAME = "X",
    n_images = c(5000, 5200, 4800, 300),
    stringsAsFactors = FALSE
  )
  expect_equal(nrow(detect_near_empty_bins(matched)), 0)
})

test_that("detect_near_empty_bins returns empty when all bins are normal", {
  matched <- data.frame(
    pid = c("a", "b"), STATION_NAME = "X",
    n_images = c(3000, 4000), stringsAsFactors = FALSE
  )
  expect_equal(nrow(detect_near_empty_bins(matched)), 0)
})

test_that("detect_near_empty_bins uses a strict threshold of 20 images", {
  matched <- data.frame(
    pid = c("at_threshold", "just_below"),
    STATION_NAME = "X",
    n_images = c(20, 19),
    stringsAsFactors = FALSE
  )
  result <- detect_near_empty_bins(matched)
  expect_equal(result$pid, "just_below")
})

test_that("detect_near_empty_bins handles missing column, NULL, and empty input", {
  expect_equal(nrow(detect_near_empty_bins(NULL)), 0)
  no_col <- data.frame(pid = "a", stringsAsFactors = FALSE)
  expect_equal(nrow(detect_near_empty_bins(no_col)), 0)
  empty <- data.frame(pid = character(0), n_images = numeric(0))
  expect_equal(nrow(detect_near_empty_bins(empty)), 0)
})

test_that("detect_near_empty_bins skips NA counts and sorts ascending", {
  matched <- data.frame(
    pid = c("a", "b", "c", "d"),
    STATION_NAME = "X",
    n_images = c(NA, 12, 5, 3000),
    stringsAsFactors = FALSE
  )
  result <- detect_near_empty_bins(matched)
  expect_equal(result$pid, c("c", "b"))
  expect_equal(result$n_images, c(5, 12))
})

test_that("detect_near_empty_bins works without STATION_NAME column", {
  matched <- data.frame(
    pid = c("a", "b"), n_images = c(10, 4000), stringsAsFactors = FALSE
  )
  result <- detect_near_empty_bins(matched)
  expect_equal(result$pid, "a")
  expect_true(is.na(result$STATION_NAME))
})

test_that("build_near_empty_warning renders count, bins, and truncation", {
  near_empty <- data.frame(
    pid = paste0("bin", 1:12),
    STATION_NAME = "BY31",
    n_images = 1:12,
    stringsAsFactors = FALSE
  )
  html <- as.character(build_near_empty_warning(near_empty))
  expect_match(html, "12 near-empty bins detected")
  expect_match(html, "bin1 \\(BY31\\): 1 images")
  expect_match(html, "bin10")
  expect_false(grepl("bin11 ", html))
  expect_match(html, "\\+ 2 more")
  expect_match(html, "Samples")

  single <- detect_near_empty_bins(data.frame(
    pid = c("a", "b"), STATION_NAME = c(NA, "X"),
    n_images = c(10, 4000), stringsAsFactors = FALSE
  ))
  html_single <- as.character(build_near_empty_warning(single))
  expect_match(html_single, "1 near-empty bin detected")
  # NA station name is omitted, not printed as "(NA)"
  expect_false(grepl("\\(NA\\)", html_single))
})

test_that("reset_corrections_state empties corrections and custom classes but keeps shape", {
  rv <- shiny::reactiveValues(
    corrections = data.frame(
      sample_name = "D20240301T100000_IFCB134", roi_number = 1L,
      original_class = "A", new_class = "B", stringsAsFactors = FALSE
    ),
    custom_classes = data.frame(
      clean_names = "Foo", name = "Foo", sflag = "", AphiaID = 1L,
      HAB = FALSE, italic = TRUE, is_diatom = TRUE, stringsAsFactors = FALSE
    ),
    selected_images = c("img1", "img2")
  )
  shiny::isolate(reset_corrections_state(rv))
  shiny::isolate({
    expect_equal(nrow(rv$corrections), 0)
    expect_named(rv$corrections,
                 c("sample_name", "roi_number", "original_class", "new_class"))
    expect_type(rv$corrections$roi_number, "integer")
    expect_equal(nrow(rv$custom_classes), 0)
    expect_named(rv$custom_classes,
                 c("clean_names", "name", "sflag", "AphiaID", "HAB", "italic",
                   "is_diatom"))
    expect_equal(rv$selected_images, character(0))
  })
})

test_that("reset_corrections_state tolerates missing fields", {
  rv <- shiny::reactiveValues()
  expect_no_error(shiny::isolate(reset_corrections_state(rv)))
  expect_equal(shiny::isolate(rv$selected_images), character(0))
})
