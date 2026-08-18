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
