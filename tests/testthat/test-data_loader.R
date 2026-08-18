test_that("build_cruise_info formats correctly", {
  times <- as.POSIXct(c("2024-03-10 08:00:00", "2024-03-11 10:00:00",
                         "2024-03-12 12:00:00"), tz = "UTC")
  result <- algaware:::build_cruise_info(times)
  expect_type(result, "character")
  expect_true(grepl("RV Svea", result))
  expect_true(grepl("March", result))
  expect_true(grepl("2024-03-10", result))
  expect_true(grepl("2024-03-12", result))
})

test_that("sanitize_error_msg strips prefix", {
  result <- algaware:::sanitize_error_msg("Error in foo(): something went wrong")
  expect_equal(result, "something went wrong")
})

test_that("sanitize_error_msg handles message without colon-space", {
  result <- algaware:::sanitize_error_msg("simple error")
  expect_equal(result, "simple error")
})

test_that("validate_sample_ids accepts valid IDs", {
  ids <- c("D20221023T000155_IFCB134", "D20240315T120000_IFCB999")
  expect_invisible(algaware:::validate_sample_ids(ids))
})

test_that("validate_sample_ids rejects invalid IDs", {
  expect_error(
    algaware:::validate_sample_ids(c("D20221023T000155_IFCB134", "../evil")),
    "Invalid IFCB sample IDs"
  )
})

test_that("validate_sample_ids handles empty input", {
  expect_invisible(algaware:::validate_sample_ids(character(0)))
})

test_that("resolve_classes auto-generates from taxa and classifications", {
  config <- list(db_folder = "")
  taxa <- data.frame(clean_names = c("diatom", "dino"),
                     stringsAsFactors = FALSE)
  classif <- data.frame(class_name = c("diatom", "cocco"),
                        stringsAsFactors = FALSE)
  result <- algaware:::resolve_classes(config, taxa, classif)
  expect_true(result$auto_generated)
  expect_true("unclassified" %in% result$class_list)
  expect_true("diatom" %in% result$class_list)
  expect_true("cocco" %in% result$class_list)
  expect_true("dino" %in% result$class_list)
})

test_that("compute_visit_dates picks most common date", {
  all_data <- data.frame(
    visit_id = rep("stn_visit1", 3),
    STATION_NAME = rep("STN1", 3),
    sample_date = as.Date(c("2024-03-10", "2024-03-10", "2024-03-11")),
    stringsAsFactors = FALSE
  )
  result <- algaware:::compute_visit_dates(all_data)
  expect_equal(nrow(result), 1)
  expect_equal(as.Date(result$visit_date), as.Date("2024-03-10"))
})

test_that("compute_per_liter guards against zero volume", {
  agg <- data.frame(
    total_counts = c(100, 200),
    total_biovolume_mm3 = c(0.5, 1.0),
    total_carbon_ug = c(10, 20),
    total_ml_analyzed = c(5.0, 0.0),
    stringsAsFactors = FALSE
  )
  result <- algaware:::compute_per_liter(agg)
  expect_equal(result$counts_per_liter[1], 100 / (5.0 / 1000))
  expect_true(is.na(result$counts_per_liter[2]))
})

test_that("compute_presence_categories assigns correct categories", {
  agg <- data.frame(
    visit_id = rep("v1", 3),
    STATION_NAME = rep("S1", 3),
    counts_per_liter = c(600, 300, 100),
    stringsAsFactors = FALSE
  )
  result <- algaware:::compute_presence_categories(agg)
  # 600/1000 = 60% -> 5, 300/1000 = 30% -> 4, 100/1000 = 10% -> 4
  expect_equal(result$Presence_cat[1], 5L)
  expect_equal(result$Presence_cat[2], 4L)
  expect_equal(result$Presence_cat[3], 4L)
})

test_that("sanitize_error_msg keeps context after inner colons", {
  # Regression: the greedy pattern stripped everything up to the LAST
  # colon, reducing the only error surface in the app to fragments like
  # "'404 Not Found'" with the URL and failing operation gone.
  msg <- "cannot open URL 'https://x.se/a': HTTP status was '404 Not Found'"
  expect_equal(algaware:::sanitize_error_msg(msg), msg)

  prefixed <- paste0("Error in download(): ", msg)
  expect_equal(algaware:::sanitize_error_msg(prefixed), msg)
})
