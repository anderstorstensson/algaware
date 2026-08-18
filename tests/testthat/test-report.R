# Tests for R/report.R helper functions

# -- extract_month_year -------------------------------------------------------

test_that("extract_month_year parses standard cruise info string", {
  result <- algaware:::extract_month_year(
    "RV Svea March cruise, 2026-03-08 to 2026-03-14"
  )
  expect_equal(result, "March 2026")
})

test_that("extract_month_year returns empty string for empty input", {
  expect_equal(algaware:::extract_month_year(""), "")
})

test_that("extract_month_year handles missing month", {
  result <- algaware:::extract_month_year("Cruise 2026-06-01 to 2026-06-07")
  expect_equal(result, "2026")
})

test_that("extract_month_year handles missing year", {
  result <- algaware:::extract_month_year("April cruise")
  expect_equal(result, "April")
})

test_that("extract_month_year handles no month or year", {
  result <- algaware:::extract_month_year("Some cruise text without dates")
  expect_equal(result, "")
})

# -- build_sample_counts ------------------------------------------------------

test_that("build_sample_counts returns NULL when n_samples absent", {
  ss <- data.frame(STATION_NAME_SHORT = "BY5",
                   visit_date = as.Date("2024-03-15"),
                   stringsAsFactors = FALSE)
  expect_null(algaware:::build_sample_counts(ss))
})

test_that("build_sample_counts returns named integer vector", {
  ss <- data.frame(
    STATION_NAME_SHORT = c("BY5", "BY5", "BY31"),
    visit_date = as.Date(c("2024-03-15", "2024-03-15", "2024-03-16")),
    n_samples = c(3L, 3L, 2L),
    stringsAsFactors = FALSE
  )
  result <- algaware:::build_sample_counts(ss)
  expect_type(result, "integer")
  expect_equal(result[["BY5_2024-03-15"]], 3L)
  expect_equal(result[["BY31_2024-03-16"]], 2L)
})

# -- add_centered_plot --------------------------------------------------------

test_that("add_centered_plot adds a plot to an rdocx document", {
  skip_if_not_installed("ggplot2")

  doc <- officer::read_docx()
  cleanup <- new.env(parent = emptyenv())
  cleanup$files <- character(0)

  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3),
                       ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  on.exit(unlink(cleanup$files), add = TRUE)

  result <- algaware:::add_centered_plot(doc, p, cleanup,
                                         width = 4, height = 3,
                                         display_width = 4, display_height = 3)
  expect_s3_class(result, "rdocx")
  expect_true(length(cleanup$files) > 0)
  expect_true(all(file.exists(cleanup$files)))
})

# -- add_plot_to_doc ----------------------------------------------------------

test_that("add_plot_to_doc adds a plot inline to an rdocx document", {
  skip_if_not_installed("ggplot2")

  doc <- officer::read_docx()
  cleanup <- new.env(parent = emptyenv())
  cleanup$files <- character(0)

  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3),
                       ggplot2::aes(x, y)) +
    ggplot2::geom_line()
  on.exit(unlink(cleanup$files), add = TRUE)

  result <- algaware:::add_plot_to_doc(doc, p, cleanup,
                                        width = 6, height = 4,
                                        display_width = 6, display_height = 4)
  expect_s3_class(result, "rdocx")
  expect_true(length(cleanup$files) > 0)
})

# -- add_heatmap_section ------------------------------------------------------

test_that("add_heatmap_section returns doc+fig_num list", {
  skip_if_not_installed("ggplot2")

  wide <- data.frame(
    scientific_name = paste0("Taxon_", 1:5),
    `STN1_2024-03-15` = runif(5, 0, 1),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  doc <- officer::read_docx()
  cleanup <- new.env(parent = emptyenv())
  cleanup$files <- character(0)
  on.exit(unlink(cleanup$files), add = TRUE)

  result <- algaware:::add_heatmap_section(doc, wide, NULL,
                                            title = "Baltic", fig_num = 1L,
                                            cleanup = cleanup)
  expect_named(result, c("doc", "fig_num"))
  expect_s3_class(result$doc, "rdocx")
  expect_equal(result$fig_num, 2L)
})

test_that("add_heatmap_section skips empty data", {
  wide <- data.frame(scientific_name = character(0), stringsAsFactors = FALSE)
  doc <- officer::read_docx()
  cleanup <- new.env(parent = emptyenv())
  cleanup$files <- character(0)

  result <- algaware:::add_heatmap_section(doc, wide, NULL,
                                            title = "Baltic", fig_num = 5L,
                                            cleanup = cleanup)
  expect_equal(result$fig_num, 5L)  # unchanged when no data
  expect_equal(length(cleanup$files), 0L)
})

# -- add_stacked_bar_section --------------------------------------------------

test_that("add_stacked_bar_section adds chart and increments fig_num", {
  skip_if_not_installed("ggplot2")

  wide <- data.frame(
    scientific_name = paste0("Taxon_", 1:8),
    `STN1_2024-03-15` = runif(8, 1, 100),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  doc <- officer::read_docx()
  cleanup <- new.env(parent = emptyenv())
  cleanup$files <- character(0)
  on.exit(unlink(cleanup$files), add = TRUE)

  result <- algaware:::add_stacked_bar_section(doc, wide, NULL,
                                                title = "Baltic",
                                                fig_num = 2L,
                                                cleanup = cleanup)
  expect_named(result, c("doc", "fig_num"))
  expect_equal(result$fig_num, 3L)
})

test_that("add_stacked_bar_section skips when wide_data has no columns", {
  wide <- data.frame(scientific_name = "Taxon A", stringsAsFactors = FALSE)
  doc <- officer::read_docx()
  cleanup <- new.env(parent = emptyenv())
  cleanup$files <- character(0)

  result <- algaware:::add_stacked_bar_section(doc, wide, NULL,
                                                title = "Baltic",
                                                fig_num = 3L,
                                                cleanup = cleanup)
  expect_equal(result$fig_num, 3L)
})

# -- add_station_sections (basic smoke test) ----------------------------------

test_that("add_station_sections returns rdocx", {
  station_summary <- data.frame(
    STATION_NAME = "BY5 Bornholm Basin",
    STATION_NAME_SHORT = "BY5",
    COAST = "EAST",
    visit_date = as.Date("2024-03-15"),
    visit_id = "BY5_2024-03-15",
    name = c("Skeletonema marinoi", "Taxon B"),
    display_name = c("Skeletonema marinoi", "Taxon B"),
    biovolume_mm3_per_liter = c(0.5, 0.1),
    carbon_ug_per_liter = c(10, 2),
    counts_per_liter = c(1000, 200),
    stringsAsFactors = FALSE
  )
  doc <- officer::read_docx()
  result <- algaware:::add_station_sections(doc, station_summary)
  expect_s3_class(result, "rdocx")
})

test_that("transform_ooxml_part preserves non-ASCII bytes regardless of locale", {
  # Regression: text connections re-encoded the XML through the native locale,
  # mangling Å/Ä/Ö and µ into literal escapes on non-UTF-8 hosts.
  xml <- enc2utf8("<w:t>5 µm at Väderöarna, SLÄGGÖ – Å17</w:t><w:sectPr>")
  path <- tempfile(fileext = ".xml")
  on.exit(unlink(path), add = TRUE)
  writeBin(charToRaw(xml), path)

  run_roundtrip <- function() {
    changed <- algaware:::transform_ooxml_part(path, function(text) {
      sub("<w:sectPr>", '<w:pgNumType w:start="1"/><w:sectPr>', text,
          fixed = TRUE)
    })
    expect_true(changed)
    result <- rawToChar(readBin(path, "raw", file.size(path)))
    Encoding(result) <- "UTF-8"
    expect_identical(
      result,
      enc2utf8(paste0("<w:t>5 µm at Väderöarna, SLÄGGÖ – Å17</w:t>",
                      '<w:pgNumType w:start="1"/><w:sectPr>'))
    )
  }

  c_locale_ok <- !is.na(Sys.setlocale("LC_CTYPE", "C"))
  Sys.setlocale("LC_CTYPE", "")
  if (c_locale_ok) {
    withr::with_locale(c(LC_CTYPE = "C"), run_roundtrip())
    # Reset the file for the native-locale pass
    writeBin(charToRaw(xml), path)
  }
  run_roundtrip()
})
