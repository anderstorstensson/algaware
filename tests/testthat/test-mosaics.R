test_that("create_mosaic errors with no images", {
  expect_error(create_mosaic(character(0)), "No images provided")
})

test_that("compute_median_color returns hex string", {
  # Create a small test image
  img <- magick::image_blank(10, 10, color = "#FF0000")
  result <- algaware:::compute_median_color(list(img))
  expect_match(result, "^#[0-9A-F]{6}$")
})

test_that("compute_median_color handles errors gracefully", {
  result <- algaware:::compute_median_color(list("not an image"))
  expect_equal(result, "#F0F0F0")
})

test_that("create_mosaic works with real images", {
  tmp_dir <- file.path(tempdir(), paste0("mosaic_test_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Create test PNG files
  paths <- character(0)
  for (i in 1:6) {
    path <- file.path(tmp_dir, paste0("img_", i, ".png"))
    img <- magick::image_blank(50, 40, color = "white")
    magick::image_write(img, path)
    paths <- c(paths, path)
  }

  result <- create_mosaic(paths, n_images = 6)
  expect_s3_class(result, "magick-image")

  info <- magick::image_info(result)
  expect_true(info$width > 0)
  expect_true(info$height > 0)
})

test_that("create_mosaic samples when too many images", {
  tmp_dir <- file.path(tempdir(), paste0("mosaic_sample_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  paths <- character(0)
  for (i in 1:10) {
    path <- file.path(tmp_dir, paste0("img_", i, ".png"))
    img <- magick::image_blank(30, 30, color = "gray")
    magick::image_write(img, path)
    paths <- c(paths, path)
  }

  result <- create_mosaic(paths, n_images = 4)
  expect_s3_class(result, "magick-image")
})

test_that("justify_row centers single image", {
  img <- magick::image_blank(50, 30, color = "white")
  result <- algaware:::justify_row(
    list(img), 50, target_row_width = 200,
    target_height = 30, bg_color = "#F0F0F0"
  )
  expect_s3_class(result, "magick-image")
  info <- magick::image_info(result)
  expect_equal(info$width, 200)
})

test_that("justify_row distributes padding for multiple images", {
  img1 <- magick::image_blank(40, 30, color = "white")
  img2 <- magick::image_blank(40, 30, color = "white")
  result <- algaware:::justify_row(
    list(img1, img2), c(40, 40),
    target_row_width = 200, target_height = 30,
    bg_color = "#F0F0F0"
  )
  expect_s3_class(result, "magick-image")
  info <- magick::image_info(result)
  expect_equal(info$width, 200)
})

# --- get_top_taxa ---

test_that("get_top_taxa returns taxa ordered by total biovolume", {
  wide <- data.frame(
    scientific_name = c("Species A", "Species B", "Species C"),
    station_1 = c(10, 50, 30),
    station_2 = c(20, 10, 25),
    stringsAsFactors = FALSE
  )
  result <- get_top_taxa(wide, 3L)
  expect_equal(result, c("Species B", "Species C", "Species A"))
})

test_that("get_top_taxa respects n_taxa limit", {
  wide <- data.frame(
    scientific_name = c("A", "B", "C", "D"),
    s1 = c(1, 4, 3, 2),
    stringsAsFactors = FALSE
  )
  result <- get_top_taxa(wide, 2L)
  expect_length(result, 2)
  expect_equal(result, c("B", "C"))
})

test_that("get_top_taxa returns empty when no data columns", {
  wide <- data.frame(scientific_name = c("A", "B"), stringsAsFactors = FALSE)
  result <- get_top_taxa(wide, 5L)
  expect_length(result, 0)
})

# --- get_taxon_rois ---

test_that("get_taxon_rois filters by taxon and sample_ids", {
  classifications <- data.frame(
    sample_name = c("S1", "S1", "S2", "S3"),
    roi_number = c(1L, 2L, 3L, 4L),
    class_name = c("Sp_a", "Sp_b", "Sp_a", "Sp_a"),
    stringsAsFactors = FALSE
  )
  taxa_lookup <- data.frame(
    clean_names = c("Sp_a", "Sp_b"),
    name = c("Species A", "Species B"),
    stringsAsFactors = FALSE
  )

  result <- get_taxon_rois(classifications, taxa_lookup, "Species A",
                           c("S1", "S2"))
  expect_equal(nrow(result), 2)
  expect_true(all(result$class_name == "Sp_a"))
  expect_true(all(result$sample_name %in% c("S1", "S2")))
})

test_that("get_taxon_rois returns empty for unknown taxon", {
  classifications <- data.frame(
    sample_name = "S1", roi_number = 1L, class_name = "Sp_a",
    stringsAsFactors = FALSE
  )
  taxa_lookup <- data.frame(
    clean_names = "Sp_a", name = "Species A", stringsAsFactors = FALSE
  )
  result <- get_taxon_rois(classifications, taxa_lookup, "Unknown", "S1")
  expect_equal(nrow(result), 0)
})

test_that("get_taxon_rois excludes samples outside sample_ids", {
  classifications <- data.frame(
    sample_name = c("S1", "S2"),
    roi_number = c(1L, 2L),
    class_name = c("Sp_a", "Sp_a"),
    stringsAsFactors = FALSE
  )
  taxa_lookup <- data.frame(
    clean_names = "Sp_a", name = "Species A", stringsAsFactors = FALSE
  )
  result <- get_taxon_rois(classifications, taxa_lookup, "Species A", "S1")
  expect_equal(nrow(result), 1)
  expect_equal(result$sample_name, "S1")
})

# --- extract_random_taxon_image ---

test_that("extract_random_taxon_image returns NULL for unknown taxon", {
  classifications <- data.frame(
    sample_name = "S1", roi_number = 1L, class_name = "Sp_a",
    stringsAsFactors = FALSE
  )
  taxa_lookup <- data.frame(
    clean_names = "Sp_a", name = "Species A", stringsAsFactors = FALSE
  )
  result <- extract_random_taxon_image(
    "Unknown", classifications, taxa_lookup, "S1",
    tempdir(), tempdir()
  )
  expect_null(result)
})

test_that("extract_random_taxon_image returns NULL when no ROI files found", {
  classifications <- data.frame(
    sample_name = "S1", roi_number = 1L, class_name = "Sp_a",
    stringsAsFactors = FALSE
  )
  taxa_lookup <- data.frame(
    clean_names = "Sp_a", name = "Species A", stringsAsFactors = FALSE
  )
  empty_dir <- file.path(tempdir(), paste0("empty_roi_", Sys.getpid()))
  dir.create(empty_dir, showWarnings = FALSE)
  on.exit(unlink(empty_dir, recursive = TRUE), add = TRUE)

  result <- extract_random_taxon_image(
    "Species A", classifications, taxa_lookup, "S1",
    empty_dir, tempdir()
  )
  expect_null(result)
})

test_that("extract_random_taxon_image respects exclude_rois", {
  classifications <- data.frame(
    sample_name = c("S1", "S1"),
    roi_number = c(1L, 2L),
    class_name = c("Sp_a", "Sp_a"),
    stringsAsFactors = FALSE
  )
  taxa_lookup <- data.frame(
    clean_names = "Sp_a", name = "Species A", stringsAsFactors = FALSE
  )
  exclude <- data.frame(
    sample_name = c("S1", "S1"),
    roi_number = c(1L, 2L),
    stringsAsFactors = FALSE
  )
  # All ROIs excluded -> NULL
  result <- extract_random_taxon_image(
    "Species A", classifications, taxa_lookup, "S1",
    tempdir(), tempdir(), exclude_rois = exclude
  )
  expect_null(result)
})

# --- create_mosaic with mixed aspect ratios (frontpage scenario) ---

test_that("create_mosaic handles mixed aspect ratio images", {
  tmp_dir <- file.path(tempdir(), paste0("mosaic_mixed_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  paths <- character(0)
  # Mix of aspect ratios: round, wide, very elongated
  dims <- list(c(50, 50), c(200, 40), c(400, 30), c(60, 60), c(150, 50))
  for (i in seq_along(dims)) {
    path <- file.path(tmp_dir, paste0("img_", i, ".png"))
    img <- magick::image_blank(dims[[i]][1], dims[[i]][2], color = "white")
    magick::image_write(img, path)
    paths <- c(paths, path)
  }

  result <- create_mosaic(paths, n_images = 5, max_height_px = 1100L)
  expect_s3_class(result, "magick-image")
  info <- magick::image_info(result)
  expect_true(info$width > 0)
  expect_true(info$height <= 1100)
})

# --- add_front_page ---

test_that("add_front_page adds logo and dnr to document", {
  template <- system.file("templates", "report_template.docx",
                          package = "algaware")
  skip_if(!nzchar(template), "Report template not available")

  doc <- officer::read_docx(template)
  cleanup <- new.env(parent = emptyenv())
  cleanup$files <- character(0)

  doc <- algaware:::add_front_page(doc, cleanup,
                                   cruise_info = "RV Svea March cruise, 2026-03-08 to 2026-03-14",
                                   report_number = "1")

  out <- tempfile(fileext = ".docx")
  on.exit({
    unlink(out)
    unlink(cleanup$files)
  }, add = TRUE)
  print(doc, target = out)
  expect_true(file.exists(out))
  expect_true(file.size(out) > 0)
})

test_that("add_mosaic_overview adds mosaics to document", {
  template <- system.file("templates", "report_template.docx",
                          package = "algaware")
  skip_if(!nzchar(template), "Report template not available")

  doc <- officer::read_docx(template)
  cleanup <- new.env(parent = emptyenv())
  cleanup$files <- character(0)

  baltic <- magick::image_blank(600, 200, color = "lightblue")
  west <- magick::image_blank(600, 200, color = "lightyellow")

  doc <- algaware:::add_mosaic_overview(doc, baltic, west, cleanup,
                                        baltic_taxa = c("Taxon A", "Taxon B"),
                                        westcoast_taxa = c("Taxon C"))

  out <- tempfile(fileext = ".docx")
  on.exit({
    unlink(out)
    unlink(cleanup$files)
  }, add = TRUE)
  print(doc, target = out)
  expect_true(file.exists(out))
  expect_true(file.size(out) > 0)
})

test_that("add_mosaic_overview works with only one mosaic", {
  template <- system.file("templates", "report_template.docx",
                          package = "algaware")
  skip_if(!nzchar(template), "Report template not available")

  doc <- officer::read_docx(template)
  cleanup <- new.env(parent = emptyenv())
  cleanup$files <- character(0)

  baltic <- magick::image_blank(600, 200, color = "lightblue")

  doc <- algaware:::add_mosaic_overview(doc, baltic, NULL, cleanup)

  out <- tempfile(fileext = ".docx")
  on.exit({
    unlink(out)
    unlink(cleanup$files)
  }, add = TRUE)
  print(doc, target = out)
  expect_true(file.exists(out))
})
