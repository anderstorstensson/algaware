test_that("get_hab_species returns empty vector for NULL input", {
  result <- algaware:::get_hab_species(NULL)
  expect_equal(result, character(0))
})

test_that("get_hab_species returns empty vector when no HAB column", {
  taxa <- data.frame(
    name = c("Species A", "Species B"),
    stringsAsFactors = FALSE
  )
  result <- algaware:::get_hab_species(taxa)
  expect_equal(result, character(0))
})

test_that("get_hab_species extracts HAB species", {
  taxa <- data.frame(
    name = c("Species A", "Species B", "Species C"),
    HAB = c(TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  result <- algaware:::get_hab_species(taxa)
  expect_setequal(result, c("Species A", "Species C"))
})

test_that("create_heatmap returns ggplot", {
  wide <- data.frame(
    scientific_name = c("Taxon A", "Taxon B"),
    `STN1_2022-01-01` = c(10, 20),
    `STN2_2022-01-02` = c(30, 40),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  p <- create_heatmap(wide, title = "Test")
  expect_s3_class(p, "ggplot")
})

test_that("create_heatmap annotates HAB species", {
  wide <- data.frame(
    scientific_name = c("HAB Taxon", "Normal Taxon"),
    `STN1_2022-01-01` = c(10, 20),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  taxa <- data.frame(
    name = c("HAB Taxon", "Normal Taxon"),
    HAB = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )

  p <- create_heatmap(wide, taxa_lookup = taxa, title = "HAB Test")
  expect_s3_class(p, "ggplot")
  # Check caption mentions harmful taxon annotation
  expect_true(any(grepl("harmful", p$labels$caption)))
})

test_that("create_stacked_bar returns ggplot", {
  wide <- data.frame(
    scientific_name = paste0("Taxon_", 1:12),
    `STN1_2022-01-01` = runif(12, 1, 100),
    `STN2_2022-01-02` = runif(12, 1, 100),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  p <- create_stacked_bar(wide, n_top = 5, title = "Test")
  expect_s3_class(p, "ggplot")
})

test_that("create_stacked_bar groups Other correctly", {
  wide <- data.frame(
    scientific_name = paste0("Taxon_", 1:15),
    `STN1_2022-01-01` = seq(15, 1),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  p <- create_stacked_bar(wide, n_top = 3, title = "Test")
  expect_s3_class(p, "ggplot")
})

test_that("create_stacked_bar annotates HAB species", {
  wide <- data.frame(
    scientific_name = c("HAB Taxon", paste0("Taxon_", 1:5)),
    `STN1_2022-01-01` = c(100, 80, 60, 40, 20, 10),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  taxa <- data.frame(
    name = c("HAB Taxon", paste0("Taxon_", 1:5)),
    HAB = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  p <- create_stacked_bar(wide, taxa_lookup = taxa, n_top = 5, title = "Test")
  expect_s3_class(p, "ggplot")
  expect_true(any(grepl("harmful", p$labels$caption)))
})

test_that("create_biomass_maps returns list of ggplots", {
  skip_if_not_installed("rnaturalearthdata")

  station_summary <- data.frame(
    STATION_NAME_SHORT = c("BY5", "BY31"),
    LATITUDE_WGS84_SWEREF99_DD = c(55.25, 58.59),
    LONGITUDE_WGS84_SWEREF99_DD = c(15.98, 18.23),
    median_time = as.POSIXct(c("2022-01-01 10:00", "2022-01-01 12:00")),
    carbon_ug_per_liter = c(10, 20),
    biovolume_mm3_per_liter = c(0.5, 1.0),
    stringsAsFactors = FALSE
  )

  result <- create_biomass_maps(station_summary)
  expect_type(result, "list")
  expect_s3_class(result$biomass_map, "ggplot")
  expect_s3_class(result$chl_map, "ggplot")
})

test_that("create_biomass_maps handles chl_mean column", {
  skip_if_not_installed("rnaturalearthdata")

  station_summary <- data.frame(
    STATION_NAME_SHORT = c("BY5", "BY31"),
    LATITUDE_WGS84_SWEREF99_DD = c(55.25, 58.59),
    LONGITUDE_WGS84_SWEREF99_DD = c(15.98, 18.23),
    median_time = as.POSIXct(c("2022-01-01 10:00", "2022-01-01 12:00")),
    carbon_ug_per_liter = c(10, 20),
    biovolume_mm3_per_liter = c(0.5, 1.0),
    chl_mean = c(2.5, 3.0),
    stringsAsFactors = FALSE
  )

  result <- create_biomass_maps(station_summary)
  expect_type(result, "list")
  expect_s3_class(result$biomass_map, "ggplot")
})

test_that("create_image_count_map returns ggplot", {
  skip_if_not_installed("rnaturalearthdata")

  image_counts <- data.frame(
    latitude = c(55.25, 58.59, 57.0),
    longitude = c(15.98, 18.23, 17.0),
    n_images = c(100, 200, 150),
    ml_analyzed = c(4.5, 5.0, 4.8),
    stringsAsFactors = FALSE
  )

  result <- create_image_count_map(image_counts)
  expect_s3_class(result, "ggplot")
})

test_that("create_heatmap handles no HAB taxa", {
  wide <- data.frame(
    scientific_name = c("Taxon A", "Taxon B"),
    `STN1_2022-01-01` = c(10, 20),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  taxa <- data.frame(
    name = c("Taxon A", "Taxon B"),
    HAB = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  p <- create_heatmap(wide, taxa_lookup = taxa, title = "Test")
  expect_s3_class(p, "ggplot")
  expect_null(p$labels$caption)
})
