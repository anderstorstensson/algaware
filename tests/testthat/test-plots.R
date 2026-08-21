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

test_that("create_biomass_maps handles all-NA chl_mean column", {
  skip_if_not_installed("rnaturalearthdata")

  # Ferrybox folder present but no valid chlorophyll readings: the chl_mean
  # column exists but is entirely NA. Aggregation must not abort with
  # "no rows to aggregate".
  station_summary <- data.frame(
    STATION_NAME_SHORT = c("BY5", "BY31"),
    LATITUDE_WGS84_SWEREF99_DD = c(55.25, 58.59),
    LONGITUDE_WGS84_SWEREF99_DD = c(15.98, 18.23),
    median_time = as.POSIXct(c("2022-01-01 10:00", "2022-01-01 12:00")),
    carbon_ug_per_liter = c(10, 20),
    biovolume_mm3_per_liter = c(0.5, 1.0),
    chl_mean = c(NA_real_, NA_real_),
    stringsAsFactors = FALSE
  )

  result <- create_biomass_maps(station_summary)
  expect_type(result, "list")
  expect_s3_class(result$biomass_map, "ggplot")
  expect_s3_class(result$chl_map, "ggplot")
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

test_that("order_taxa_by_group sorts by group then alphabetically", {
  groups <- data.frame(
    name = c("Skeletonema", "Dinophysis", "Aphanizomenon", "Chaetoceros"),
    phyto_group = c("Diatoms", "Dinoflagellates", "Cyanobacteria", "Diatoms"),
    stringsAsFactors = FALSE
  )
  result <- algaware:::order_taxa_by_group(
    c("Dinophysis", "Skeletonema", "Unknown thing", "Aphanizomenon",
      "Chaetoceros"),
    groups
  )
  expect_equal(result, c("Chaetoceros", "Skeletonema",   # Diatoms, A->Z
                         "Dinophysis",                   # Dinoflagellates
                         "Aphanizomenon",                # Cyanobacteria
                         "Unknown thing"))               # Other last
})

test_that("order_taxa_by_group matches names carrying an sflag suffix", {
  groups <- data.frame(
    name = c("Chaetoceros", "Tripos"),
    phyto_group = c("Diatoms", "Dinoflagellates"),
    stringsAsFactors = FALSE
  )
  result <- algaware:::order_taxa_by_group(
    c("Tripos spp.", "Chaetoceros spp."), groups
  )
  expect_equal(result, c("Chaetoceros spp.", "Tripos spp."))
})

test_that("create_heatmap orders taxa by group when phyto_groups given", {
  wide <- data.frame(
    scientific_name = c("Dinophysis", "Skeletonema", "Aphanizomenon"),
    `STN1_2022-01-01` = c(100, 1, 10),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  groups <- data.frame(
    name = c("Dinophysis", "Skeletonema", "Aphanizomenon"),
    phyto_group = c("Dinoflagellates", "Diatoms", "Cyanobacteria"),
    stringsAsFactors = FALSE
  )
  p <- create_heatmap(wide, phyto_groups = groups, title = "Test")
  expect_s3_class(p, "ggplot")
  # Factor levels run bottom-to-top: Diatoms must be the LAST level (top row)
  y_levels <- levels(p$data$scientific_name)
  expect_equal(y_levels, c("Aphanizomenon", "Dinophysis", "Skeletonema"))
})

test_that("create_heatmap keeps biovolume ordering without phyto_groups", {
  wide <- data.frame(
    scientific_name = c("Low", "High"),
    `STN1_2022-01-01` = c(1, 100),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  p <- create_heatmap(wide, title = "Test")
  expect_s3_class(p, "ggplot")
  expect_equal(levels(p$data$scientific_name), c("High", "Low"))
})

test_that("heatmap_group_of folds Mesodinium spp. into Ciliates", {
  groups <- data.frame(
    name = c("Mesodinium rubrum", "Strombidium", "Skeletonema"),
    phyto_group = c("Mesodinium spp.", "Ciliates", "Diatoms"),
    stringsAsFactors = FALSE
  )
  result <- algaware:::heatmap_group_of(
    c("Mesodinium rubrum", "Strombidium spp.", "Skeletonema", "Nope"),
    groups
  )
  expect_equal(result, c("Ciliates", "Ciliates", "Diatoms", "Other"))
})

test_that("order_taxa_by_group keeps all ciliates together", {
  groups <- data.frame(
    name = c("Mesodinium rubrum", "Strombidium", "Dictyocha", "Skeletonema"),
    phyto_group = c("Mesodinium spp.", "Ciliates", "Silicoflagellates",
                    "Diatoms"),
    stringsAsFactors = FALSE
  )
  result <- algaware:::order_taxa_by_group(
    c("Dictyocha", "Strombidium", "Skeletonema", "Mesodinium rubrum"),
    groups
  )
  expect_equal(result, c("Skeletonema", "Mesodinium rubrum", "Strombidium",
                         "Dictyocha"))
})

test_that("create_heatmap facets by group with coloured strips", {
  wide <- data.frame(
    scientific_name = c("Dinophysis", "Skeletonema", "Mesodinium rubrum"),
    `STN1_2022-01-01` = c(100, 1, 10),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  groups <- data.frame(
    name = c("Dinophysis", "Skeletonema", "Mesodinium rubrum"),
    phyto_group = c("Dinoflagellates", "Diatoms", "Mesodinium spp."),
    stringsAsFactors = FALSE
  )
  p <- create_heatmap(wide, phyto_groups = groups)
  expect_s3_class(p$facet, "FacetGrid")
  built <- ggplot2::ggplot_build(p)
  panels <- built$layout$layout$phyto_group
  expect_equal(as.character(panels),
               c("Diatoms", "Dinoflagellates", "Ciliates"))
  # No facets without groups
  p0 <- create_heatmap(wide)
  expect_s3_class(p0$facet, "FacetNull")
})

test_that("phyto_group_colors covers all heatmap and pie groups", {
  pal <- algaware:::phyto_group_colors()
  expect_true(all(c("Diatoms", "Dinoflagellates", "Cyanobacteria",
                    "Cryptophytes", "Mesodinium spp.", "Ciliates",
                    "Silicoflagellates", "Other") %in% names(pal)))
})

test_that("heatmap_x_labels builds three-line and compact two-line labels", {
  x <- c("BY31_2026-08-01", "ANHOLT E_2026-08-02")
  n <- c(`BY31_2026-08-01` = 3L)
  expect_equal(algaware:::heatmap_x_labels(x),
               c("BY31\n2026-08-01", "ANHOLT E\n2026-08-02"))
  expect_equal(algaware:::heatmap_x_labels(x, n),
               c("BY31\n2026-08-01\nn = 3", "ANHOLT E\n2026-08-02"))
  expect_equal(algaware:::heatmap_x_labels(x, n, compact = TRUE),
               c("BY31 (n = 3)\n2026-08-01", "ANHOLT E\n2026-08-02"))
})

test_that("create_heatmap colours HAB labels via markdown, not a colour vector", {
  wide <- data.frame(
    scientific_name = c("Dinophysis", "Skeletonema", "Attheya"),
    `STN1_2022-01-01` = c(1, 2, 3),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  taxa <- data.frame(name = c("Dinophysis", "Skeletonema", "Attheya"),
                     HAB = c(TRUE, FALSE, FALSE))
  groups <- data.frame(
    name = c("Dinophysis", "Skeletonema", "Attheya"),
    phyto_group = c("Dinoflagellates", "Diatoms", "Diatoms"),
    stringsAsFactors = FALSE
  )
  p <- create_heatmap(wide, taxa_lookup = taxa, phyto_groups = groups)
  labels <- p$scales$get_scales("y")$labels
  expect_match(labels[["Dinophysis"]], "color:red.*Dinophysis\\*")
  expect_false(grepl("red", labels[["Attheya"]]))
  expect_false(grepl("red", labels[["Skeletonema"]]))
  expect_s3_class(p$theme$axis.text.y.left, "element_markdown")
  expect_equal(p$theme$legend.position, "top")
})

test_that("escape_markdown_label escapes HTML-significant characters", {
  expect_equal(escape_markdown_label(c("Foo <sp>", "A & B", "x>y", "plain")),
               c("Foo &lt;sp&gt;", "A &amp; B", "x&gt;y", "plain"))
})

test_that("create_heatmap renders taxon names containing angle brackets", {
  wide <- data.frame(
    scientific_name = c("Baz <sp>", "Normal Taxon"),
    `STN1_2022-01-01` = c(10, 20),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  taxa <- data.frame(
    name = c("Baz <sp>", "Normal Taxon"),
    HAB = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  p <- create_heatmap(wide, taxa_lookup = taxa, title = "Escape Test")
  tmp <- withr::local_tempfile(fileext = ".png")
  expect_no_error(ggplot2::ggsave(tmp, p, width = 5, height = 4, dpi = 50))
  expect_true(file.exists(tmp))
})
