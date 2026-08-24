test_that("apply_invalidation replaces classes with unclassified", {
  classifications <- data.frame(
    sample_name = c("s1", "s1", "s1"),
    roi_number = c(1L, 2L, 3L),
    class_name = c("Skeletonema", "detritus", "Chaetoceros"),
    stringsAsFactors = FALSE
  )

  result <- algaware:::apply_invalidation(classifications, c("detritus"))
  expect_equal(result$class_name, c("Skeletonema", "unclassified", "Chaetoceros"))
})

test_that("apply_invalidation does not modify original", {
  classifications <- data.frame(
    sample_name = "s1",
    roi_number = 1L,
    class_name = "detritus",
    stringsAsFactors = FALSE
  )

  result <- algaware:::apply_invalidation(classifications, c("detritus"))
  expect_equal(classifications$class_name, "detritus")
  expect_equal(result$class_name, "unclassified")
})

test_that("apply_invalidation with empty invalidated list", {
  classifications <- data.frame(
    sample_name = c("s1", "s1"),
    roi_number = c(1L, 2L),
    class_name = c("A", "B"),
    stringsAsFactors = FALSE
  )

  result <- algaware:::apply_invalidation(classifications, character(0))
  expect_equal(result$class_name, c("A", "B"))
})

test_that("identify_diatom_classes finds known diatoms", {
  taxa_lookup <- data.frame(
    clean_names = c("Skeletonema_marinoi", "Chaetoceros_sp",
                    "Dinophysis_sp", "Navicula_sp"),
    stringsAsFactors = FALSE
  )

  result <- algaware:::identify_diatom_classes(taxa_lookup)
  expect_true("Skeletonema_marinoi" %in% result)
  expect_true("Chaetoceros_sp" %in% result)
  expect_true("Navicula_sp" %in% result)
  expect_false("Dinophysis_sp" %in% result)
})

test_that("create_wide_summary handles empty data", {
  station_summary <- data.frame(
    COAST = character(0),
    STATION_NAME_SHORT = character(0),
    visit_date = character(0),
    name = character(0),
    biovolume_mm3_per_liter = numeric(0),
    stringsAsFactors = FALSE
  )

  result <- create_wide_summary(station_summary, "EAST")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("create_wide_summary pivots correctly", {
  station_summary <- data.frame(
    COAST = c("EAST", "EAST", "EAST", "EAST"),
    STATION_NAME_SHORT = c("BY5", "BY5", "BY31", "BY31"),
    visit_date = c("2022-01-01", "2022-01-01", "2022-01-02", "2022-01-02"),
    name = c("Taxon A", "Taxon B", "Taxon A", "Taxon B"),
    biovolume_mm3_per_liter = c(10, 20, 30, 40),
    stringsAsFactors = FALSE
  )

  result <- create_wide_summary(station_summary, "EAST")
  expect_true("scientific_name" %in% names(result))
  expect_equal(nrow(result), 2)
  expect_true(ncol(result) > 1)
})

test_that("create_wide_summary filters by coast", {
  station_summary <- data.frame(
    COAST = c("EAST", "WEST"),
    STATION_NAME_SHORT = c("BY5", "ANHOLT"),
    visit_date = c("2022-01-01", "2022-01-01"),
    name = c("Taxon A", "Taxon B"),
    biovolume_mm3_per_liter = c(10, 20),
    stringsAsFactors = FALSE
  )

  result <- create_wide_summary(station_summary, "WEST")
  expect_equal(nrow(result), 1)
})

test_that("collect_ferrybox_data returns empty df for invalid path", {
  result <- collect_ferrybox_data(
    as.POSIXct(c("2022-01-01", "2022-01-02")),
    ""
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("collect_ferrybox_data returns empty df for missing dir", {
  result <- collect_ferrybox_data(
    as.POSIXct(c("2022-01-01", "2022-01-02")),
    "/nonexistent/ferrybox/path"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("compute_visit_dates picks most common date", {
  all_data <- data.frame(
    visit_id = c("STN_A_visit1", "STN_A_visit1", "STN_A_visit1"),
    STATION_NAME = c("STN_A", "STN_A", "STN_A"),
    sample_date = as.Date(c("2022-01-01", "2022-01-01", "2022-01-02")),
    stringsAsFactors = FALSE
  )

  result <- algaware:::compute_visit_dates(all_data)
  expect_equal(nrow(result), 1)
  expect_equal(result$visit_date, as.Date("2022-01-01"))
})

test_that("compute_sample_volumes aggregates correctly", {
  all_data <- data.frame(
    sample = c("s1", "s2", "s3"),
    visit_id = c("STN_A_visit1", "STN_A_visit1", "STN_B_visit1"),
    STATION_NAME = c("STN_A", "STN_A", "STN_B"),
    ml_analyzed = c(3.0, 2.0, 5.0),
    sample_time = as.POSIXct(c("2022-01-01 10:00", "2022-01-01 11:00",
                                "2022-01-01 12:00")),
    stringsAsFactors = FALSE
  )

  result <- algaware:::compute_sample_volumes(all_data)
  expect_equal(nrow(result), 2)
  stn_a <- result[result$STATION_NAME == "STN_A", ]
  expect_equal(stn_a$total_ml_analyzed, 5.0)
  expect_equal(stn_a$n_samples, 2L)
  stn_b <- result[result$STATION_NAME == "STN_B", ]
  expect_equal(stn_b$n_samples, 1L)
})

test_that("compute_per_liter calculates concentrations", {
  agg <- data.frame(
    total_counts = c(100, 200),
    total_biovolume_mm3 = c(0.5, 1.0),
    total_carbon_ug = c(10, 20),
    total_ml_analyzed = c(5000, 0),
    stringsAsFactors = FALSE
  )

  result <- algaware:::compute_per_liter(agg)
  expect_equal(result$counts_per_liter[1], 100 / 5)
  expect_true(is.na(result$counts_per_liter[2]))
  expect_equal(result$biovolume_mm3_per_liter[1], 0.5 / 5)
  expect_equal(result$carbon_ug_per_liter[1], 10 / 5)
})

test_that("compute_presence_categories assigns correct categories", {
  agg <- data.frame(
    visit_id = rep("v1", 5),
    STATION_NAME = rep("STN_A", 5),
    counts_per_liter = c(600, 100, 10, 1, 0),
    stringsAsFactors = FALSE
  )

  result <- algaware:::compute_presence_categories(agg)
  # Total = 711; pcts ~ 84.4%, 14.1%, 1.4%, 0.14%, 0%
  expect_equal(result$Presence_cat, c(5L, 4L, 3L, 2L, 0L))
})

test_that("compute_presence_categories handles zero total", {
  agg <- data.frame(
    visit_id = "v1",
    STATION_NAME = "STN_A",
    counts_per_liter = 0,
    stringsAsFactors = FALSE
  )

  result <- algaware:::compute_presence_categories(agg)
  expect_equal(result$pct, 0)
  expect_equal(result$Presence_cat, 0L)
})

test_that("identify_diatom_classes returns empty for no matches", {
  taxa_lookup <- data.frame(
    clean_names = c("Dinophysis_sp", "Alexandrium_sp"),
    stringsAsFactors = FALSE
  )
  result <- algaware:::identify_diatom_classes(taxa_lookup)
  expect_length(result, 0)
})

test_that("build_sample_counts creates named vector", {
  station_summary <- data.frame(
    STATION_NAME_SHORT = c("BY5", "BY5", "BY31"),
    visit_date = c("2022-01-01", "2022-01-01", "2022-01-02"),
    n_samples = c(3L, 3L, 5L),
    name = c("Taxon A", "Taxon B", "Taxon A"),
    stringsAsFactors = FALSE
  )
  result <- algaware:::build_sample_counts(station_summary)
  expect_equal(result[["BY5_2022-01-01"]], 3L)
  expect_equal(result[["BY31_2022-01-02"]], 5L)
})

test_that("build_sample_counts returns NULL without n_samples column", {
  station_summary <- data.frame(
    STATION_NAME_SHORT = "BY5",
    visit_date = "2022-01-01",
    stringsAsFactors = FALSE
  )
  result <- algaware:::build_sample_counts(station_summary)
  expect_null(result)
})

test_that("create_heatmap with sample_counts adds n = X labels", {
  wide <- data.frame(
    scientific_name = c("Taxon A", "Taxon B"),
    `STN1_2022-01-01` = c(10, 20),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  counts <- c("STN1_2022-01-01" = 5L)
  p <- create_heatmap(wide, sample_counts = counts)
  expect_s3_class(p, "ggplot")
})

test_that("create_wide_summary orders columns by date then station", {
  station_summary <- data.frame(
    COAST = rep("EAST", 2),
    STATION_NAME_SHORT = c("BY31", "BY5"),
    visit_date = c("2022-01-02", "2022-01-01"),
    name = c("Taxon A", "Taxon A"),
    biovolume_mm3_per_liter = c(10, 20),
    stringsAsFactors = FALSE
  )

  result <- create_wide_summary(station_summary, "EAST")
  data_cols <- names(result)[-1]
  # BY5_2022-01-01 should come before BY31_2022-01-02
  expect_true(grepl("2022-01-01", data_cols[1]))
  expect_true(grepl("2022-01-02", data_cols[2]))
})

# -- identify_diatom_classes with custom_classes ---------------------------

test_that("identify_diatom_classes detects known diatom patterns", {
  taxa <- data.frame(
    clean_names = c("Skeletonema_marinoi", "Chaetoceros_spp", "Dinophysis"),
    stringsAsFactors = FALSE
  )
  result <- identify_diatom_classes(taxa)
  expect_true("Skeletonema_marinoi" %in% result)
  expect_true("Chaetoceros_spp" %in% result)
  expect_false("Dinophysis" %in% result)
})

test_that("identify_diatom_classes includes custom diatoms", {
  taxa <- data.frame(
    clean_names = c("Skeletonema_marinoi", "Dinophysis"),
    stringsAsFactors = FALSE
  )
  custom <- data.frame(
    clean_names = c("MyDiatom", "MyDino"),
    is_diatom = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  result <- identify_diatom_classes(taxa, custom)
  expect_true("MyDiatom" %in% result)
  expect_false("MyDino" %in% result)
  expect_true("Skeletonema_marinoi" %in% result)
})

test_that("identify_diatom_classes works without custom_classes", {
  taxa <- data.frame(
    clean_names = c("Thalassiosira_spp"),
    stringsAsFactors = FALSE
  )
  result <- identify_diatom_classes(taxa, NULL)
  expect_equal(result, "Thalassiosira_spp")
})

# -- compute_unclassified_fractions -------------------------------------------

test_that("compute_unclassified_fractions computes correct percentages", {
  classifications <- data.frame(
    sample_name = rep(c("s1", "s2"), each = 5),
    class_name = c("ClassA", "ClassA", "unclassified", "unclassified",
                   "unclassified",       # s1: 3/5 = 60% unclassified
                   "unclassified", "unclassified", "unclassified",
                   "unclassified", "ClassB"),  # s2: 4/5 = 80% unclassified
    stringsAsFactors = FALSE
  )
  metadata <- data.frame(
    pid = c("s1", "s2"),
    STATION_NAME = c("STN_A", "STN_A"),
    sample_time = as.POSIXct(c("2024-01-01 10:00", "2024-01-01 11:00")),
    stringsAsFactors = FALSE
  )

  result <- compute_unclassified_fractions(classifications, metadata)
  expect_type(result, "list")
  expect_length(result, 1)
  # Both samples at same station within 12h -> one visit, 7/10 = 70%
  expect_equal(result[[1]], 70)
})

test_that("compute_unclassified_fractions returns 0 when none unclassified", {
  classifications <- data.frame(
    sample_name = "s1",
    class_name = "ClassA",
    stringsAsFactors = FALSE
  )
  metadata <- data.frame(
    pid = "s1",
    STATION_NAME = "STN_A",
    sample_time = as.POSIXct("2024-01-01 10:00"),
    stringsAsFactors = FALSE
  )

  result <- compute_unclassified_fractions(classifications, metadata)
  expect_equal(result[[1]], 0)
})

test_that("compute_unclassified_fractions returns empty list for no data", {
  classifications <- data.frame(
    sample_name = "s1", class_name = "ClassA", stringsAsFactors = FALSE
  )
  metadata <- data.frame(
    pid = "s999",
    STATION_NAME = "STN_A",
    sample_time = as.POSIXct("2024-01-01 10:00"),
    stringsAsFactors = FALSE
  )

  result <- compute_unclassified_fractions(classifications, metadata)
  expect_length(result, 0)
})

test_that("compute_per_liter adds cell_counts_per_liter when totals are present", {
  agg <- data.frame(
    total_counts = c(100, 200),
    total_biovolume_mm3 = c(0.5, 1.0),
    total_carbon_ug = c(10, 20),
    total_ml_analyzed = c(5000, 5000),
    total_cell_counts = c(500, NA_real_),
    stringsAsFactors = FALSE
  )

  result <- algaware:::compute_per_liter(agg)
  expect_equal(result$cell_counts_per_liter[1], 500 / 5)
  expect_true(is.na(result$cell_counts_per_liter[2]))

  # Without the totals column the per-liter column is not invented.
  agg$total_cell_counts <- NULL
  result <- algaware:::compute_per_liter(agg)
  expect_false("cell_counts_per_liter" %in% names(result))
})

test_that("summarize_biovolumes passes chain counts to iRfcb when present", {
  skip_if_not_installed("mockery")

  classifications <- data.frame(
    sample_name = c("D20250101T000000_IFCB134", "D20250101T000000_IFCB134"),
    roi_number = c(1L, 2L),
    class_name = c("Pseudo-nitzschia_spp", "Dinophysis_acuta"),
    score = c(0.9, 0.9),
    cell_count = c(6L, NA_integer_),
    stringsAsFactors = FALSE
  )
  taxa_lookup <- data.frame(
    clean_names = c("Pseudo-nitzschia_spp", "Dinophysis_acuta"),
    name = c("Pseudo-nitzschia", "Dinophysis acuta"),
    AphiaID = c(149151L, 109604L),
    stringsAsFactors = FALSE
  )

  captured <- NULL
  fake_summarize <- function(...) {
    captured <<- list(...)
    data.frame(
      sample = "D20250101T000000_IFCB134",
      class = c("Pseudo-nitzschia_spp", "Dinophysis_acuta"),
      counts = c(1, 1),
      biovolume_mm3 = c(0.1, 0.1),
      carbon_ug = c(1, 1),
      ml_analyzed = c(3, 3),
      cell_counts = c(6, NA_real_),
      stringsAsFactors = FALSE
    )
  }
  mockery::stub(summarize_biovolumes, "iRfcb::ifcb_summarize_biovolumes",
                fake_summarize)

  result <- summarize_biovolumes("feat", "raw", classifications, taxa_lookup)

  expect_true(captured$use_cell_counts)
  expect_equal(captured$carbon_conversion, "cell")
  expect_equal(captured$custom_cell_counts, c(6L, NA_integer_))
  expect_equal(result$cell_counts[result$class == "Pseudo-nitzschia_spp"], 6)
})

test_that("summarize_biovolumes keeps the image-based call without chain counts", {
  skip_if_not_installed("mockery")

  classifications <- data.frame(
    sample_name = "D20240101T000000_IFCB134",
    roi_number = 1L,
    class_name = "Dinophysis_acuta",
    score = 0.9,
    cell_count = NA_integer_,
    stringsAsFactors = FALSE
  )
  taxa_lookup <- data.frame(
    clean_names = "Dinophysis_acuta",
    name = "Dinophysis acuta",
    AphiaID = 109604L,
    stringsAsFactors = FALSE
  )

  captured <- NULL
  fake_summarize <- function(...) {
    captured <<- list(...)
    # iRfcb omits cell_counts entirely when use_cell_counts = FALSE
    data.frame(
      sample = "D20240101T000000_IFCB134",
      class = "Dinophysis_acuta",
      counts = 1,
      biovolume_mm3 = 0.1,
      carbon_ug = 1,
      ml_analyzed = 3,
      stringsAsFactors = FALSE
    )
  }
  mockery::stub(summarize_biovolumes, "iRfcb::ifcb_summarize_biovolumes",
                fake_summarize)

  result <- summarize_biovolumes("feat", "raw", classifications, taxa_lookup)

  expect_false(captured$use_cell_counts)
  expect_equal(captured$carbon_conversion, "roi")
  expect_null(captured$custom_cell_counts)
  # The column is ensured downstream with NA (not 0, which would read as
  # genuine absence).
  expect_true(all(is.na(result$cell_counts)))
})

test_that("aggregate_station_data propagates NA cell counts within a visit", {
  skip_if_not_installed("mockery")

  # Two samples in the same visit: S1 has chain-counter data, S2 does not.
  # The Pseudo-nitzschia visit total must be NA (never a partial sum), while
  # the Dinophysis total (present only in the chain-counted sample) sums.
  biovolume_data <- data.frame(
    sample = c("S1", "S1", "S2"),
    counts = c(10, 5, 20),
    biovolume_mm3 = c(0.1, 0.05, 0.2),
    carbon_ug = c(1, 0.5, 2),
    ml_analyzed = c(3, 3, 5),
    cell_counts = c(40, 5, NA_real_),
    name = c("Pseudo-nitzschia", "Dinophysis acuta", "Pseudo-nitzschia"),
    sflag = c("spp.", "", "spp."),
    AphiaID = c(149151L, 109604L, 149151L),
    stringsAsFactors = FALSE
  )
  metadata <- data.frame(
    pid = c("S1", "S2"),
    STATION_NAME = c("STN_A", "STN_A"),
    STATION_NAME_SHORT = c("A", "A"),
    COAST = c("EAST", "EAST"),
    sample_time = as.POSIXct(c("2026-08-01 10:00:00", "2026-08-01 10:30:00"),
                             tz = "UTC"),
    stringsAsFactors = FALSE
  )
  stations <- data.frame(
    STATION_NAME = "STN_A",
    LATITUDE_WGS84_SWEREF99_DD = 58,
    LONGITUDE_WGS84_SWEREF99_DD = 11,
    stringsAsFactors = FALSE
  )
  mockery::stub(aggregate_station_data, "load_shark_stations", stations)

  result <- aggregate_station_data(biovolume_data, metadata)

  pn <- result[result$name == "Pseudo-nitzschia", ]
  expect_true(is.na(pn$total_cell_counts))
  expect_true(is.na(pn$cell_counts_per_liter))
  # Image-based abundance is unaffected by the missing chain counts.
  expect_equal(pn$total_counts, 30)
  expect_equal(pn$counts_per_liter, 30 / (8 / 1000))

  dino <- result[result$name == "Dinophysis acuta", ]
  expect_equal(dino$total_cell_counts, 5)
  expect_equal(dino$cell_counts_per_liter, 5 / (8 / 1000))
})

test_that("aggregate_station_data handles input without a cell_counts column", {
  skip_if_not_installed("mockery")

  biovolume_data <- data.frame(
    sample = "S1",
    counts = 10,
    biovolume_mm3 = 0.1,
    carbon_ug = 1,
    ml_analyzed = 3,
    name = "Dinophysis acuta",
    sflag = "",
    AphiaID = 109604L,
    stringsAsFactors = FALSE
  )
  metadata <- data.frame(
    pid = "S1",
    STATION_NAME = "STN_A",
    STATION_NAME_SHORT = "A",
    COAST = "EAST",
    sample_time = as.POSIXct("2026-08-01 10:00:00", tz = "UTC"),
    stringsAsFactors = FALSE
  )
  stations <- data.frame(
    STATION_NAME = "STN_A",
    LATITUDE_WGS84_SWEREF99_DD = 58,
    LONGITUDE_WGS84_SWEREF99_DD = 11,
    stringsAsFactors = FALSE
  )
  mockery::stub(aggregate_station_data, "load_shark_stations", stations)

  result <- aggregate_station_data(biovolume_data, metadata)
  expect_true(all(is.na(result$total_cell_counts)))
  expect_true(all(is.na(result$cell_counts_per_liter)))
})
