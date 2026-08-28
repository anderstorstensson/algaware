# Helpers -------------------------------------------------------------------

make_cache <- function() {
  list(
    roi_biovolumes = data.frame(
      sample = c("s1", "s1", "s1", "s2", "s2"),
      roi_number = c(1L, 2L, 3L, 1L, 2L),
      biovolume_px = c(1000, 2000, 3000, 4000, NA),
      stringsAsFactors = FALSE
    ),
    sample_volumes = data.frame(
      sample = c("s1", "s2"),
      ml_analyzed = c(5, NA),
      stringsAsFactors = FALSE
    )
  )
}

make_classifications <- function() {
  data.frame(
    sample_name = c("s1", "s1", "s1", "s2", "s2"),
    roi_number = c(1L, 2L, 3L, 1L, 2L),
    class_name = c("Skeletonema_marinoi", "Skeletonema_marinoi",
                   "Dinophysis_acuta", "Dinophysis_acuta", "Dinophysis_acuta"),
    stringsAsFactors = FALSE
  )
}

make_taxa_lookup <- function() {
  data.frame(
    clean_names = c("Skeletonema_marinoi", "Dinophysis_acuta"),
    name = c("Skeletonema marinoi", "Dinophysis acuta"),
    AphiaID = c(149151L, 109604L),
    stringsAsFactors = FALSE
  )
}

no_worms_status <- function(classes) {
  data.frame(class = classes, worms_diatom = FALSE, stringsAsFactors = FALSE)
}

# Carbon formulas ------------------------------------------------------------

test_that("carbon formulas match the iRfcb implementations", {
  volumes <- c(0.5, 1, 10, 1234.5, 1e6)
  expect_equal(algaware:::vol2c_diatom_large(volumes),
               getFromNamespace("vol2C_lgdiatom", "iRfcb")(volumes))
  expect_equal(algaware:::vol2c_nondiatom(volumes),
               getFromNamespace("vol2C_nondiatom", "iRfcb")(volumes))
})

# feature_sample_name --------------------------------------------------------

test_that("feature_sample_name strips known suffixes", {
  expect_equal(
    algaware:::feature_sample_name(c(
      "/x/D20250714T110535_IFCB134_fea_v2.csv",
      "D20250714T110535_IFCB134_features.csv",
      "D20250714T110535_IFCB134.csv"
    )),
    rep("D20250714T110535_IFCB134", 3)
  )
})

# summarize_biovolumes_cached ------------------------------------------------

test_that("summarize_biovolumes_cached aggregates counts, biovolume, carbon", {
  ppm <- 2.77
  mf <- 1 / ppm
  result <- summarize_biovolumes_cached(
    make_cache(), make_classifications(), make_taxa_lookup(),
    pixels_per_micron = ppm,
    diatom_status = no_worms_status(c("Skeletonema_marinoi",
                                      "Dinophysis_acuta"))
  )

  s1_skel <- result[result$sample == "s1" &
                      result$class == "Skeletonema_marinoi", ]
  expect_equal(s1_skel$counts, 2L)
  um3 <- c(1000, 2000) * mf^3
  expect_equal(s1_skel$biovolume_mm3, sum(um3) * 1e-9)
  # Skeletonema matches the local diatom genus list -> diatom formula
  expect_equal(s1_skel$carbon_ug,
               sum(algaware:::vol2c_diatom_large(um3)) * 1e-6)
  expect_equal(s1_skel$counts_per_liter, 2 / (5 / 1000))
  expect_equal(s1_skel$biovolume_mm3_per_liter, sum(um3) * 1e-9 / (5 / 1000))

  s1_dino <- result[result$sample == "s1" &
                      result$class == "Dinophysis_acuta", ]
  dino_um3 <- 3000 * mf^3
  expect_equal(s1_dino$carbon_ug,
               algaware:::vol2c_nondiatom(dino_um3) * 1e-6)

  # Taxonomy joined
  expect_equal(s1_skel$name, "Skeletonema marinoi")
  expect_equal(s1_skel$AphiaID, 149151L)
})

test_that("summarize_biovolumes_cached counts ROIs with NA biovolume", {
  result <- summarize_biovolumes_cached(
    make_cache(), make_classifications(), make_taxa_lookup(),
    diatom_status = no_worms_status(c("Skeletonema_marinoi",
                                      "Dinophysis_acuta"))
  )
  s2 <- result[result$sample == "s2", ]
  # Both s2 ROIs counted although one has NA biovolume
  expect_equal(s2$counts, 2L)
  expect_false(is.na(s2$biovolume_mm3))
  # NA ml_analyzed -> NA per-liter values
  expect_true(is.na(s2$counts_per_liter))
})

test_that("relabelling changes the carbon formula but not the biovolume", {
  ppm <- 2.77
  mf <- 1 / ppm
  status <- no_worms_status(c("Skeletonema_marinoi", "Dinophysis_acuta"))

  before <- summarize_biovolumes_cached(
    make_cache(), make_classifications(), make_taxa_lookup(),
    pixels_per_micron = ppm, diatom_status = status
  )

  # Relabel s1 ROI 3 from Dinophysis (non-diatom) to Skeletonema (diatom)
  corrected <- make_classifications()
  corrected$class_name[3] <- "Skeletonema_marinoi"
  after <- summarize_biovolumes_cached(
    make_cache(), corrected, make_taxa_lookup(),
    pixels_per_micron = ppm, diatom_status = status
  )

  expect_false("Dinophysis_acuta" %in%
                 after$class[after$sample == "s1"])
  s1_after <- after[after$sample == "s1", ]
  expect_equal(s1_after$counts, 3L)

  um3 <- c(1000, 2000, 3000) * mf^3
  expect_equal(s1_after$biovolume_mm3, sum(um3) * 1e-9)
  expect_equal(s1_after$carbon_ug,
               sum(algaware:::vol2c_diatom_large(um3)) * 1e-6)

  # Total biovolume across classes is unchanged by the relabel
  expect_equal(sum(after$biovolume_mm3[after$sample == "s1"]),
               sum(before$biovolume_mm3[before$sample == "s1"]))
})

test_that("summarize_biovolumes_cached drops samples absent from classifications", {
  # Excluding s2: its classification rows are filtered out upstream
  cls <- make_classifications()
  cls <- cls[cls$sample_name != "s2", ]
  result <- summarize_biovolumes_cached(
    make_cache(), cls, make_taxa_lookup(),
    diatom_status = no_worms_status(c("Skeletonema_marinoi",
                                      "Dinophysis_acuta"))
  )
  expect_false("s2" %in% result$sample)
  expect_true("s1" %in% result$sample)
})

test_that("summarize_biovolumes_cached excludes non-biological classes", {
  cls <- make_classifications()
  cls$class_name[cls$sample_name == "s2"] <- "detritus"
  result <- summarize_biovolumes_cached(
    make_cache(), cls, make_taxa_lookup(),
    non_bio_classes = "detritus",
    diatom_status = no_worms_status(c("Skeletonema_marinoi",
                                      "Dinophysis_acuta", "detritus"))
  )
  expect_false("detritus" %in% result$class)
})

test_that("summarize_biovolumes_cached uses WoRMS status and custom diatoms", {
  cls <- make_classifications()
  cls$class_name <- "Mystery_taxon"
  ppm <- 2.77
  taxa <- data.frame(clean_names = "Mystery_taxon", name = "Mystery taxon",
                     AphiaID = 1L, stringsAsFactors = FALSE)

  worms_diatom <- data.frame(class = "Mystery_taxon", worms_diatom = TRUE,
                             stringsAsFactors = FALSE)
  result <- summarize_biovolumes_cached(
    make_cache(), cls, taxa, pixels_per_micron = ppm,
    diatom_status = worms_diatom
  )
  s1 <- result[result$sample == "s1", ]
  expect_equal(s1$carbon_ug,
               sum(algaware:::vol2c_diatom_large(c(1000, 2000, 3000) *
                                                   (1 / ppm)^3)) * 1e-6)

  # Same class flagged non-diatom by WoRMS but included via custom classes
  custom <- data.frame(clean_names = "Mystery_taxon", is_diatom = TRUE,
                       stringsAsFactors = FALSE)
  result2 <- summarize_biovolumes_cached(
    make_cache(), cls, taxa, pixels_per_micron = ppm,
    diatom_status = no_worms_status("Mystery_taxon"),
    custom_classes = custom
  )
  expect_equal(result2$carbon_ug, result$carbon_ug)
})

test_that("summarize_biovolumes_cached handles empty overlap", {
  cls <- data.frame(sample_name = "s99", roi_number = 1L,
                    class_name = "Skeletonema_marinoi",
                    stringsAsFactors = FALSE)
  result <- summarize_biovolumes_cached(
    make_cache(), cls, make_taxa_lookup(),
    diatom_status = no_worms_status("Skeletonema_marinoi")
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

# resolve_diatom_status ------------------------------------------------------

test_that("resolve_diatom_status only looks up unknown classes", {
  lookup_calls <- list()
  mockery::stub(resolve_diatom_status, "worms_diatom_lookup",
                function(classes) {
                  lookup_calls[[length(lookup_calls) + 1]] <<- classes
                  rep(FALSE, length(classes))
                })

  cached <- data.frame(class = "Known_class", worms_diatom = TRUE,
                       stringsAsFactors = FALSE)
  result <- resolve_diatom_status(c("Known_class", "New_class"), cached)

  expect_equal(length(lookup_calls), 1)
  expect_equal(lookup_calls[[1]], "New_class")
  expect_true(result$worms_diatom[result$class == "Known_class"])
  expect_false(result$worms_diatom[result$class == "New_class"])
})

test_that("resolve_diatom_status retries classes with failed (NA) lookups", {
  mockery::stub(resolve_diatom_status, "worms_diatom_lookup",
                function(classes) rep(TRUE, length(classes)))
  cached <- data.frame(class = c("Failed_class", "Known_class"),
                       worms_diatom = c(NA, FALSE),
                       stringsAsFactors = FALSE)
  result <- resolve_diatom_status(c("Failed_class", "Known_class"), cached)
  expect_true(result$worms_diatom[result$class == "Failed_class"])
  expect_false(result$worms_diatom[result$class == "Known_class"])
})

test_that("resolve_diatom_status skips lookup when everything is cached", {
  mockery::stub(resolve_diatom_status, "worms_diatom_lookup",
                function(classes) stop("should not be called"))
  cached <- data.frame(class = "A", worms_diatom = FALSE,
                       stringsAsFactors = FALSE)
  expect_silent(result <- resolve_diatom_status("A", cached))
  expect_equal(nrow(result), 1)
})

test_that("worms_diatom_lookup returns NA on lookup failure", {
  mockery::stub(worms_diatom_lookup, "iRfcb::ifcb_is_diatom",
                function(...) stop("offline"))
  expect_warning(result <- worms_diatom_lookup(c("A", "B")), "WoRMS")
  expect_equal(result, c(NA, NA))
})

# build_biovolume_cache ------------------------------------------------------

test_that("build_biovolume_cache reads feature files for requested samples", {
  feat_dir <- withr::local_tempdir()
  raw_dir <- withr::local_tempdir()

  write.csv(data.frame(roi_number = 1:3, Biovolume = c(100, 200, 300)),
            file.path(feat_dir, "D20250714T110535_IFCB134_fea_v2.csv"),
            row.names = FALSE)
  write.csv(data.frame(roi_number = 1:2, Biovolume = c(400, 500)),
            file.path(feat_dir, "D20250714T120000_IFCB134_fea_v2.csv"),
            row.names = FALSE)

  mockery::stub(build_biovolume_cache, "read_sample_volumes",
                function(hdr_folder, sample_ids) {
                  data.frame(sample = sample_ids, ml_analyzed = 5,
                             stringsAsFactors = FALSE)
                })

  cache <- build_biovolume_cache(feat_dir, raw_dir,
                                 "D20250714T110535_IFCB134")

  expect_equal(unique(cache$roi_biovolumes$sample),
               "D20250714T110535_IFCB134")
  expect_equal(cache$roi_biovolumes$roi_number, 1:3)
  expect_equal(cache$roi_biovolumes$biovolume_px, c(100, 200, 300))
  expect_equal(cache$sample_volumes$ml_analyzed, 5)
})

test_that("build_biovolume_cache returns empty structures without files", {
  feat_dir <- withr::local_tempdir()
  raw_dir <- withr::local_tempdir()
  cache <- build_biovolume_cache(feat_dir, raw_dir, "D20250714T110535_IFCB134")
  expect_equal(nrow(cache$roi_biovolumes), 0)
  expect_equal(nrow(cache$sample_volumes), 0)
})

test_that("volume_analyzed_safe returns NA with warning on bad file", {
  expect_warning(
    result <- algaware:::volume_analyzed_safe("/nonexistent/file.hdr"),
    "Failed to read"
  )
  expect_true(is.na(result))
})

# recompute_biovolume_data ---------------------------------------------------

test_that("recompute_biovolume_data uses the cache when available", {
  rv <- new.env()
  rv$biovolume_cache <- make_cache()
  rv$classifications <- make_classifications()
  rv$custom_classes <- NULL
  rv$diatom_status <- no_worms_status(c("Skeletonema_marinoi",
                                        "Dinophysis_acuta"))
  config <- list(local_storage_path = "/nonexistent",
                 pixels_per_micron = 2.77)

  mockery::stub(recompute_biovolume_data, "summarize_biovolumes",
                function(...) stop("file-based path should not be used"))

  result <- recompute_biovolume_data(rv, config, make_taxa_lookup())
  expect_true(nrow(result) > 0)
  expect_true(all(c("counts", "biovolume_mm3", "carbon_ug") %in%
                    names(result)))
})

test_that("recompute_biovolume_data falls back without a cache", {
  rv <- new.env()
  rv$biovolume_cache <- NULL
  rv$classifications <- make_classifications()
  rv$custom_classes <- NULL
  config <- list(local_storage_path = "/tmp/storage",
                 pixels_per_micron = 2.77)

  called_args <- NULL
  mockery::stub(recompute_biovolume_data, "summarize_biovolumes",
                function(feature_folder, hdr_folder, ...) {
                  called_args <<- c(feature_folder, hdr_folder)
                  data.frame()
                })

  recompute_biovolume_data(rv, config, make_taxa_lookup())
  expect_equal(called_args,
               c("/tmp/storage/features", "/tmp/storage/raw"))
})
