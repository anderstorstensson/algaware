test_that("load_taxa_lookup returns a data.frame", {
  lookup <- load_taxa_lookup()
  expect_s3_class(lookup, "data.frame")
  expect_true("clean_names" %in% names(lookup))
  expect_true("name" %in% names(lookup))
  expect_true("AphiaID" %in% names(lookup))
  expect_true(nrow(lookup) > 0)
})

# -- build_relabel_choices -------------------------------------------------

test_that("build_relabel_choices groups DB, taxa, and custom classes", {
  db <- c("ClassA", "ClassB", "unclassified")
  taxa <- data.frame(clean_names = c("ClassA", "ClassC", "ClassD"),
                     stringsAsFactors = FALSE)
  custom <- data.frame(clean_names = "ClassE", stringsAsFactors = FALSE)

  result <- build_relabel_choices(db, taxa, custom)

  expect_true("Database classes" %in% names(result$grouped))
  expect_true("Taxa lookup" %in% names(result$grouped))
  expect_true("Custom classes" %in% names(result$grouped))

  # DB group excludes unclassified

  expect_equal(result$grouped[["Database classes"]], c("ClassA", "ClassB"))
  # Taxa group excludes classes already in DB
  expect_equal(result$grouped[["Taxa lookup"]], c("ClassC", "ClassD"))
  # Custom group excludes classes in DB or taxa
  expect_equal(result$grouped[["Custom classes"]], "ClassE")
  # Other group includes unclassified
  expect_true("Other" %in% names(result$grouped))
  expect_equal(result$grouped[["Other"]], "unclassified")
  # All classes combined (including unclassified)
  expect_equal(result$all,
               c("ClassA", "ClassB", "ClassC", "ClassD", "ClassE", "unclassified"))
})

test_that("build_relabel_choices handles empty inputs", {
  result <- build_relabel_choices()
  # Still has the "Other" group with unclassified
  expect_equal(length(result$grouped), 1)
  expect_equal(result$grouped[["Other"]], "unclassified")
  expect_equal(result$all, "unclassified")
})

test_that("build_relabel_choices omits empty groups except Other", {
  db <- c("ClassA")
  result <- build_relabel_choices(db, NULL, NULL)

  expect_true("Database classes" %in% names(result$grouped))
  expect_false("Taxa lookup" %in% names(result$grouped))
  expect_false("Custom classes" %in% names(result$grouped))
  expect_true("Other" %in% names(result$grouped))
})

test_that("build_relabel_choices deduplicates across sources", {
  db <- c("Shared")
  taxa <- data.frame(clean_names = "Shared", stringsAsFactors = FALSE)
  custom <- data.frame(clean_names = "Shared", stringsAsFactors = FALSE)

  result <- build_relabel_choices(db, taxa, custom)

  # Shared only appears in DB group, plus Other
  expect_equal(result$all, c("Shared", "unclassified"))
  expect_equal(length(result$grouped), 2)
})

# -- merge_custom_taxa -----------------------------------------------------

test_that("merge_custom_taxa appends new classes", {
  taxa <- data.frame(
    clean_names = "Existing",
    name = "Existing sp.",
    AphiaID = 100L,
    HAB = FALSE,
    italic = TRUE,
    stringsAsFactors = FALSE
  )
  custom <- data.frame(
    clean_names = "NewTaxon",
    name = "New taxon",
    AphiaID = 999L,
    HAB = TRUE,
    italic = TRUE,
    is_diatom = FALSE,
    stringsAsFactors = FALSE
  )

  result <- merge_custom_taxa(taxa, custom)
  expect_equal(nrow(result), 2)
  expect_true("NewTaxon" %in% result$clean_names)
})

test_that("merge_custom_taxa does not duplicate existing classes", {
  taxa <- data.frame(
    clean_names = "ClassA",
    name = "A",
    AphiaID = 1L,
    HAB = FALSE,
    italic = TRUE,
    stringsAsFactors = FALSE
  )
  custom <- data.frame(
    clean_names = "ClassA",
    name = "A different",
    AphiaID = 2L,
    HAB = FALSE,
    italic = TRUE,
    is_diatom = FALSE,
    stringsAsFactors = FALSE
  )

  result <- merge_custom_taxa(taxa, custom)
  expect_equal(nrow(result), 1)
  expect_equal(result$name, "A")
})

test_that("format_taxon_labels italicizes and appends sflag", {
  taxa <- data.frame(
    name = "Pseudo-nitzschia seriata",
    sflag = "cf.",
    italic = TRUE,
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_taxon_labels("Pseudo-nitzschia seriata cf.", taxa)
  expect_match(result[["Pseudo-nitzschia seriata cf."]], "<i>")
  expect_match(result[["Pseudo-nitzschia seriata cf."]], "cf\\.")
})

test_that("format_taxon_labels returns plain when format='plain'", {
  taxa <- data.frame(
    name = "Skeletonema marinoi",
    sflag = "",
    italic = TRUE,
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_taxon_labels("Skeletonema marinoi", taxa,
                                            format = "plain")
  expect_equal(result[["Skeletonema marinoi"]], "Skeletonema marinoi")
})

# -- enrich_corrections_for_export -------------------------------------------

test_that("enrich_corrections_for_export adds custom metadata when matched", {
  corrections <- data.frame(
    new_class = c("MyAlga", "unclassified"),
    stringsAsFactors = FALSE
  )
  custom_classes <- data.frame(
    clean_names = "MyAlga",
    name = "My alga sp.",
    sflag = "",
    AphiaID = 999L,
    HAB = FALSE,
    italic = TRUE,
    stringsAsFactors = FALSE
  )
  result <- algaware:::enrich_corrections_for_export(corrections, custom_classes)
  expect_equal(result$custom_sci_name[result$new_class == "MyAlga"], "My alga sp.")
  expect_equal(result$custom_aphia_id[result$new_class == "MyAlga"], 999L)
  expect_true(is.na(result$custom_sci_name[result$new_class == "unclassified"]))
})

test_that("enrich_corrections_for_export returns unchanged when no custom_classes", {
  corrections <- data.frame(new_class = "SomeTaxon", stringsAsFactors = FALSE)
  result <- algaware:::enrich_corrections_for_export(corrections, NULL)
  expect_true("custom_sci_name" %in% names(result))
  expect_true(is.na(result$custom_sci_name))
})

test_that("enrich_corrections_for_export returns unchanged when no matches", {
  corrections <- data.frame(new_class = "SomeTaxon", stringsAsFactors = FALSE)
  custom_classes <- data.frame(
    clean_names = "OtherTaxon", name = "Other taxon",
    sflag = "", AphiaID = 1L, HAB = FALSE, italic = TRUE,
    stringsAsFactors = FALSE
  )
  result <- algaware:::enrich_corrections_for_export(corrections, custom_classes)
  expect_true(is.na(result$custom_sci_name))
})

test_that("enrich_corrections_for_export carries the is_diatom flag", {
  corrections <- data.frame(
    new_class = c("MyDiatom", "SomeTaxon"),
    stringsAsFactors = FALSE
  )
  custom_classes <- data.frame(
    clean_names = "MyDiatom", name = "My diatom", sflag = "",
    AphiaID = 1L, HAB = FALSE, italic = TRUE, is_diatom = TRUE,
    stringsAsFactors = FALSE
  )
  result <- algaware:::enrich_corrections_for_export(corrections, custom_classes)
  expect_true(result$custom_is_diatom[result$new_class == "MyDiatom"])
  expect_true(is.na(result$custom_is_diatom[result$new_class == "SomeTaxon"]))
})

# -- custom_classes_from_corrections -----------------------------------------

make_import_df <- function(is_diatom = NULL) {
  df <- data.frame(
    sample_name = "samp1", roi_number = 1L,
    original_class = "ClassA", new_class = "MyAlga",
    custom_sci_name = "My alga", custom_sflag = "spp.",
    custom_aphia_id = 42L, custom_hab = TRUE, custom_italic = TRUE,
    stringsAsFactors = FALSE
  )
  if (!is.null(is_diatom)) df$custom_is_diatom <- is_diatom
  df
}

test_that("custom_classes_from_corrections rebuilds a custom class", {
  result <- algaware:::custom_classes_from_corrections(
    make_import_df(is_diatom = TRUE), known_classes = character(0)
  )
  expect_equal(nrow(result), 1L)
  expect_equal(result$clean_names, "MyAlga")
  expect_equal(result$name, "My alga")
  expect_equal(result$AphiaID, 42L)
  expect_true(result$HAB)
  expect_true(result$is_diatom)
})

test_that("custom_classes_from_corrections defaults is_diatom to FALSE for old files", {
  # Files written before the custom_is_diatom column existed
  result <- algaware:::custom_classes_from_corrections(
    make_import_df(is_diatom = NULL), known_classes = character(0)
  )
  expect_equal(nrow(result), 1L)
  expect_false(result$is_diatom)

  # NA values (e.g. an all-NA column read as logical) also default to FALSE
  result <- algaware:::custom_classes_from_corrections(
    make_import_df(is_diatom = NA), known_classes = character(0)
  )
  expect_false(result$is_diatom)
})

test_that("custom_classes_from_corrections skips known classes and plain files", {
  result <- algaware:::custom_classes_from_corrections(
    make_import_df(is_diatom = TRUE), known_classes = "MyAlga"
  )
  expect_equal(nrow(result), 0L)

  # Corrections files without any custom columns (hand-made 4-column CSV)
  plain <- data.frame(
    sample_name = "samp1", roi_number = 1L,
    original_class = "ClassA", new_class = "ClassB",
    stringsAsFactors = FALSE
  )
  result <- algaware:::custom_classes_from_corrections(
    plain, known_classes = character(0)
  )
  expect_equal(nrow(result), 0L)
})

test_that("merge_custom_taxa returns taxa_lookup unchanged with no custom", {
  taxa <- data.frame(
    clean_names = "X", name = "X", AphiaID = 1L,
    HAB = FALSE, italic = TRUE, stringsAsFactors = FALSE
  )
  expect_identical(merge_custom_taxa(taxa, NULL), taxa)
  expect_identical(
    merge_custom_taxa(taxa, data.frame(clean_names = character(0),
                                        stringsAsFactors = FALSE)),
    taxa
  )
})
