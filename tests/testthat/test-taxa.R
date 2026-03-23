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
  # All classes combined
  expect_equal(result$all, c("ClassA", "ClassB", "ClassC", "ClassD", "ClassE"))
})

test_that("build_relabel_choices handles empty inputs", {
  result <- build_relabel_choices()
  expect_equal(length(result$grouped), 0)
  expect_equal(length(result$all), 0)
})

test_that("build_relabel_choices omits empty groups", {
  db <- c("ClassA")
  result <- build_relabel_choices(db, NULL, NULL)

  expect_true("Database classes" %in% names(result$grouped))
  expect_false("Taxa lookup" %in% names(result$grouped))
  expect_false("Custom classes" %in% names(result$grouped))
})

test_that("build_relabel_choices deduplicates across sources", {
  db <- c("Shared")
  taxa <- data.frame(clean_names = "Shared", stringsAsFactors = FALSE)
  custom <- data.frame(clean_names = "Shared", stringsAsFactors = FALSE)

  result <- build_relabel_choices(db, taxa, custom)

  # Shared only appears in DB group
  expect_equal(result$all, "Shared")
  expect_equal(length(result$grouped), 1)
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
