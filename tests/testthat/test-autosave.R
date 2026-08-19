# ---- autosave_corrections ----

make_corrections <- function(n = 2) {
  data.frame(
    sample_name = rep("D20221023T000155_IFCB134", n),
    roi_number = seq_len(n),
    original_class = rep("ClassA", n),
    new_class = rep("ClassB", n),
    stringsAsFactors = FALSE
  )
}

empty_custom_classes <- data.frame(
  clean_names = character(0), name = character(0), sflag = character(0),
  AphiaID = integer(0), HAB = logical(0), italic = logical(0),
  is_diatom = logical(0), stringsAsFactors = FALSE
)

test_that("autosave_corrections writes the enriched CSV under corrections/", {
  storage <- withr::local_tempdir()

  result <- autosave_corrections(make_corrections(), empty_custom_classes,
                                 storage)

  expect_true(result$success)
  expect_null(result$error)
  expected <- file.path(
    storage, "corrections",
    paste0("algaware_corrections_", format(Sys.Date(), "%Y%m%d"), ".csv")
  )
  expect_equal(result$path, expected)
  expect_true(file.exists(expected))

  df <- utils::read.csv(expected, stringsAsFactors = FALSE)
  # Same 9-column schema as the manual "Download corrections" export
  expect_equal(
    names(df),
    c("sample_name", "roi_number", "original_class", "new_class",
      "custom_sci_name", "custom_sflag", "custom_aphia_id", "custom_hab",
      "custom_italic", "custom_is_diatom")
  )
  expect_equal(nrow(df), 2L)
  # Contains everything the import path requires
  expect_true(all(
    c("sample_name", "roi_number", "original_class", "new_class") %in%
      names(df)
  ))
})

test_that("autosave_corrections embeds custom class metadata", {
  storage <- withr::local_tempdir()
  corrections <- make_corrections(1)
  corrections$new_class <- "My_custom"
  custom <- data.frame(
    clean_names = "My_custom", name = "My custom", sflag = "spp.",
    AphiaID = 123L, HAB = TRUE, italic = TRUE, is_diatom = TRUE,
    stringsAsFactors = FALSE
  )

  result <- autosave_corrections(corrections, custom, storage)
  expect_true(result$success)

  df <- utils::read.csv(result$path, stringsAsFactors = FALSE)
  expect_equal(df$custom_sci_name, "My custom")
  expect_equal(df$custom_aphia_id, 123L)
  expect_true(df$custom_hab)
  expect_true(df$custom_is_diatom)
})

test_that("autosave_corrections overwrites an earlier save the same day", {
  storage <- withr::local_tempdir()

  first <- autosave_corrections(make_corrections(1), empty_custom_classes,
                                storage)
  second <- autosave_corrections(make_corrections(5), empty_custom_classes,
                                 storage)

  expect_true(second$success)
  expect_equal(first$path, second$path)
  df <- utils::read.csv(second$path, stringsAsFactors = FALSE)
  expect_equal(nrow(df), 5L)
  # No stray temp files left behind
  leftovers <- list.files(file.path(storage, "corrections"))
  expect_equal(leftovers, basename(second$path))
})

test_that("autosave_corrections skips empty corrections and empty path", {
  storage <- withr::local_tempdir()

  result <- autosave_corrections(make_corrections(0), empty_custom_classes,
                                 storage)
  expect_false(result$success)
  expect_null(result$error)
  expect_false(dir.exists(file.path(storage, "corrections")))

  result <- autosave_corrections(NULL, empty_custom_classes, storage)
  expect_false(result$success)

  result <- autosave_corrections(make_corrections(), empty_custom_classes, "")
  expect_false(result$success)
  result <- autosave_corrections(make_corrections(), empty_custom_classes,
                                 NULL)
  expect_false(result$success)
})

test_that("autosave_corrections rejects malformed storage paths quietly", {
  # These can only come from a hand-edited/corrupted settings file; they
  # must return a failure result, not throw (a throw would kill the Shiny
  # session) and not write anywhere (NA would create a literal 'NA/' dir).
  for (bad in list(NA, NA_character_, character(0), c("a", "b"), 1L)) {
    result <- autosave_corrections(make_corrections(), empty_custom_classes,
                                   bad)
    expect_false(result$success)
    expect_null(result$error)
  }
  expect_false(dir.exists("NA"))
})

test_that("first save of a session sets an existing file aside as _prev", {
  storage <- withr::local_tempdir()

  # A previous session (e.g. one that crashed) left an autosave behind
  old <- autosave_corrections(make_corrections(3), empty_custom_classes,
                              storage)
  expect_true(old$success)

  # New session: first save must not clobber the recovery file
  result <- autosave_corrections(make_corrections(1), empty_custom_classes,
                                 storage, backup_existing = TRUE)
  expect_true(result$success)

  prev <- sub("\\.csv$", "_prev.csv", result$path)
  expect_true(file.exists(prev))
  expect_equal(nrow(utils::read.csv(prev)), 3L)
  expect_equal(nrow(utils::read.csv(result$path)), 1L)

  # Subsequent saves in the same session overwrite in place, leaving _prev
  result <- autosave_corrections(make_corrections(2), empty_custom_classes,
                                 storage, backup_existing = FALSE)
  expect_true(result$success)
  expect_equal(nrow(utils::read.csv(result$path)), 2L)
  expect_equal(nrow(utils::read.csv(prev)), 3L)
})

test_that("autosave_corrections leaves no temp file behind on write failure", {
  skip_on_os("windows")
  storage <- withr::local_tempdir()
  autosave_dir <- file.path(storage, "corrections")
  dir.create(autosave_dir)
  Sys.chmod(autosave_dir, mode = "0500")
  withr::defer(Sys.chmod(autosave_dir, mode = "0700"))
  skip_if(file.access(autosave_dir, mode = 2) == 0,  # e.g. running as root
          "cannot make directory read-only")

  # suppressWarnings: the failing write.csv warns before erroring
  result <- suppressWarnings(
    autosave_corrections(make_corrections(), empty_custom_classes, storage)
  )

  expect_false(result$success)
  expect_type(result$error, "character")
  Sys.chmod(autosave_dir, mode = "0700")
  expect_equal(list.files(autosave_dir), character(0))
})

test_that("autosave_corrections reports failure instead of erroring", {
  storage <- withr::local_tempdir()
  # Occupy the corrections path with a *file* so dir.create/write must fail
  file.create(file.path(storage, "corrections"))

  result <- autosave_corrections(make_corrections(), empty_custom_classes,
                                 storage)

  expect_false(result$success)
  expect_type(result$error, "character")
  expect_match(result$path, "algaware_corrections_")
})

# ---- observer wiring in mod_validation_server ----

test_that("navigating to another class auto-saves changed corrections", {
  storage <- withr::local_tempdir()
  rv <- shiny::reactiveValues(
    data_loaded = TRUE,
    corrections = make_corrections(2),
    custom_classes = empty_custom_classes,
    current_class_idx = 1L,
    current_region = "EAST",
    selected_images = character(0),
    invalidated_classes = character(0)
  )
  config <- shiny::reactiveValues(
    local_storage_path = storage, db_folder = "", annotator = ""
  )

  shiny::testServer(mod_validation_server,
                    args = list(rv = rv, config = config), {
    target <- file.path(
      storage, "corrections",
      paste0("algaware_corrections_", format(Sys.Date(), "%Y%m%d"), ".csv")
    )

    # Consume the observer's initial run (skipped via ignoreInit); in the
    # real app this happens on the session's first reactive flush.
    session$flushReact()
    expect_false(file.exists(target))

    # Navigating to another class triggers a save
    rv$current_class_idx <- 2L
    session$flushReact()
    expect_true(file.exists(target))
    expect_equal(nrow(utils::read.csv(target)), 2L)

    # Unchanged log: navigation does not rewrite the file
    unlink(target)
    rv$current_class_idx <- 3L
    session$flushReact()
    expect_false(file.exists(target))

    # Changed log: next navigation saves again
    rv$corrections <- make_corrections(4)
    rv$current_region <- "WEST"
    session$flushReact()
    expect_true(file.exists(target))
    expect_equal(nrow(utils::read.csv(target)), 4L)
  })
})

test_that("autosave observer does nothing before data is loaded", {
  storage <- withr::local_tempdir()
  rv <- shiny::reactiveValues(
    data_loaded = FALSE,
    corrections = make_corrections(2),
    custom_classes = empty_custom_classes,
    current_class_idx = 1L,
    current_region = "EAST",
    selected_images = character(0),
    invalidated_classes = character(0)
  )
  config <- shiny::reactiveValues(
    local_storage_path = storage, db_folder = "", annotator = ""
  )

  shiny::testServer(mod_validation_server,
                    args = list(rv = rv, config = config), {
    session$flushReact()  # consume the ignoreInit run
    rv$current_class_idx <- 2L
    session$flushReact()
    expect_false(dir.exists(file.path(storage, "corrections")))
  })
})

test_that("autosaved CSV round-trips through the import reader", {
  storage <- withr::local_tempdir()
  corrections <- make_corrections(3)
  corrections$new_class[3] <- "unclassified"

  result <- autosave_corrections(corrections, empty_custom_classes, storage)
  expect_true(result$success)

  # Mirror the import path's read (mod_validation.R import observer)
  df <- as_utf8_columns(
    utils::read.csv(result$path, stringsAsFactors = FALSE, encoding = "UTF-8")
  )
  df$roi_number <- as.integer(df$roi_number)
  expect_equal(df$sample_name, corrections$sample_name)
  expect_equal(df$roi_number, corrections$roi_number)
  expect_equal(df$new_class, corrections$new_class)
})

test_that("custom classes round-trip through autosave including is_diatom", {
  storage <- withr::local_tempdir()
  corrections <- make_corrections(1)
  corrections$new_class <- "My_diatom"
  custom <- data.frame(
    clean_names = "My_diatom", name = "My diatom", sflag = "",
    AphiaID = 77L, HAB = FALSE, italic = TRUE, is_diatom = TRUE,
    stringsAsFactors = FALSE
  )

  result <- autosave_corrections(corrections, custom, storage)
  expect_true(result$success)

  df <- utils::read.csv(result$path, stringsAsFactors = FALSE)
  rebuilt <- custom_classes_from_corrections(df, known_classes = character(0))
  expect_equal(rebuilt, custom)
})
