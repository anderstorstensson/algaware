test_that("get_db_path constructs correct path", {
  result <- get_db_path("/some/folder")
  expect_equal(result, "/some/folder/annotations.sqlite")
})

test_that("init_db_schema creates tables", {
  tmp <- tempfile(fileext = ".sqlite")
  on.exit(unlink(tmp), add = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  algaware:::init_db_schema(con)

  tables <- DBI::dbListTables(con)
  expect_true("annotations" %in% tables)
  expect_true("class_lists" %in% tables)
  expect_true("class_taxonomy" %in% tables)
  expect_true("global_class_list" %in% tables)
})

test_that("init_db_schema is idempotent", {
  tmp <- tempfile(fileext = ".sqlite")
  on.exit(unlink(tmp), add = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  algaware:::init_db_schema(con)
  expect_no_error(algaware:::init_db_schema(con))
})

test_that("save_annotations_db returns TRUE for empty input", {
  result <- save_annotations_db(
    tempfile(),
    data.frame(sample_name = character(0), roi_number = integer(0),
               class_name = character(0))
  )
  expect_true(result)
})

test_that("save_annotations_db and load_annotations_db roundtrip", {
  tmp_dir <- file.path(tempdir(), paste0("db_test_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  db_path <- file.path(tmp_dir, "annotations.sqlite")

  annotations <- data.frame(
    sample_name = c("D20220101T000000_IFCB134", "D20220101T000000_IFCB134"),
    roi_number = c(1L, 2L),
    class_name = c("Skeletonema", "Chaetoceros"),
    stringsAsFactors = FALSE
  )

  result <- save_annotations_db(db_path, annotations, annotator = "test_user")
  expect_true(result)

  loaded <- load_annotations_db(db_path)
  expect_equal(nrow(loaded), 2)
  expect_true("Skeletonema" %in% loaded$class_name)
  expect_true("Chaetoceros" %in% loaded$class_name)
  expect_true(all(loaded$annotator == "test_user"))
})

test_that("save_annotations_db validates class names against class list", {
  tmp_dir <- file.path(tempdir(), paste0("db_valid_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  db_path <- file.path(tmp_dir, "annotations.sqlite")

  annotations <- data.frame(
    sample_name = c("sample1", "sample1"),
    roi_number = c(1L, 2L),
    class_name = c("ValidClass", "InvalidClass"),
    stringsAsFactors = FALSE
  )

  expect_warning(
    result <- save_annotations_db(db_path, annotations,
                                   class_list = c("ValidClass")),
    "Rejected annotations with invalid class names"
  )
  expect_true(result)

  loaded <- load_annotations_db(db_path)
  expect_equal(nrow(loaded), 1)
  expect_equal(loaded$class_name, "ValidClass")
})

test_that("load_annotations_db returns empty df for missing file", {
  result <- load_annotations_db("/nonexistent/annotations.sqlite")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true("sample_name" %in% names(result))
})

test_that("load_annotations_db filters by sample_names", {
  tmp_dir <- file.path(tempdir(), paste0("db_filter_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  db_path <- file.path(tmp_dir, "annotations.sqlite")

  annotations <- data.frame(
    sample_name = c("sample_A", "sample_A", "sample_B"),
    roi_number = c(1L, 2L, 1L),
    class_name = c("ClassA", "ClassB", "ClassC"),
    stringsAsFactors = FALSE
  )

  save_annotations_db(db_path, annotations)

  loaded <- load_annotations_db(db_path, sample_names = "sample_A")
  expect_equal(nrow(loaded), 2)
  expect_true(all(loaded$sample_name == "sample_A"))
})

test_that("save and load global_class_list roundtrip", {
  tmp_dir <- file.path(tempdir(), paste0("db_gcl_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  db_path <- file.path(tmp_dir, "annotations.sqlite")

  classes <- c("Skeletonema", "Chaetoceros", "detritus")
  result <- save_global_class_list_db(db_path, classes)
  expect_true(result)

  loaded <- load_global_class_list_db(db_path)
  expect_equal(loaded, classes)
})

test_that("save_global_class_list_db returns TRUE for empty input", {
  result <- save_global_class_list_db(tempfile(), character(0))
  expect_true(result)
  result2 <- save_global_class_list_db(tempfile(), NULL)
  expect_true(result2)
})

test_that("load_global_class_list_db returns NULL for missing file", {
  result <- load_global_class_list_db("/nonexistent/annotations.sqlite")
  expect_null(result)
})

test_that("load_global_class_list_db returns NULL for empty table", {
  tmp_dir <- file.path(tempdir(), paste0("db_empty_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  db_path <- file.path(tmp_dir, "annotations.sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  algaware:::init_db_schema(con)
  DBI::dbDisconnect(con)

  result <- load_global_class_list_db(db_path)
  expect_null(result)
})

test_that("resolve_class_list delegates to load_global_class_list_db", {
  tmp_dir <- file.path(tempdir(), paste0("db_resolve_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  db_path <- file.path(tmp_dir, "annotations.sqlite")
  save_global_class_list_db(db_path, c("A", "B", "C"))

  result <- resolve_class_list(db_path)
  expect_equal(result, c("A", "B", "C"))
})

test_that("save_global_class_list_db replaces existing data", {
  tmp_dir <- file.path(tempdir(), paste0("db_replace_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  db_path <- file.path(tmp_dir, "annotations.sqlite")

  save_global_class_list_db(db_path, c("A", "B"))
  save_global_class_list_db(db_path, c("X", "Y", "Z"))

  result <- load_global_class_list_db(db_path)
  expect_equal(result, c("X", "Y", "Z"))
})

test_that("save_annotations_db stores class_lists per sample", {
  tmp_dir <- file.path(tempdir(), paste0("db_classlist_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  db_path <- file.path(tmp_dir, "annotations.sqlite")

  annotations <- data.frame(
    sample_name = c("sample1", "sample1"),
    roi_number = c(1L, 2L),
    class_name = c("ClassA", "ClassB"),
    stringsAsFactors = FALSE
  )

  save_annotations_db(db_path, annotations,
                       class_list = c("ClassA", "ClassB", "ClassC"))

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  cl <- DBI::dbGetQuery(con,
    "SELECT * FROM class_lists WHERE sample_name = 'sample1' ORDER BY class_index")
  expect_equal(nrow(cl), 3)
  expect_equal(cl$class_name, c("ClassA", "ClassB", "ClassC"))
})

test_that("save_annotations_db rejects all-invalid annotations", {
  tmp_dir <- file.path(tempdir(), paste0("db_allinvalid_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  db_path <- file.path(tmp_dir, "annotations.sqlite")

  annotations <- data.frame(
    sample_name = "sample1",
    roi_number = 1L,
    class_name = "InvalidClass",
    stringsAsFactors = FALSE
  )

  expect_warning(
    result <- save_annotations_db(db_path, annotations,
                                   class_list = c("ValidOnly")),
    "Rejected"
  )
  expect_true(result)
})

test_that("save_annotations_db backfills missing ROIs as unclassified", {
  tmp_dir <- file.path(tempdir(), paste0("db_backfill_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  db_path <- file.path(tmp_dir, "annotations.sqlite")

  annotations <- data.frame(
    sample_name = "sample1", roi_number = c(2L, 4L),
    class_name = c("ClassA", "ClassA"), stringsAsFactors = FALSE
  )
  # Full ROI set of the sample, annotated ROIs included
  backfill <- data.frame(
    sample_name = "sample1", roi_number = 1:5, stringsAsFactors = FALSE
  )

  result <- save_annotations_db(db_path, annotations, annotator = "anders",
                                backfill_rois = backfill)
  expect_true(result)

  loaded <- load_annotations_db(db_path)
  loaded <- loaded[order(loaded$roi_number), ]
  expect_equal(nrow(loaded), 5)
  expect_equal(loaded$class_name,
               c("unclassified", "ClassA", "unclassified", "ClassA",
                 "unclassified"))
  expect_equal(loaded$is_manual, c(0L, 1L, 0L, 1L, 0L))
  expect_equal(loaded$annotator,
               c("algaware", "anders", "algaware", "anders", "algaware"))
})

test_that("backfill never overwrites existing annotations across saves", {
  tmp_dir <- file.path(tempdir(), paste0("db_backfill2_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  db_path <- file.path(tmp_dir, "annotations.sqlite")
  backfill <- data.frame(
    sample_name = "sample1", roi_number = 1:4, stringsAsFactors = FALSE
  )

  # First save: ROI 1 -> ClassA, rest backfilled unclassified
  save_annotations_db(db_path,
    data.frame(sample_name = "sample1", roi_number = 1L,
               class_name = "ClassA", stringsAsFactors = FALSE),
    annotator = "u1", backfill_rois = backfill)

  # Second save: ROI 2 -> ClassB, backfill runs again
  save_annotations_db(db_path,
    data.frame(sample_name = "sample1", roi_number = 2L,
               class_name = "ClassB", stringsAsFactors = FALSE),
    annotator = "u2", backfill_rois = backfill)

  loaded <- load_annotations_db(db_path)
  loaded <- loaded[order(loaded$roi_number), ]
  # ROI 1 keeps its ClassA annotation, ROI 2 was upgraded from
  # unclassified to ClassB, ROIs 3-4 stay unreviewed
  expect_equal(loaded$class_name,
               c("ClassA", "ClassB", "unclassified", "unclassified"))
  expect_equal(loaded$is_manual, c(1L, 1L, 0L, 0L))
})

test_that("explicit unclassified annotations are allowed and manual", {
  tmp_dir <- file.path(tempdir(), paste0("db_uncl_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  db_path <- file.path(tmp_dir, "annotations.sqlite")

  # "unclassified" is not in the class list but must not be rejected
  annotations <- data.frame(
    sample_name = "sample1", roi_number = 1L,
    class_name = "unclassified", stringsAsFactors = FALSE
  )
  expect_no_warning(
    result <- save_annotations_db(db_path, annotations,
                                  class_list = c("ClassA", "ClassB"))
  )
  expect_true(result)

  loaded <- load_annotations_db(db_path)
  expect_equal(loaded$class_name, "unclassified")
  expect_equal(loaded$is_manual, 1L)
})

test_that("unclassified survives filtering when batch has invalid classes", {
  tmp_dir <- file.path(tempdir(), paste0("db_uncl_mix_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  db_path <- file.path(tmp_dir, "annotations.sqlite")

  # A batch mixing a valid class, an explicit unclassified, and an invalid
  # class: only the invalid row may be dropped by the filter
  annotations <- data.frame(
    sample_name = "sample1", roi_number = 1:3,
    class_name = c("ClassA", "unclassified", "BadClass"),
    stringsAsFactors = FALSE
  )
  expect_warning(
    result <- save_annotations_db(db_path, annotations,
                                  class_list = c("ClassA", "ClassB")),
    "BadClass"
  )
  expect_true(result)

  loaded <- load_annotations_db(db_path)
  loaded <- loaded[order(loaded$roi_number), ]
  expect_equal(loaded$roi_number, c(1L, 2L))
  expect_equal(loaded$class_name, c("ClassA", "unclassified"))
  expect_equal(loaded$is_manual, c(1L, 1L))
})

test_that("init_db_schema migrates existing db without is_manual", {
  tmp <- tempfile(fileext = ".sqlite")
  on.exit(unlink(tmp), add = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  DBI::dbExecute(con, "
    CREATE TABLE annotations (
      sample_name TEXT NOT NULL,
      roi_number INTEGER NOT NULL,
      class_name TEXT NOT NULL,
      annotator TEXT,
      timestamp TEXT,
      PRIMARY KEY (sample_name, roi_number)
    )
  ")
  DBI::dbExecute(con, "CREATE TABLE class_lists (sample_name TEXT, class_index INTEGER, class_name TEXT)")
  DBI::dbExecute(con, "CREATE TABLE class_taxonomy (class_name TEXT PRIMARY KEY)")
  DBI::dbExecute(con, "CREATE TABLE global_class_list (class_index INTEGER PRIMARY KEY, class_name TEXT)")

  algaware:::init_db_schema(con)

  cols <- DBI::dbGetQuery(con, "PRAGMA table_info(annotations)")
  expect_true("is_manual" %in% cols$name)

  DBI::dbDisconnect(con)
})

test_that("save_annotations_db upserts (replaces on conflict)", {
  tmp_dir <- file.path(tempdir(), paste0("db_upsert_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  db_path <- file.path(tmp_dir, "annotations.sqlite")

  ann1 <- data.frame(
    sample_name = "sample1", roi_number = 1L,
    class_name = "ClassA", stringsAsFactors = FALSE
  )
  save_annotations_db(db_path, ann1, annotator = "user1")

  ann2 <- data.frame(
    sample_name = "sample1", roi_number = 1L,
    class_name = "ClassB", stringsAsFactors = FALSE
  )
  save_annotations_db(db_path, ann2, annotator = "user2")

  loaded <- load_annotations_db(db_path)
  expect_equal(nrow(loaded), 1)
  expect_equal(loaded$class_name, "ClassB")
  expect_equal(loaded$annotator, "user2")
})
