#' Get path to the annotations SQLite database
#'
#' Uses the same database file as ClassiPyR for annotation compatibility.
#'
#' @param db_folder Path to the database directory.
#' @return Path to the SQLite database file.
#' @export
get_db_path <- function(db_folder) {
  file.path(db_folder, "annotations.sqlite")
}

#' Initialize the annotations database schema
#'
#' Creates tables compatible with ClassiPyR's schema.
#'
#' @param con A DBI connection object.
#' @return Invisible NULL.
#' @keywords internal
init_db_schema <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS annotations (
      sample_name TEXT NOT NULL,
      roi_number  INTEGER NOT NULL,
      class_name  TEXT NOT NULL,
      annotator   TEXT,
      timestamp   TEXT DEFAULT (datetime('now')),
      is_manual   INTEGER NOT NULL DEFAULT 1,
      PRIMARY KEY (sample_name, roi_number)
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS class_lists (
      sample_name TEXT NOT NULL,
      class_index INTEGER NOT NULL,
      class_name  TEXT NOT NULL,
      PRIMARY KEY (sample_name, class_index)
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS class_taxonomy (
      class_name      TEXT PRIMARY KEY,
      aphia_id        TEXT NOT NULL,
      scientific_name TEXT,
      accepted_name   TEXT,
      accepted_aphia_id TEXT,
      updated_at      TEXT DEFAULT (datetime('now'))
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS global_class_list (
      class_index INTEGER PRIMARY KEY,
      class_name  TEXT NOT NULL
    )
  ")

  # Migration: add is_manual column to existing databases

  cols <- DBI::dbGetQuery(con, "PRAGMA table_info(annotations)")
  if (!"is_manual" %in% cols$name) {
    DBI::dbExecute(con,
      "ALTER TABLE annotations ADD COLUMN is_manual INTEGER NOT NULL DEFAULT 1")
  }

  invisible(NULL)
}

#' Save selected annotations to SQLite
#'
#' Stores annotations for selected images. Compatible with ClassiPyR's
#' annotation format.
#'
#' @param db_path Path to the SQLite database file.
#' @param annotations A data.frame with columns: \code{sample_name},
#'   \code{roi_number}, \code{class_name}.
#' @param annotator Name of the annotator.
#' @param class_list Character vector of all class names (for class_lists
#'   table).
#' @return Logical TRUE on success, FALSE on failure.
#' @export
save_annotations_db <- function(db_path, annotations, annotator = "",
                                class_list = character(0)) {
  if (nrow(annotations) == 0) return(TRUE)

  # Validate class names against class list
  if (length(class_list) > 0) {
    invalid <- setdiff(unique(annotations$class_name), class_list)
    if (length(invalid) > 0) {
      warning("Rejected annotations with invalid class names: ",
              paste(invalid, collapse = ", "), call. = FALSE)
      annotations <- annotations[annotations$class_name %in% class_list, ]
      if (nrow(annotations) == 0) return(TRUE)
    }
  }

  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  init_db_schema(con)

  tryCatch({
    DBI::dbExecute(con, "BEGIN TRANSACTION")

    # Upsert annotations
    for (i in seq_len(nrow(annotations))) {
      DBI::dbExecute(con, "
        INSERT OR REPLACE INTO annotations
          (sample_name, roi_number, class_name, annotator, timestamp, is_manual)
        VALUES (?, ?, ?, ?, datetime('now'), 1)
      ", params = list(
        annotations$sample_name[i],
        as.integer(annotations$roi_number[i]),
        annotations$class_name[i],
        annotator
      ))
    }

    # Save class list per sample
    if (length(class_list) > 0) {
      samples <- unique(annotations$sample_name)
      for (samp in samples) {
        DBI::dbExecute(con,
          "DELETE FROM class_lists WHERE sample_name = ?",
          params = list(samp))
        for (j in seq_along(class_list)) {
          DBI::dbExecute(con, "
            INSERT INTO class_lists (sample_name, class_index, class_name)
            VALUES (?, ?, ?)
          ", params = list(samp, j, class_list[j]))
        }
      }
    }

    DBI::dbExecute(con, "COMMIT")
    TRUE
  }, error = function(e) {
    tryCatch(DBI::dbExecute(con, "ROLLBACK"), error = function(e2) NULL)
    warning("Failed to save annotations: ", e$message, call. = FALSE)
    FALSE
  })
}

#' Load annotations from SQLite
#'
#' @param db_path Path to the SQLite database file.
#' @param sample_names Optional character vector of sample names to filter.
#' @return A data.frame of annotations.
#' @export
load_annotations_db <- function(db_path, sample_names = NULL) {
  if (!file.exists(db_path)) {
    return(data.frame(sample_name = character(0),
                      roi_number = integer(0),
                      class_name = character(0),
                      annotator = character(0),
                      timestamp = character(0),
                      stringsAsFactors = FALSE))
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  init_db_schema(con)

  if (!is.null(sample_names) && length(sample_names) > 0) {
    placeholders <- paste(rep("?", length(sample_names)), collapse = ", ")
    query <- paste0("SELECT * FROM annotations WHERE sample_name IN (",
                    placeholders, ")")
    DBI::dbGetQuery(con, query, params = as.list(sample_names))
  } else {
    DBI::dbGetQuery(con, "SELECT * FROM annotations")
  }
}

#' Save global class list to SQLite
#'
#' Replaces the contents of the \code{global_class_list} table.
#' Compatible with ClassiPyR's global_class_list table.
#'
#' @param db_path Path to the SQLite database file.
#' @param class2use Character vector of class names.
#' @return Logical TRUE on success, FALSE on failure.
#' @export
save_global_class_list_db <- function(db_path, class2use) {
  if (is.null(class2use) || length(class2use) == 0) {
    return(TRUE)
  }

  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  init_db_schema(con)

  tryCatch({
    DBI::dbExecute(con, "BEGIN TRANSACTION")
    DBI::dbExecute(con, "DELETE FROM global_class_list")
    for (i in seq_along(class2use)) {
      DBI::dbExecute(con, "
        INSERT INTO global_class_list (class_index, class_name)
        VALUES (?, ?)
      ", params = list(i, class2use[i]))
    }
    DBI::dbExecute(con, "COMMIT")
    TRUE
  }, error = function(e) {
    tryCatch(DBI::dbExecute(con, "ROLLBACK"), error = function(re) NULL)
    warning("Failed to save global class list: ", e$message, call. = FALSE)
    FALSE
  })
}

#' Load global class list from SQLite
#'
#' Returns the class list stored in the \code{global_class_list} table,
#' ordered by class_index. Returns NULL if the table is empty or the
#' database does not exist.
#'
#' @param db_path Path to the SQLite database file.
#' @return Character vector of class names, or NULL if unavailable.
#' @export
load_global_class_list_db <- function(db_path) {
  if (!file.exists(db_path)) {
    return(NULL)
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  init_db_schema(con)

  tryCatch({
    df <- DBI::dbGetQuery(con,
      "SELECT class_name FROM global_class_list ORDER BY class_index")
    if (nrow(df) == 0) NULL else df$class_name
  }, error = function(e) {
    warning("Failed to load global class list: ", e$message, call. = FALSE)
    NULL
  })
}

#' Load a class list from a text file
#'
#' Reads one class name per line from a \code{.txt} file.
#'
#' @param path Path to the class list file.
#' @return Character vector of class names.
#' @export
load_class_list_file <- function(path) {
  if (!file.exists(path)) {
    warning("Class list file not found: ", path, call. = FALSE)
    return(character(0))
  }
  classes <- readLines(path, warn = FALSE)
  classes <- trimws(classes)
  classes[nzchar(classes)]
}

#' Resolve the active class list
#'
#' Loads the class list using the following priority:
#' 1. SQLite global_class_list table (if database exists and has entries)
#' 2. Class list file from settings (if path is set and file exists)
#' 3. NULL (no class list available)
#'
#' @param db_path Path to the SQLite database file.
#' @param class_list_path Path to a class list text file.
#' @return Character vector of class names, or NULL.
#' @export
resolve_class_list <- function(db_path, class_list_path = "") {
  # Priority 1: database
  db_classes <- load_global_class_list_db(db_path)
  if (!is.null(db_classes) && length(db_classes) > 0) {
    return(db_classes)
  }

  # Priority 2: file
  if (nzchar(class_list_path) && file.exists(class_list_path)) {
    file_classes <- load_class_list_file(class_list_path)
    if (length(file_classes) > 0) {
      # Also persist to database for next time
      save_global_class_list_db(db_path, file_classes)
      return(file_classes)
    }
  }

  NULL
}
