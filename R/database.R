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

#' Open a connection to an annotations database
#'
#' Sets a busy timeout so a concurrent writer (e.g. ClassiPyR holding a
#' short write lock on the shared database) makes us wait briefly instead
#' of failing immediately with "database is locked".
#'
#' @param db_path Path to the SQLite database file.
#' @return A DBI connection object.
#' @keywords internal
connect_annotations_db <- function(db_path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  tryCatch(DBI::dbExecute(con, "PRAGMA busy_timeout = 5000"),
           error = function(e) NULL)
  con
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

  # Schema migration: older databases (pre-AlgAware) may lack the is_manual
  # column. This check runs every time a connection is opened, which is safe
  # because ALTER TABLE ADD COLUMN is a no-op if the column already exists.
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
  con <- connect_annotations_db(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  tryCatch({
    # DDL inside the tryCatch: on a read-only or locked database it throws,
    # and outside the handler that error would escape to the caller instead
    # of returning FALSE as documented.
    init_db_schema(con)
    DBI::dbExecute(con, "BEGIN TRANSACTION")

    # Upsert annotations: INSERT OR REPLACE inserts new rows or overwrites
    # existing ones matching the PRIMARY KEY (sample_name, roi_number).
    # This means re-annotating the same image updates the record.
    stmt <- DBI::dbSendStatement(con, "
      INSERT OR REPLACE INTO annotations
        (sample_name, roi_number, class_name, annotator, timestamp, is_manual)
      VALUES (?, ?, ?, ?, datetime('now'), 1)
    ")
    for (i in seq_len(nrow(annotations))) {
      DBI::dbBind(stmt, params = list(
        annotations$sample_name[i],
        as.integer(annotations$roi_number[i]),
        annotations$class_name[i],
        annotator
      ))
    }
    DBI::dbClearResult(stmt)

    # Save class list per sample
    if (length(class_list) > 0) {
      samples <- unique(annotations$sample_name)
      for (samp in samples) {
        DBI::dbExecute(con,
          "DELETE FROM class_lists WHERE sample_name = ?",
          params = list(samp))
      }
      cl_stmt <- DBI::dbSendStatement(con, "
        INSERT INTO class_lists (sample_name, class_index, class_name)
        VALUES (?, ?, ?)
      ")
      for (samp in samples) {
        for (j in seq_along(class_list)) {
          DBI::dbBind(cl_stmt, params = list(samp, j, class_list[j]))
        }
      }
      DBI::dbClearResult(cl_stmt)
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

  con <- connect_annotations_db(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Read-only path: no DDL. Running init_db_schema() here failed outright on
  # a read-only or locked database, aborting the caller instead of degrading
  # to "no annotations". A database without the expected table lands in the
  # error handler below.
  tryCatch({
    if (!is.null(sample_names) && length(sample_names) > 0) {
      placeholders <- paste(rep("?", length(sample_names)), collapse = ", ")
      query <- paste0("SELECT * FROM annotations WHERE sample_name IN (",
                      placeholders, ")")
      DBI::dbGetQuery(con, query, params = as.list(sample_names))
    } else {
      DBI::dbGetQuery(con, "SELECT * FROM annotations")
    }
  }, error = function(e) {
    warning("Failed to load annotations: ", e$message, call. = FALSE)
    data.frame(sample_name = character(0),
               roi_number = integer(0),
               class_name = character(0),
               annotator = character(0),
               timestamp = character(0),
               stringsAsFactors = FALSE)
  })
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
  con <- connect_annotations_db(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  tryCatch({
    init_db_schema(con)
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

  con <- connect_annotations_db(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Read-only path: no DDL (see load_annotations_db). init_db_schema() here
  # ran before the tryCatch, so a read-only or locked database aborted the
  # entire cruise load instead of falling back to the auto-generated class
  # list. A database without the table lands in the error handler.
  tryCatch({
    df <- DBI::dbGetQuery(con,
      "SELECT class_name FROM global_class_list ORDER BY class_index")
    if (nrow(df) == 0) NULL else df$class_name
  }, error = function(e) {
    warning("Failed to load global class list: ", e$message, call. = FALSE)
    NULL
  })
}

#' Resolve the active class list
#'
#' Loads the global class list from the SQLite database (shared with
#' ClassiPyR). Returns NULL if the database does not exist or has no
#' class list entries.
#'
#' @param db_path Path to the SQLite database file.
#' @return Character vector of class names, or NULL.
#' @export
resolve_class_list <- function(db_path) {
  load_global_class_list_db(db_path)
}
