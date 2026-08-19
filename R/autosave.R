#' Auto-save the corrections log to the local storage path
#'
#' Writes the same enriched corrections CSV that the "Download corrections"
#' button produces (see \code{enrich_corrections_for_export()}) to
#' \code{<storage_path>/corrections/algaware_corrections_<YYYYMMDD>.csv},
#' so that work lost to a crash -- or to closing the app without
#' downloading -- can be recovered with the existing "Import corrections"
#' button. The corrections log is cumulative, so each save overwrites the
#' day's file with the complete state.
#'
#' The CSV is first written to a temporary file in the same folder and then
#' renamed into place, so a crash mid-write can never leave a truncated
#' file where a previous good autosave was.
#'
#' @param corrections Data frame of corrections (from \code{rv$corrections}).
#' @param custom_classes Data frame of custom classes
#'   (from \code{rv$custom_classes}).
#' @param storage_path The local storage path (from settings).
#' @param backup_existing Set \code{TRUE} on a session's first save: an
#'   already-existing target file must then come from an earlier session
#'   (e.g. one that crashed), so it is set aside as
#'   \code{..._prev.csv} instead of being overwritten. Later saves in the
#'   same session overwrite in place.
#' @return A list with \code{success} (logical), \code{path} (the file
#'   written, or the intended target on failure) and \code{error} (message,
#'   or NULL). Returns \code{success = FALSE} without writing when there is
#'   nothing to save or no storage path is configured.
#' @keywords internal
autosave_corrections <- function(corrections, custom_classes, storage_path,
                                 backup_existing = FALSE) {
  if (is.null(corrections) || !is.data.frame(corrections) ||
      nrow(corrections) == 0 ||
      !is.character(storage_path) || length(storage_path) != 1L ||
      is.na(storage_path) || !nzchar(storage_path)) {
    return(list(success = FALSE, path = NULL, error = NULL))
  }

  autosave_dir <- file.path(storage_path, "corrections")
  target <- file.path(
    autosave_dir,
    paste0("algaware_corrections_", format(Sys.Date(), "%Y%m%d"), ".csv")
  )

  tryCatch({
    if (!dir.exists(autosave_dir)) {
      dir.create(autosave_dir, recursive = TRUE, showWarnings = FALSE)
    }
    if (!dir.exists(autosave_dir)) {
      stop("could not create folder '", autosave_dir, "'")
    }

    if (backup_existing && file.exists(target)) {
      prev <- sub("\\.csv$", "_prev.csv", target)
      unlink(prev)
      if (!suppressWarnings(file.rename(target, prev))) {
        file.copy(target, prev, overwrite = TRUE)
      }
    }

    enriched <- enrich_corrections_for_export(corrections, custom_classes)
    tmp <- tempfile("algaware_autosave_", tmpdir = autosave_dir,
                    fileext = ".csv")
    # Clean up the temp file however this function exits; a no-op once it
    # has been renamed into place.
    on.exit(unlink(tmp), add = TRUE)
    utils::write.csv(enriched, tmp, row.names = FALSE, fileEncoding = "UTF-8")
    # file.rename is atomic within a filesystem but cannot overwrite an
    # existing target on Windows; fall back to copy-over there.
    if (!suppressWarnings(file.rename(tmp, target))) {
      if (!file.copy(tmp, target, overwrite = TRUE)) {
        stop("could not write '", target, "'")
      }
    }
    list(success = TRUE, path = target, error = NULL)
  }, error = function(e) {
    list(success = FALSE, path = target, error = conditionMessage(e))
  })
}
