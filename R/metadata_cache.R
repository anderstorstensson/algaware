#' Dashboard metadata cache
#'
#' Fetching metadata previously downloaded the full dashboard export on every
#' click, which grows with the archive. These helpers cache the last download
#' as an RDS file in local storage and, on the next fetch, download only bins
#' sampled on or after the newest cached day, replacing the overlap. A full
#' refetch happens automatically when the dashboard URL or dataset changes,
#' or on request (\code{force_full = TRUE}).
#'
#' @name metadata-cache
#' @keywords internal
NULL

#' Path of the metadata cache file inside local storage
#'
#' @param storage_dir Local storage base directory.
#' @return Character path.
#' @keywords internal
metadata_cache_path <- function(storage_dir) {
  file.path(storage_dir, "metadata_cache.rds")
}

#' Delete the metadata cache
#'
#' Used by the "Clear Metadata Cache" button in Settings: with the cache
#' gone, the next Fetch Metadata downloads the complete export again. This
#' is the escape hatch for edits to old bins (e.g. skip flags or cruise
#' numbers changed on the dashboard) that the incremental fetch cannot see.
#'
#' @param storage_dir Local storage base directory.
#' @return Invisible logical: TRUE when a cache file existed and was removed.
#' @export
clear_metadata_cache <- function(storage_dir) {
  cache_file <- metadata_cache_path(storage_dir)
  existed <- file.exists(cache_file)
  if (existed) unlink(cache_file)
  invisible(existed)
}

#' Load cached dashboard metadata
#'
#' @param cache_file Path from \code{metadata_cache_path()}.
#' @param dashboard_url Dashboard base URL the cache must match.
#' @param dataset_name Dataset name the cache must match.
#' @return The cached metadata data.frame, or NULL when there is no usable
#'   cache for this URL/dataset.
#' @keywords internal
load_metadata_cache <- function(cache_file, dashboard_url, dataset_name) {
  if (!file.exists(cache_file)) return(NULL)
  cache <- tryCatch(readRDS(cache_file), error = function(e) NULL)
  if (!is.list(cache) ||
      !identical(cache$dashboard_url, dashboard_url) ||
      !identical(cache$dataset_name, dataset_name) ||
      !is.data.frame(cache$metadata) ||
      nrow(cache$metadata) == 0) {
    return(NULL)
  }
  cache$metadata
}

#' Save dashboard metadata to the cache
#'
#' Failures are downgraded to a warning: a broken cache write must never
#' break the fetch itself.
#'
#' @param cache_file Path from \code{metadata_cache_path()}.
#' @param dashboard_url Dashboard base URL.
#' @param dataset_name Dataset name.
#' @param metadata Metadata data.frame to store.
#' @return Invisible NULL.
#' @keywords internal
save_metadata_cache <- function(cache_file, dashboard_url, dataset_name,
                                metadata) {
  tryCatch({
    dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
    saveRDS(list(
      dashboard_url = dashboard_url,
      dataset_name = dataset_name,
      fetched_at = Sys.time(),
      metadata = metadata
    ), cache_file)
  }, error = function(e) {
    warning("Failed to write metadata cache: ", conditionMessage(e),
            call. = FALSE)
  })
  invisible(NULL)
}

#' Name of the sample-time column in dashboard metadata
#'
#' @param metadata Dashboard metadata data.frame.
#' @return Column name, or NULL when neither candidate exists.
#' @keywords internal
metadata_time_col <- function(metadata) {
  if ("sample_time" %in% names(metadata)) return("sample_time")
  if ("timestamp" %in% names(metadata)) return("timestamp")
  NULL
}

#' Fetch dashboard metadata for a date window
#'
#' Same \code{export_metadata} endpoint and CSV parsing as
#' \code{iRfcb::ifcb_download_dashboard_metadata()} (readr with all-character
#' columns followed by \code{type_convert()}), plus the \code{start_date}/
#' \code{end_date} query parameters the dashboard supports.
#'
#' @param dashboard_url Dashboard base URL.
#' @param dataset_name Dataset name.
#' @param start_date Start date (inclusive).
#' @param end_date End date (inclusive). Defaults to tomorrow so bins
#'   sampled today are always included.
#' @return Metadata data.frame (possibly zero rows).
#' @keywords internal
fetch_metadata_window <- function(dashboard_url, dataset_name,
                                  start_date, end_date = Sys.Date() + 1) {
  api_url <- paste0(
    sub("/+$", "", dashboard_url),
    "/api/export_metadata/"
  )
  if (!is.null(dataset_name) && nzchar(dataset_name)) {
    api_url <- paste0(api_url,
                      utils::URLencode(dataset_name, reserved = TRUE))
  }

  resp <- httr2::request(api_url) |>
    httr2::req_headers(Accept = "text/csv") |>
    httr2::req_url_query(
      start_date = as.character(as.Date(start_date)),
      end_date = as.character(as.Date(end_date))
    ) |>
    httr2::req_timeout(120) |>
    httr2::req_perform()

  csv_content <- httr2::resp_body_string(resp, encoding = "UTF-8")
  df <- readr::read_csv(I(csv_content), show_col_types = FALSE,
                        progress = FALSE,
                        col_types = readr::cols(.default = readr::col_character()))
  as.data.frame(readr::type_convert(df, col_types = readr::cols()))
}

#' Coerce a column to the class of a template column
#'
#' \code{type_convert()} on a small increment can infer a different type
#' than on the full archive (e.g. an all-NA column becomes logical). Coerce
#' towards the cached (template) column so the rows can be bound.
#'
#' @param x Column from the increment.
#' @param template Column from the cached metadata.
#' @return \code{x} coerced to \code{class(template)}.
#' @keywords internal
coerce_like <- function(x, template) {
  if (identical(class(x), class(template))) return(x)
  if (inherits(template, "POSIXct")) {
    tz <- attr(template, "tzone")
    if (is.null(tz) || !nzchar(tz)) tz <- "UTC"
    return(as.POSIXct(x, tz = tz))
  }
  if (inherits(template, "Date")) return(as.Date(x))
  if (is.character(template)) return(as.character(x))
  if (is.integer(template)) return(as.integer(x))
  if (is.numeric(template)) return(as.numeric(x))
  if (is.logical(template)) return(as.logical(x))
  x
}

#' Bind incremental metadata rows onto cached metadata
#'
#' Uses the union of both column sets (missing columns become NA) and
#' coerces increment columns to the cached column types.
#'
#' @param cached Cached metadata data.frame.
#' @param fresh Increment data.frame from \code{fetch_metadata_window()}.
#' @return Combined data.frame.
#' @keywords internal
bind_metadata_rows <- function(cached, fresh) {
  if (nrow(fresh) == 0) return(cached)

  all_cols <- union(names(cached), names(fresh))
  for (col in setdiff(all_cols, names(cached))) cached[[col]] <- NA
  for (col in setdiff(all_cols, names(fresh))) fresh[[col]] <- NA
  for (col in names(cached)) {
    fresh[[col]] <- coerce_like(fresh[[col]], cached[[col]])
  }
  rbind(cached[, all_cols, drop = FALSE], fresh[, all_cols, drop = FALSE])
}

#' Merge an increment into cached metadata
#'
#' Cached rows sampled on or after \code{start_date} are replaced by the
#' freshly fetched rows for that window, so same-day additions and edits
#' (e.g. skip flags or a cruise number assigned later that day) are picked
#' up. Rows without a parseable sample time are kept.
#'
#' @param cached Cached metadata data.frame.
#' @param fresh Increment data.frame.
#' @param start_date First date covered by \code{fresh}.
#' @return Combined data.frame.
#' @keywords internal
merge_metadata_increment <- function(cached, fresh, start_date) {
  time_col <- metadata_time_col(cached)
  cached_dates <- suppressWarnings(as.Date(cached[[time_col]]))
  keep <- is.na(cached_dates) | cached_dates < as.Date(start_date)
  bind_metadata_rows(cached[keep, , drop = FALSE], fresh)
}
