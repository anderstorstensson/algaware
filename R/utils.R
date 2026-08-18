#' Null coalescing operator
#'
#' Base R only ships \code{\%||\%} from 4.4.0, while this package supports
#' R >= 4.1. It must also be exported (not just defined internally): the
#' Shiny app in \code{inst/app/} resolves names through the attached
#' namespace, so an internal definition would leave \code{server.R} without
#' it on older R versions.
#'
#' @param x,y Any objects.
#' @return \code{x} unless it is \code{NULL}, in which case \code{y}.
#' @name null-coalesce
#' @keywords internal
#' @export
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Get the configuration directory for algaware
#'
#' @return Path to the configuration directory
#' @keywords internal
get_config_dir <- function() {
  config_dir <- tools::R_user_dir("algaware", "config")
  if (!dir.exists(config_dir)) {
    dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)
  }
  config_dir
}

#' Get path to settings file
#'
#' @return Path to the settings JSON file
#' @keywords internal
get_settings_path <- function() {
  file.path(get_config_dir(), "settings.json")
}

#' Default settings for algaware
#'
#' @return A named list of default settings
#' @keywords internal
default_settings <- function() {
  list(
    dashboard_url = "",
    dashboard_dataset = "",
    classification_path = "",
    raw_data_path = "",
    ferrybox_path = "",
    local_storage_path = file.path(getwd(), "algaware_data"),
    db_folder = "",
    non_biological_classes = "detritus,Air_bubbles,Beads,Debris,mix,mixed",
    annotator = "",
    report_dnr = "",
    extra_stations = list(),
    pixels_per_micron = 2.77,  # IFCB optical calibration (pixels per micron)
    n_mosaic_taxa = 5L,       # Number of top taxa to show in report mosaics
    n_mosaic_images = 32L,    # Max images per mosaic panel
    include_class_mosaics = FALSE,  # Include per-class mosaics in report
    ctd_cnv_folder = "",            # Path to folder with .cnv CTD files
    ctd_lims_path = ""              # Path to LIMS data.txt file
  )
}

#' Normalise character columns of a data frame to UTF-8
#'
#' After reading a text file with an explicit \code{encoding}, this re-encodes
#' every character column to UTF-8 so downstream joins, matches and report
#' output behave identically regardless of the R session's native locale
#' (e.g. a non-UTF-8 Windows Server, where Å/Ä/Ö would otherwise be mangled
#' and silently drop rows). Strings already in UTF-8 are left unchanged.
#'
#' @param df A data.frame.
#' @return The data.frame with all character columns marked/encoded as UTF-8.
#' @keywords internal
as_utf8_columns <- function(df) {
  char_cols <- vapply(df, is.character, logical(1))
  df[char_cols] <- lapply(df[char_cols], enc2utf8)
  df
}

#' Load SHARK station bundle (internal wrapper)
#'
#' Wraps the internal \code{SHARK4R:::load_station_bundle()} call to
#' centralise the dependency and provide a fallback.
#'
#' @param verbose Passed to \code{load_station_bundle}.
#' @return A data.frame of SHARK stations, or an empty data.frame on failure.
#' @keywords internal
load_shark_stations <- function(verbose = FALSE) {
  if (!requireNamespace("SHARK4R", quietly = TRUE)) {
    warning("Package 'SHARK4R' is required for station data.", call. = FALSE)
    return(data.frame(STATION_NAME = character(0)))
  }
  tryCatch(
    SHARK4R:::load_station_bundle(verbose = verbose),
    error = function(e) {
      warning("Failed to load SHARK station bundle: ", e$message,
              call. = FALSE)
      data.frame(STATION_NAME = character(0))
    }
  )
}

#' Load persistent settings
#'
#' @return A named list of settings
#' @export
load_settings <- function() {
  defaults <- default_settings()
  path <- get_settings_path()

  if (!file.exists(path)) {
    return(defaults)
  }

  tryCatch({
    # simplifyDataFrame = FALSE keeps `extra_stations` (saved as a JSON array
    # of objects) a list of per-station lists. The default simplification
    # turned it into a data.frame, which every consumer (station loading,
    # the Settings tab) then crashed on with "$ operator is invalid for
    # atomic vectors" -- permanently, since the Settings tab could no longer
    # open to remove the station.
    saved <- jsonlite::fromJSON(path, simplifyVector = TRUE,
                                simplifyDataFrame = FALSE)
    # Merge saved over defaults (saved values win)
    for (key in names(saved)) {
      defaults[[key]] <- saved[[key]]
    }
    defaults$extra_stations <- normalize_extra_stations(defaults$extra_stations)
    defaults
  }, error = function(e) {
    warning("Failed to load settings: ", e$message, call. = FALSE)
    default_settings()
  })
}

#' Normalise extra_stations to a list of per-station lists
#'
#' Settings files written while the data.frame read bug was live may have been
#' re-saved in a mangled shape; accept both a data.frame and a list of lists
#' so those installations recover on the next load.
#'
#' @param x The \code{extra_stations} value read from settings.
#' @return A list of station lists (possibly empty).
#' @keywords internal
normalize_extra_stations <- function(x) {
  if (is.data.frame(x)) {
    return(lapply(seq_len(nrow(x)), function(i) as.list(x[i, ])))
  }
  if (!is.list(x)) {
    return(list())
  }
  x
}

#' Save settings to disk
#'
#' @param settings A named list of settings to persist
#' @return Invisible NULL
#' @export
save_settings <- function(settings) {
  path <- get_settings_path()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(settings, path, auto_unbox = TRUE, pretty = TRUE)
  invisible(NULL)
}

#' Parse non-biological classes from comma-separated string
#'
#' @param class_string Comma-separated class names
#' @return Character vector of trimmed class names
#' @export
parse_non_bio_classes <- function(class_string) {
  classes <- trimws(unlist(strsplit(class_string, ",")))
  classes[nzchar(classes)]
}

#' Read ROI dimensions from an ADC file
#'
#' @param adc_path Path to the .adc file
#' @return A data.frame with roi_number, width, height, roi_area columns
#' @keywords internal
read_roi_dimensions <- function(adc_path) {
  if (!file.exists(adc_path)) {
    return(data.frame(roi_number = integer(0),
                      width = integer(0),
                      height = integer(0),
                      roi_area = integer(0)))
  }

  adc <- utils::read.csv(adc_path, header = FALSE)
  data.frame(
    roi_number = seq_len(nrow(adc)),
    width = adc$V16,
    height = adc$V17,
    roi_area = adc$V16 * adc$V17
  )
}
