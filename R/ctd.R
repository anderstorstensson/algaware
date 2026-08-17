#' Load bundled historical Chl-a statistics (1991-2020)
#'
#' @return A data.frame with columns: STATN, STNCODE, MONTH (integer),
#'   CHLA_mean (numeric), CHLA_std (numeric), CHLA_n (integer).
#' @export
load_chl_statistics <- function() {
  path <- system.file("extdata", "annual_1991-2020_statistics_chl20m.txt",
                      package = "algaware")
  if (!nzchar(path)) {
    warning("Chl statistics file not found in package.", call. = FALSE)
    return(data.frame(STATN = character(0), STNCODE = character(0),
                      MONTH = integer(0), CHLA_mean = numeric(0),
                      CHLA_std = numeric(0), CHLA_n = integer(0)))
  }

  # File is latin1; `fileEncoding` re-encodes it through the connection to the
  # native encoding (the only form that parses on a UTF-8 host), then
  # as_utf8_columns() normalises station names to UTF-8 so they match the
  # (UTF-8) algaware stations on any locale.
  raw <- utils::read.delim(path, fileEncoding = "latin1",
                           stringsAsFactors = FALSE, check.names = FALSE)

  out <- data.frame(
    STATN = raw$STATN,
    STNCODE = raw$STNCODE,
    MONTH = as.integer(raw$MONTH),
    CHLA_mean = suppressWarnings(as.numeric(raw[["CHLA:mean"]])),
    CHLA_std = suppressWarnings(as.numeric(raw[["CHLA:std"]])),
    CHLA_n = suppressWarnings(as.integer(raw[["CHLA:number_of_values"]])),
    stringsAsFactors = FALSE
  )
  # "nan" strings become NA via as.numeric, which is correct
  as_utf8_columns(out)
}

#' Load station name synonym mapper
#'
#' Maps various raw station name synonyms to canonical algaware station names.
#'
#' @return A data.frame with columns: synonym, statn.
#' @export
load_station_mapper <- function() {
  path <- system.file("extdata", "station_mapper.txt", package = "algaware")
  if (!nzchar(path)) {
    return(data.frame(synonym = character(0), statn = character(0),
                      stringsAsFactors = FALSE))
  }
  # station_mapper.txt is latin1; re-encode via fileEncoding, then normalise to
  # UTF-8 so the synonym/canonical names match algaware stations on any locale.
  mapper <- utils::read.delim(path, fileEncoding = "latin1",
                              stringsAsFactors = FALSE, check.names = FALSE)
  as_utf8_columns(mapper)
}

#' Load standard monitoring stations with regional assignments
#'
#' @return A data.frame with columns: station_name (character), region (character).
#'   Region values are e.g. "The Skagerrak", "The Eastern Baltic", etc.
#' @export
load_standard_stations <- function() {
  path <- system.file("extdata", "standard_stations.yaml", package = "algaware")
  if (!nzchar(path)) {
    return(data.frame(station_name = character(0), region = character(0),
                      stringsAsFactors = FALSE))
  }
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required to read standard_stations.yaml. ",
         "Install it with: install.packages(\"yaml\")", call. = FALSE)
  }
  s <- yaml::read_yaml(path)$standard_stations
  station_list <- s$station_list
  regions <- vapply(station_list, function(nm) {
    r <- s[[nm]]
    if (is.character(r) && length(r) == 1L) r else NA_character_
  }, character(1L))
  data.frame(station_name = enc2utf8(station_list),
             region = enc2utf8(unname(regions)),
             stringsAsFactors = FALSE)
}

#' Normalize a raw station name to the canonical name
#'
#' Looks up in the station mapper, then falls back to standard_stations direct match.
#'
#' @param raw_name Character raw station name from CNV header or LIMS STATN column.
#' @param station_mapper Data frame from \code{load_station_mapper()}.
#' @param standard_stations Data frame from \code{load_standard_stations()}.
#'   If provided, used as fallback when mapper does not match.
#' @return Canonical station name string, or NA_character_ if no match.
#' @keywords internal
normalize_station_name <- function(raw_name, station_mapper,
                                   standard_stations = NULL) {
  if (is.na(raw_name) || !nzchar(trimws(raw_name))) return(NA_character_)

  norm_raw <- tolower(trimws(raw_name))
  norm_syn <- tolower(trimws(station_mapper$synonym))

  # 1. Exact match in mapper
  idx <- match(norm_raw, norm_syn)
  if (!is.na(idx)) return(station_mapper$statn[idx])

  # 2. Prefix: raw name starts with a synonym (handles "BY2 ARKONA EXTRA TEXT")
  idx2 <- which(startsWith(norm_raw, paste0(norm_syn, " ")) |
                  norm_raw == norm_syn)[1]
  if (!is.na(idx2)) return(station_mapper$statn[idx2])

  # 3. Fallback: exact or prefix match against standard station names
  if (!is.null(standard_stations) && nrow(standard_stations) > 0) {
    norm_std <- tolower(trimws(standard_stations$station_name))
    idx3 <- match(norm_raw, norm_std)
    if (!is.na(idx3)) return(standard_stations$station_name[idx3])

    idx4 <- which(startsWith(norm_raw, paste0(norm_std, " ")) |
                    norm_raw == norm_std)[1]
    if (!is.na(idx4)) return(standard_stations$station_name[idx4])
  }

  NA_character_
}

#' Match a canonical station name against historical Chl-a statistics
#'
#' Handles encoding differences between YAML (UTF-8) and stats file (latin1).
#'
#' @param canonical_name Canonical station name string.
#' @param chl_stats Data frame from \code{load_chl_statistics()}.
#' @return Data frame (subset of chl_stats) for that station, or NULL.
#' @keywords internal
match_stats_station <- function(canonical_name, chl_stats) {
  if (is.null(chl_stats) || nrow(chl_stats) == 0) return(NULL)

  norm_name <- tolower(trimws(canonical_name))
  norm_statn <- tolower(trimws(chl_stats$STATN))

  # Exact match
  idx <- which(norm_statn == norm_name)
  if (length(idx) > 0) return(chl_stats[idx, ])

  # ASCII-stripped match (handles Å/A, Ö/O, etc.)
  safe_name <- gsub("[^a-z0-9 ]", "", norm_name)
  safe_statn <- gsub("[^a-z0-9 ]", "", norm_statn)
  idx2 <- which(safe_statn == safe_name)
  if (length(idx2) > 0) return(chl_stats[idx2, ])

  NULL
}

# ---------------------------------------------------------------------------
# CNV parsing (AlgAware-matched, for chl map)
# ---------------------------------------------------------------------------

#' Parse a single CNV file (AlgAware-matched)
#'
#' @param file_path Path to a .cnv file.
#' @param algaware_stations Data frame from \code{load_algaware_stations()}.
#' @return A data.frame with profile data, or NULL if parsing fails or
#'   station is unmatched.
#' @keywords internal
parse_single_cnv <- function(file_path, algaware_stations) {
  if (!requireNamespace("oce", quietly = TRUE)) {
    stop("Package 'oce' is required to parse CNV files. ",
         "Install it with: install.packages(\"oce\")", call. = FALSE)
  }
  ctd <- tryCatch(
    suppressWarnings(oce::read.ctd(file_path, processingLog = FALSE)),
    error = function(e) NULL
  )
  if (is.null(ctd)) return(NULL)

  header_lines <- ctd@metadata$header
  if (is.null(header_lines)) header_lines <- character(0)
  station_line <- grep("^\\*\\*\\s*Station:", header_lines, value = TRUE)
  if (length(station_line) == 0) return(NULL)

  raw_station <- enc2utf8(trimws(sub("^\\*\\*\\s*Station:\\s*", "",
                                     station_line[1])))
  station_short <- trimws(strsplit(raw_station, "\\s+")[[1]][1])

  match_idx <- match(tolower(station_short),
                     tolower(algaware_stations$STATION_NAME_SHORT))
  if (is.na(match_idx)) {
    aw_short <- tolower(algaware_stations$STATION_NAME_SHORT)
    match_idx <- which(vapply(aw_short, function(s) {
      startsWith(tolower(raw_station), s)
    }, logical(1)))[1]
  }
  if (is.na(match_idx) || length(match_idx) == 0) return(NULL)

  matched_station <- algaware_stations[match_idx, ]

  lat <- tryCatch(ctd[["latitude"]], error = function(e) NA_real_)
  lon <- tryCatch(ctd[["longitude"]], error = function(e) NA_real_)
  pressure <- tryCatch(ctd[["pressure"]], error = function(e) NULL)
  if (is.null(pressure)) return(NULL)

  fluor <- tryCatch(ctd[["fluorescence"]], error = function(e) NULL)
  if (is.null(fluor)) {
    data_names <- names(ctd@data)
    fluor_col <- grep("fluor|flECO|chlorophyll", data_names,
                      ignore.case = TRUE, value = TRUE)
    if (length(fluor_col) > 0) fluor <- ctd@data[[fluor_col[1]]]
  }
  if (is.null(fluor)) return(NULL)

  cruise_line <- grep("^\\*\\*\\s*Cruise:", header_lines, value = TRUE)
  cruise <- if (length(cruise_line) > 0) {
    trimws(sub("^\\*\\*\\s*Cruise:\\s*", "", cruise_line[1]))
  } else {
    NA_character_
  }

  sample_date <- tryCatch({
    time_val <- ctd[["time"]]
    if (!is.null(time_val) && length(time_val) > 0) {
      as.Date(time_val[1])
    } else {
      date_match <- regmatches(basename(file_path),
                               regexpr("\\d{8}", basename(file_path)))
      if (length(date_match) > 0) as.Date(date_match, "%Y%m%d") else NA
    }
  }, error = function(e) NA)

  data.frame(
    station_short = matched_station$STATION_NAME_SHORT,
    station_name = matched_station$STATION_NAME,
    coast = matched_station$COAST,
    latitude = lat,
    longitude = lon,
    pressure_dbar = pressure,
    chl_fluorescence = fluor,
    cruise = cruise,
    sample_date = sample_date,
    file_path = file_path,
    stringsAsFactors = FALSE
  )
}

#' Parse all CNV files from a folder tree (AlgAware-matched, for chl map)
#'
#' @param cnv_folder Path to parent Cnv/ folder (searched recursively).
#' @param algaware_stations Data frame from \code{load_algaware_stations()}.
#' @return A data.frame or NULL.
#' @export
read_cnv_folder <- function(cnv_folder, algaware_stations) {
  if (!dir.exists(cnv_folder)) {
    warning("CNV folder does not exist: ", cnv_folder, call. = FALSE)
    return(NULL)
  }

  cnv_files <- list.files(cnv_folder, pattern = "\\.cnv$",
                          recursive = TRUE, full.names = TRUE)
  if (length(cnv_files) == 0) {
    warning("No .cnv files found in: ", cnv_folder, call. = FALSE)
    return(NULL)
  }

  results <- lapply(cnv_files, function(f) {
    tryCatch(parse_single_cnv(f, algaware_stations), error = function(e) NULL)
  })
  results <- results[!vapply(results, is.null, logical(1))]
  if (length(results) == 0) return(NULL)
  do.call(rbind, results)
}

# ---------------------------------------------------------------------------
# CNV parsing (all standard stations, for CTD plots)
# ---------------------------------------------------------------------------

#' Parse a single CNV file using station mapper and standard stations
#'
#' @param file_path Path to a .cnv file.
#' @param station_mapper Data frame from \code{load_station_mapper()}.
#' @param standard_stations Data frame from \code{load_standard_stations()}.
#' @return A data.frame with profile data including canonical_name and region,
#'   or NULL if parsing fails or station cannot be matched.
#' @keywords internal
parse_single_cnv_all <- function(file_path, station_mapper, standard_stations) {
  if (!requireNamespace("oce", quietly = TRUE)) {
    stop("Package 'oce' is required to parse CNV files. ",
         "Install it with: install.packages(\"oce\")", call. = FALSE)
  }
  ctd <- tryCatch(
    suppressWarnings(oce::read.ctd(file_path, processingLog = FALSE)),
    error = function(e) NULL
  )
  if (is.null(ctd)) return(NULL)

  header_lines <- ctd@metadata$header
  if (is.null(header_lines)) header_lines <- character(0)
  station_line <- grep("^\\*\\*\\s*Station:", header_lines, value = TRUE)
  if (length(station_line) == 0) return(NULL)

  raw_station <- enc2utf8(trimws(sub("^\\*\\*\\s*Station:\\s*", "",
                                     station_line[1])))

  canonical <- normalize_station_name(raw_station, station_mapper,
                                      standard_stations)
  if (is.na(canonical)) return(NULL)

  region_idx <- match(tolower(trimws(canonical)),
                      tolower(trimws(standard_stations$station_name)))
  region <- if (!is.na(region_idx)) standard_stations$region[region_idx] else NA_character_
  if (is.na(region)) return(NULL)

  lat <- tryCatch(ctd[["latitude"]], error = function(e) NA_real_)
  lon <- tryCatch(ctd[["longitude"]], error = function(e) NA_real_)
  pressure <- tryCatch(ctd[["pressure"]], error = function(e) NULL)
  if (is.null(pressure)) return(NULL)

  fluor <- tryCatch(ctd[["fluorescence"]], error = function(e) NULL)
  if (is.null(fluor)) {
    data_names <- names(ctd@data)
    fluor_col <- grep("fluor|flECO|chlorophyll", data_names,
                      ignore.case = TRUE, value = TRUE)
    if (length(fluor_col) > 0) fluor <- ctd@data[[fluor_col[1]]]
  }
  if (is.null(fluor)) return(NULL)

  cruise_line <- grep("^\\*\\*\\s*Cruise:", header_lines, value = TRUE)
  cruise <- if (length(cruise_line) > 0) {
    trimws(sub("^\\*\\*\\s*Cruise:\\s*", "", cruise_line[1]))
  } else {
    NA_character_
  }

  sample_date <- tryCatch({
    time_val <- ctd[["time"]]
    if (!is.null(time_val) && length(time_val) > 0) {
      as.Date(time_val[1])
    } else {
      date_match <- regmatches(basename(file_path),
                               regexpr("\\d{8}", basename(file_path)))
      if (length(date_match) > 0) as.Date(date_match, "%Y%m%d") else NA
    }
  }, error = function(e) NA)

  data.frame(
    canonical_name = canonical,
    region = region,
    latitude = if (length(lat) > 0) lat[[1]] else NA_real_,
    longitude = if (length(lon) > 0) lon[[1]] else NA_real_,
    pressure_dbar = pressure,
    chl_fluorescence = fluor,
    cruise = cruise,
    sample_date = sample_date,
    file_path = file_path,
    stringsAsFactors = FALSE
  )
}

#' Parse all CNV files from a folder tree (all standard stations)
#'
#' Reads every .cnv file in a folder tree and matches stations against the
#' standard monitoring station list (not just AlgAware stations). Use for
#' CTD profile and time-series plots.
#'
#' @param cnv_folder Path to parent Cnv/ folder (searched recursively).
#' @param station_mapper Data frame from \code{load_station_mapper()}.
#' @param standard_stations Data frame from \code{load_standard_stations()}.
#' @return A data.frame with columns: canonical_name, region, latitude,
#'   longitude, pressure_dbar, chl_fluorescence, cruise, sample_date,
#'   file_path. Returns NULL if no valid files found.
#' @export
read_cnv_folder_all <- function(cnv_folder, station_mapper, standard_stations) {
  if (!dir.exists(cnv_folder)) {
    warning("CNV folder does not exist: ", cnv_folder, call. = FALSE)
    return(NULL)
  }
  cnv_files <- list.files(cnv_folder, pattern = "\\.cnv$",
                          recursive = TRUE, full.names = TRUE)
  if (length(cnv_files) == 0) {
    warning("No .cnv files found in: ", cnv_folder, call. = FALSE)
    return(NULL)
  }
  results <- lapply(cnv_files, function(f) {
    tryCatch(parse_single_cnv_all(f, station_mapper, standard_stations),
             error = function(e) NULL)
  })
  results <- results[!vapply(results, is.null, logical(1))]
  if (length(results) == 0) return(NULL)
  do.call(rbind, results)
}

# ---------------------------------------------------------------------------
# LIMS parsing (AlgAware-matched, for chl map)
# ---------------------------------------------------------------------------

#' Read LIMS discrete chlorophyll data (AlgAware-matched)
#'
#' @param lims_path Path to LIMS data.txt file.
#' @param algaware_stations Data frame from \code{load_algaware_stations()}.
#' @return A data.frame with columns: station_short, station_name, coast,
#'   sample_date, sample_month, DEPH, CPHL, latitude, longitude. NULL if
#'   file not found or no valid data.
#' @export
read_lims_data <- function(lims_path, algaware_stations) {
  if (!file.exists(lims_path)) {
    warning("LIMS file not found: ", lims_path, call. = FALSE)
    return(NULL)
  }

  raw <- tryCatch(
    utils::read.delim(lims_path, stringsAsFactors = FALSE,
                      fileEncoding = "latin1"),
    error = function(e) {
      warning("Failed to read LIMS file: ", e$message, call. = FALSE)
      NULL
    }
  )
  if (is.null(raw) || nrow(raw) == 0) return(NULL)
  raw <- as_utf8_columns(raw)

  if ("Q_CPHL" %in% names(raw)) {
    raw <- raw[is.na(raw$Q_CPHL) | raw$Q_CPHL != "B", ]
  }
  if ("Q_CHLFL" %in% names(raw)) {
    raw <- raw[is.na(raw$Q_CHLFL) | raw$Q_CHLFL != "B", ]
  }

  if ("CPHL" %in% names(raw)) {
    raw$CPHL <- suppressWarnings(as.numeric(raw$CPHL))
  }
  if ("CHLFL" %in% names(raw)) {
    raw$CHLFL <- suppressWarnings(as.numeric(raw$CHLFL))
  }

  has_cphl <- "CPHL" %in% names(raw) && any(!is.na(raw$CPHL))
  has_chlfl <- "CHLFL" %in% names(raw) && any(!is.na(raw$CHLFL))

  if (!has_cphl && !has_chlfl) return(NULL)

  raw$chl_value <- if (has_cphl) raw$CPHL else raw$CHLFL

  if (!"DEPH" %in% names(raw)) return(NULL)
  raw$DEPH <- suppressWarnings(as.numeric(raw$DEPH))
  raw <- raw[!is.na(raw$chl_value) & !is.na(raw$DEPH), ]
  if (nrow(raw) == 0) return(NULL)

  raw$station_short <- NA_character_
  raw$station_name_matched <- NA_character_
  raw$coast <- NA_character_

  normalize_stn <- function(x) {
    x <- tolower(trimws(x))
    gsub("[/ ]+", " ", x)
  }

  for (i in seq_len(nrow(algaware_stations))) {
    aw_name <- algaware_stations$STATION_NAME[i]
    aw_short <- algaware_stations$STATION_NAME_SHORT[i]
    aw_coast <- algaware_stations$COAST[i]

    norm_raw <- normalize_stn(raw$STATN)
    norm_name <- normalize_stn(aw_name)
    norm_short <- normalize_stn(aw_short)

    hits <- norm_raw == norm_name |
      norm_raw == norm_short |
      startsWith(norm_raw, paste0(norm_short, " ")) |
      startsWith(norm_raw, paste0(norm_short, "-"))
    raw$station_short[hits] <- aw_short
    raw$station_name_matched[hits] <- aw_name
    raw$coast[hits] <- aw_coast
  }

  raw <- raw[!is.na(raw$station_short), ]
  if (nrow(raw) == 0) return(NULL)

  lat <- suppressWarnings(as.numeric(raw$LATIT))
  lon <- suppressWarnings(as.numeric(raw$LONGI))
  lat_deg <- floor(lat / 100) + (lat %% 100) / 60
  lon_deg <- floor(lon / 100) + (lon %% 100) / 60

  data.frame(
    station_short = raw$station_short,
    station_name = raw$station_name_matched,
    coast = raw$coast,
    sample_date = as.Date(raw$SDATE),
    sample_month = as.integer(format(as.Date(raw$SDATE), "%m")),
    DEPH = raw$DEPH,
    CPHL = raw$chl_value,
    SMPNO = if ("SMPNO" %in% names(raw)) raw$SMPNO else NA_character_,
    latitude = lat_deg,
    longitude = lon_deg,
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# LIMS parsing (all standard stations, for CTD plots)
# ---------------------------------------------------------------------------

#' Read LIMS discrete chlorophyll data (all standard stations)
#'
#' Like \code{read_lims_data()} but matches against all standard monitoring
#' stations instead of just AlgAware stations.
#'
#' @param lims_path Path to LIMS data.txt file.
#' @param station_mapper Data frame from \code{load_station_mapper()}.
#' @param standard_stations Data frame from \code{load_standard_stations()}.
#' @return A data.frame with columns: canonical_name, region, sample_date,
#'   sample_month, DEPH, CPHL, latitude, longitude. NULL if file not found
#'   or no valid data.
#' @export
read_lims_data_all <- function(lims_path, station_mapper, standard_stations) {
  if (!file.exists(lims_path)) {
    warning("LIMS file not found: ", lims_path, call. = FALSE)
    return(NULL)
  }

  raw <- tryCatch(
    utils::read.delim(lims_path, stringsAsFactors = FALSE,
                      fileEncoding = "latin1"),
    error = function(e) {
      warning("Failed to read LIMS file: ", e$message, call. = FALSE)
      NULL
    }
  )
  if (is.null(raw) || nrow(raw) == 0) return(NULL)
  raw <- as_utf8_columns(raw)

  if ("Q_CPHL" %in% names(raw)) {
    raw <- raw[is.na(raw$Q_CPHL) | raw$Q_CPHL != "B", ]
  }
  if ("Q_CHLFL" %in% names(raw)) {
    raw <- raw[is.na(raw$Q_CHLFL) | raw$Q_CHLFL != "B", ]
  }

  if ("CPHL" %in% names(raw)) {
    raw$CPHL <- suppressWarnings(as.numeric(raw$CPHL))
  }
  if ("CHLFL" %in% names(raw)) {
    raw$CHLFL <- suppressWarnings(as.numeric(raw$CHLFL))
  }

  has_cphl <- "CPHL" %in% names(raw) && any(!is.na(raw$CPHL))
  has_chlfl <- "CHLFL" %in% names(raw) && any(!is.na(raw$CHLFL))

  if (!has_cphl && !has_chlfl) return(NULL)

  raw$chl_value <- if (has_cphl) raw$CPHL else raw$CHLFL

  if (!"DEPH" %in% names(raw)) return(NULL)
  raw$DEPH <- suppressWarnings(as.numeric(raw$DEPH))
  raw <- raw[!is.na(raw$chl_value) & !is.na(raw$DEPH), ]
  if (nrow(raw) == 0) return(NULL)

  # Normalize STATN column
  raw$canonical_name <- vapply(raw$STATN, function(nm) {
    normalize_station_name(nm, station_mapper, standard_stations)
  }, character(1L))

  raw <- raw[!is.na(raw$canonical_name), ]
  if (nrow(raw) == 0) return(NULL)

  # Look up region
  raw$region <- standard_stations$region[
    match(tolower(trimws(raw$canonical_name)),
          tolower(trimws(standard_stations$station_name)))
  ]
  raw <- raw[!is.na(raw$region), ]
  if (nrow(raw) == 0) return(NULL)

  lat <- suppressWarnings(as.numeric(raw$LATIT))
  lon <- suppressWarnings(as.numeric(raw$LONGI))
  lat_deg <- floor(lat / 100) + (lat %% 100) / 60
  lon_deg <- floor(lon / 100) + (lon %% 100) / 60

  data.frame(
    canonical_name = raw$canonical_name,
    region = raw$region,
    sample_date = as.Date(raw$SDATE),
    sample_month = as.integer(format(as.Date(raw$SDATE), "%m")),
    DEPH = raw$DEPH,
    CPHL = raw$chl_value,
    latitude = lat_deg,
    longitude = lon_deg,
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# Depth-averaged chlorophyll (for chl map, 0-20m)
# ---------------------------------------------------------------------------

#' Compute 0-20m depth-averaged chlorophyll from CTD data
#'
#' @param ctd_data Data frame from \code{read_cnv_folder()}.
#' @return Data frame with station_short, latitude, longitude, chl_mean.
#' @export
compute_ctd_chl_avg <- function(ctd_data) {
  if (is.null(ctd_data) || nrow(ctd_data) == 0) {
    return(data.frame(station_short = character(0), latitude = numeric(0),
                      longitude = numeric(0), chl_mean = numeric(0)))
  }

  shallow <- ctd_data[ctd_data$pressure_dbar <= 20, ]
  if (nrow(shallow) == 0) return(data.frame(
    station_short = character(0), latitude = numeric(0),
    longitude = numeric(0), chl_mean = numeric(0)))

  # Aggregate by station only (mirroring compute_lims_chl_avg): grouping by
  # lat/lon as well produced several rows per station when repeat casts
  # carried slightly different coordinates, and the report's station-summary
  # merge then duplicated every taxon row of that station, doubling its
  # biomass in the maps and LLM text.
  agg <- stats::aggregate(
    chl_fluorescence ~ station_short,
    data = shallow,
    FUN = function(x) mean(x, na.rm = TRUE)
  )
  names(agg)[names(agg) == "chl_fluorescence"] <- "chl_mean"

  # Representative lat/lon: first occurrence per station
  coords <- shallow[!duplicated(shallow$station_short),
                    c("station_short", "latitude", "longitude")]
  merge(agg, coords, by = "station_short")
}

#' Compute 0-20m depth-averaged chlorophyll from LIMS bottle data
#'
#' Uses discrete bottle samples only (excludes hose/SLA samples).  Duplicate
#' station rows arising from slightly varying coordinates are collapsed by
#' aggregating on \code{station_short} only.
#'
#' @param lims_data Data frame from \code{read_lims_data()}.
#' @return Data frame with station_short, latitude, longitude, chl_mean.
#' @export
compute_lims_chl_avg <- function(lims_data) {
  empty <- data.frame(station_short = character(0), latitude = numeric(0),
                      longitude = numeric(0), chl_mean = numeric(0))
  if (is.null(lims_data) || nrow(lims_data) == 0) return(empty)

  # Exclude hose/integrated samples (SMPNO contains "SLA")
  bottle <- lims_data
  if ("SMPNO" %in% names(lims_data)) {
    is_hose <- grepl("-SLA_", lims_data$SMPNO, fixed = TRUE) &
               !is.na(lims_data$SMPNO)
    bottle <- lims_data[!is_hose, ]
  }

  shallow <- bottle[!is.na(bottle$DEPH) & bottle$DEPH <= 20, ]
  if (nrow(shallow) == 0) return(empty)

  # Aggregate chl by station only to avoid duplicate map labels when
  # lat/lon differs slightly across rows for the same station.
  chl_agg <- stats::aggregate(
    CPHL ~ station_short,
    data = shallow,
    FUN = function(x) mean(x, na.rm = TRUE)
  )
  names(chl_agg)[names(chl_agg) == "CPHL"] <- "chl_mean"

  # Representative lat/lon: first occurrence per station
  coords <- shallow[!duplicated(shallow$station_short),
                    c("station_short", "latitude", "longitude")]
  merge(chl_agg, coords, by = "station_short")
}

#' Compute chlorophyll from LIMS hose (integrated 0-10 m) samples
#'
#' Hose samples have \code{"SLA"} in their \code{SMPNO} field (e.g.
#' \code{"2026-N14_FALKENBERG-SLA_0-10-017_SYNC"}).  They represent a
#' water-column integration over 0-10 m and require no depth averaging.
#'
#' @param lims_data Data frame from \code{read_lims_data()} (must include
#'   the \code{SMPNO} column).
#' @return Data frame with station_short, latitude, longitude, chl_mean,
#'   or an empty data frame if no hose samples are found.
#' @export
compute_lims_hose_avg <- function(lims_data) {
  empty <- data.frame(station_short = character(0), latitude = numeric(0),
                      longitude = numeric(0), chl_mean = numeric(0))
  if (is.null(lims_data) || nrow(lims_data) == 0) return(empty)
  if (!"SMPNO" %in% names(lims_data)) return(empty)

  hose <- lims_data[grepl("-SLA_", lims_data$SMPNO, fixed = TRUE) &
                    !is.na(lims_data$SMPNO) &
                    !is.na(lims_data$CPHL), ]
  if (nrow(hose) == 0) return(empty)

  chl_agg <- stats::aggregate(
    CPHL ~ station_short,
    data = hose,
    FUN = function(x) mean(x, na.rm = TRUE)
  )
  names(chl_agg)[names(chl_agg) == "CPHL"] <- "chl_mean"

  coords <- hose[!duplicated(hose$station_short),
                 c("station_short", "latitude", "longitude")]
  merge(chl_agg, coords, by = "station_short")
}
