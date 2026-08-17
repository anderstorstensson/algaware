#' Load AlgAware station definitions
#'
#' Loads the bundled station list and appends any extra stations from settings.
#'
#' @param extra_stations A list of extra station definitions, each with
#'   \code{STATION_NAME}, \code{COAST}, and \code{STATION_NAME_SHORT}.
#' @return A data.frame with STATION_NAME, COAST, STATION_NAME_SHORT columns.
#' @export
load_algaware_stations <- function(extra_stations = list()) {
  station_file <- system.file("stations", "algaware_stations.tsv",
                              package = "algaware")
  # The bundled file is UTF-8. Declare it via `encoding` (which marks the
  # parsed strings as UTF-8 without re-encoding through the native locale) so
  # station names with Swedish characters (Å/Ä/Ö, e.g. "Å17", "SLÄGGÖ") are
  # read correctly on any machine. Note: do NOT also pass `fileEncoding`, which
  # would re-encode the bytes through the native locale first and corrupt those
  # characters on a non-UTF-8 host (e.g. Windows Server). as_utf8_columns()
  # then guarantees consistent UTF-8 marking for downstream matching.
  stations <- utils::read.delim(station_file, stringsAsFactors = FALSE,
                                encoding = "UTF-8")
  stations <- as_utf8_columns(stations)

  if (length(extra_stations) > 0) {
    extra_df <- do.call(rbind, lapply(extra_stations, function(s) {
      data.frame(
        STATION_NAME = s$STATION_NAME,
        COAST = s$COAST,
        STATION_NAME_SHORT = s$STATION_NAME_SHORT,
        stringsAsFactors = FALSE
      )
    }))
    # Avoid duplicates (compare on UTF-8 to match the bundled names reliably)
    extra_df <- as_utf8_columns(extra_df)
    extra_df <- extra_df[!extra_df$STATION_NAME %in% stations$STATION_NAME, ]
    stations <- rbind(stations, extra_df)
  }

  stations
}

#' Match dashboard metadata to AlgAware stations using spatial join
#'
#' Finds IFCB bins sampled near AlgAware stations by creating buffers around
#' station centroids and performing a spatial join.
#'
#' @param metadata A data.frame with at least \code{latitude} and
#'   \code{longitude} columns (from dashboard metadata).
#' @param algaware_stations A data.frame from \code{load_algaware_stations()}.
#' @return A tibble of metadata rows matched to stations, with
#'   \code{STATION_NAME}, \code{COAST}, and \code{STATION_NAME_SHORT} columns
#'   added.
#' @export
match_bins_to_stations <- function(metadata, algaware_stations) {
  # Note: load_station_bundle is an internal SHARK4R function

  station_bundle <- load_shark_stations(verbose = FALSE)

  # Normalise both sides to UTF-8 before matching. The SHARK register and the
  # bundled station list can carry different encoding marks depending on the
  # host locale; without this, names containing Å/Ä/Ö (e.g. "Å17", "SLÄGGÖ",
  # "BY39 ÖLANDS SÖDRA UDDE") fail the comparison and the station is silently
  # dropped from the spatial join (and therefore from downloads and plots).
  station_bundle$STATION_NAME <- enc2utf8(station_bundle$STATION_NAME)
  algaware_names <- enc2utf8(algaware_stations$STATION_NAME)

  algaware_station_data <- station_bundle[
    station_bundle$STATION_NAME %in% algaware_names,
  ]

  # Convert station coordinates to an sf spatial object.
  # Coordinates are in WGS84 (GPS) decimal degrees (CRS 4326).
  stations_sf <- sf::st_as_sf(
    algaware_station_data,
    coords = c("LONGITUDE_WGS84_SWEREF99_DD", "LATITUDE_WGS84_SWEREF99_DD"),
    crs = 4326
  )
  # Transform to SWEREF99 TM (CRS 3006, the Swedish national projection)
  # so that buffer distances are in meters rather than degrees.
  stations_sf <- sf::st_transform(stations_sf, 3006)

  metadata <- metadata[
    !is.na(metadata$longitude) & !is.na(metadata$latitude),
  ]

  if (nrow(metadata) == 0) return(data.frame())

  metadata_sf <- sf::st_as_sf(
    metadata,
    coords = c("longitude", "latitude"),
    crs = 4326
  )
  metadata_sf <- sf::st_transform(metadata_sf, 3006)

  # Create circular buffers around each station. OUT_OF_BOUNDS_RADIUS (meters)
  # comes from the SHARK station register and defines how far from the station
  # centroid a sample can be and still count as "at that station".
  station_buffers <- sf::st_buffer(
    stations_sf,
    dist = algaware_station_data$OUT_OF_BOUNDS_RADIUS
  )

  # Spatial join: keep only IFCB bins that fall within a station buffer
  joined <- sf::st_join(
    metadata_sf,
    station_buffers[, "STATION_NAME"],
    join = sf::st_within
  )

  result <- sf::st_drop_geometry(joined)
  result <- result[!is.na(result$STATION_NAME), ]

  merge(result, algaware_stations, by = "STATION_NAME", all.x = TRUE)
}

#' Group bins into station visits
#'
#' Handles cases where a station is visited multiple times during a cruise
#' (days apart), and where bins at the same station span midnight.
#'
#' @param metadata A data.frame with \code{STATION_NAME} and
#'   \code{sample_time} columns (POSIXct).
#' @param max_gap_hours Maximum gap between consecutive bins to consider them
#'   part of the same visit. Default 12 hours. The ship may pass the same
#'   station twice on different days; this threshold splits those into
#'   separate visits.
#' @return The input data.frame with an added \code{visit_id} column
#'   (character: \code{STATION_NAME_visitN}).
#' @keywords internal
assign_station_visits <- function(metadata, max_gap_hours = 12) {
  metadata <- metadata[order(metadata$STATION_NAME, metadata$sample_time), ]
  metadata$visit_id <- NA_character_

  for (stn in unique(metadata$STATION_NAME)) {
    idx <- which(metadata$STATION_NAME == stn)
    times <- metadata$sample_time[idx]
    visit_num <- 1L
    metadata$visit_id[idx[1]] <- paste0(stn, "_visit", visit_num)

    if (length(idx) > 1) {
      for (i in 2:length(idx)) {
        gap_hours <- as.numeric(difftime(times[i], times[i - 1], units = "hours"))
        # A missing sample_time yields an NA gap; treat the bin as part of
        # the current visit instead of crashing the whole aggregation.
        if (!is.na(gap_hours) && gap_hours > max_gap_hours) {
          visit_num <- visit_num + 1L
        }
        metadata$visit_id[idx[i]] <- paste0(stn, "_visit", visit_num)
      }
    }
  }

  metadata
}
