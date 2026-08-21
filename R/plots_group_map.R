#' Shared colour palette for phytoplankton groups
#'
#' One palette used by both the pie-chart group map and the heatmap group
#' strips so a group always has the same colour across the report.
#' \code{"Ciliates"} (heatmap only) is a darker gold in the same family as
#' the \emph{Mesodinium} pie colour: the heatmap strip label is text on a
#' light grey band, where the brighter pie yellow lacks contrast.
#'
#' @return Named character vector of hex colours.
#' @keywords internal
phyto_group_colors <- function() {
  c(
    Diatoms           = "#4A90D9",
    Dinoflagellates   = "#E74C3C",
    Cyanobacteria     = "#14B8A6",
    Cryptophytes      = "#9B59B6",
    `Mesodinium spp.` = "#F1C40F",
    Ciliates          = "#D4AC0D",
    Silicoflagellates = "#E67E22",
    Other             = "#95A5A6"
  )
}

#' Create a phytoplankton group composition map
#'
#' Thin AlgAware-specific wrapper around
#' \code{\link[SHARK4R]{create_pie_map}}. Draws a pie chart at each station
#' showing the relative carbon biomass contributed by Diatoms,
#' Dinoflagellates, Cyanobacteria, Cryptophytes, Mesodinium spp.,
#' Silicoflagellates, and Other. Any other group (e.g. "Ciliates") is
#' folded into "Other".
#'
#' @param station_summary Aggregated station data from
#'   \code{aggregate_station_data()}, containing columns \code{name},
#'   \code{AphiaID}, \code{carbon_ug_per_liter},
#'   \code{STATION_NAME_SHORT}, \code{LATITUDE_WGS84_SWEREF99_DD}, and
#'   \code{LONGITUDE_WGS84_SWEREF99_DD}.
#' @param phyto_groups Data frame with columns \code{name}, \code{AphiaID},
#'   and \code{phyto_group} as returned by
#'   \code{SHARK4R::assign_phytoplankton_group()}.
#' @param r_lat Pie chart radius in latitude degrees (default \code{0.28}).
#' @return A ggplot object.
#' @export
create_group_map <- function(station_summary, phyto_groups, r_lat = 0.28) {
  group_levels <- c("Diatoms", "Dinoflagellates", "Cyanobacteria",
                    "Cryptophytes", "Mesodinium spp.", "Silicoflagellates",
                    "Other")
  group_colors <- phyto_group_colors()[group_levels]
  group_labels <- c(
    Diatoms           = "Diatoms",
    Dinoflagellates   = "Dinoflagellates",
    Cyanobacteria     = "Cyanobacteria",
    Cryptophytes      = "Cryptophytes",
    `Mesodinium spp.` = "<i>Mesodinium</i> spp.",
    Silicoflagellates = "Silicoflagellates",
    Other             = "Other"
  )

  # Merge group assignments; unmatched taxa fall into "Other".
  # `phyto_group` is the flat column callers now build (assign_phyto_groups
  # returns a plain vector); `phyto_group.plankton_group` is the legacy nested
  # shape from when the SHARK4R data frame was embedded directly.
  if (!"phyto_group" %in% names(phyto_groups) &&
      "phyto_group.plankton_group" %in% names(phyto_groups)) {
    names(phyto_groups)[
      names(phyto_groups) == "phyto_group.plankton_group"
    ] <- "phyto_group"
  }
  merged <- merge(
    station_summary,
    phyto_groups[, c("name", "AphiaID", "phyto_group")],
    by    = c("name", "AphiaID"),
    all.x = TRUE
  )
  merged$phyto_group[is.na(merged$phyto_group) |
                       !merged$phyto_group %in% group_levels] <- "Other"

  # Drop rows without coordinates (station absent from the SHARK register);
  # they cannot be drawn and would otherwise be discarded silently.
  merged <- merged[!is.na(merged$LATITUDE_WGS84_SWEREF99_DD) &
                     !is.na(merged$LONGITUDE_WGS84_SWEREF99_DD), ,
                   drop = FALSE]
  if (nrow(merged) == 0) {
    stop("No stations with coordinates to draw in the group map",
         call. = FALSE)
  }

  # Aggregate carbon biomass per station + group into the long format
  # expected by create_pie_map(). na.action = na.pass keeps rows whose carbon
  # is NA (e.g. missing sample volume): the default na.omit dropped the whole
  # row before FUN ran, skewing group proportions despite na.rm = TRUE.
  long <- stats::aggregate(
    carbon_ug_per_liter ~
      STATION_NAME_SHORT + LATITUDE_WGS84_SWEREF99_DD +
      LONGITUDE_WGS84_SWEREF99_DD + phyto_group,
    data  = merged,
    FUN   = sum,
    na.rm = TRUE,
    na.action = stats::na.pass
  )
  names(long) <- c("station", "lat", "lon", "group", "value")
  long <- long[!is.na(long$value) & long$value > 0, , drop = FALSE]

  SHARK4R::create_pie_map(
    long,
    station_col  = "station",
    lon_col      = "lon",
    lat_col      = "lat",
    group_levels = group_levels,
    group_colors = group_colors,
    group_labels = group_labels,
    radius       = r_lat,
    size_by      = "total",
    xlim         = c(10, 22),
    ylim         = c(54, 60),
    title        = NULL,
    legend_title = "Taxon group"
  )
}
