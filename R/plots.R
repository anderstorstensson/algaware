#' Create a base map of Swedish waters
#'
#' @return A ggplot object with coastline, coordinate system, and theme.
#' @keywords internal
base_sweden_map <- function() {
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = world, fill = "gray95", color = "gray70") +
    ggplot2::coord_sf(xlim = c(10, 22), ylim = c(54, 60), expand = FALSE) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "aliceblue"),
      axis.title = ggplot2::element_blank(),
      legend.position = "right",
      legend.direction = "vertical",
      legend.key.width  = ggplot2::unit(0.6, "cm"),
      legend.key.height = ggplot2::unit(1.8, "cm"),
      legend.title.position = "top",
      legend.title = ggplot2::element_text(hjust = 0.5)
    )
}

#' Create biomass and chlorophyll maps
#'
#' @param station_summary Aggregated station data from
#'   \code{aggregate_station_data()}.
#' @return A list with \code{biomass_map} and \code{chl_map} ggplot objects.
#' @export
create_biomass_maps <- function(station_summary) {
  station_biomass <- stats::aggregate(
    cbind(total_carbon_biomass = carbon_ug_per_liter,
          total_biovolume = biovolume_mm3_per_liter) ~
      STATION_NAME_SHORT + LATITUDE_WGS84_SWEREF99_DD +
      LONGITUDE_WGS84_SWEREF99_DD + median_time,
    data = station_summary,
    FUN = sum,
    na.rm = TRUE
  )

  # Add chlorophyll if available
  if ("chl_mean" %in% names(station_summary)) {
    chl_agg <- stats::aggregate(
      chl_mean ~ STATION_NAME_SHORT,
      data = station_summary,
      FUN = mean,
      na.rm = TRUE
    )
    station_biomass <- merge(station_biomass, chl_agg,
                             by = "STATION_NAME_SHORT", all.x = TRUE)
  } else {
    station_biomass$chl_mean <- NA_real_
  }

  base_map <- base_sweden_map()

  biomass_map <- base_map +
    ggplot2::geom_point(
      data = station_biomass,
      ggplot2::aes(x = .data$LONGITUDE_WGS84_SWEREF99_DD,
                   y = .data$LATITUDE_WGS84_SWEREF99_DD,
                   color = .data$total_carbon_biomass),
      size = 5
    ) +
    ggrepel::geom_text_repel(
      data = station_biomass,
      ggplot2::aes(x = .data$LONGITUDE_WGS84_SWEREF99_DD,
                   y = .data$LATITUDE_WGS84_SWEREF99_DD,
                   label = .data$STATION_NAME_SHORT),
      size = 3, min.segment.length = 0, segment.color = "gray50",
      box.padding = 0.5
    ) +
    ggplot2::scale_color_viridis_c(
      option = "mako", direction = -1,
      name = expression(paste("Biomass\n(", mu, "g C/L)"))
    ) +
    ggplot2::guides(
      color = ggplot2::guide_colorbar(
        direction = "vertical",
        title.position = "top"
      )
    ) +
    ggplot2::ggtitle("Total carbon biomass")

  chl_data <- data.frame(
    station_short = station_biomass$STATION_NAME_SHORT,
    latitude = station_biomass$LATITUDE_WGS84_SWEREF99_DD,
    longitude = station_biomass$LONGITUDE_WGS84_SWEREF99_DD,
    chl_mean = station_biomass$chl_mean,
    stringsAsFactors = FALSE
  )
  chl_map <- create_chl_map(chl_data, title = "FerryBox chlorophyll fluorescence")

  list(biomass_map = biomass_map, chl_map = chl_map)
}

#' Create a standalone chlorophyll map
#'
#' @param chl_summary Data frame with columns: station_short, latitude,
#'   longitude, chl_mean.
#' @param title Plot title string.
#' @return A ggplot object.
#' @export
create_chl_map <- function(chl_summary, title = "Chlorophyll") {
  base_map <- base_sweden_map()

  base_map +
    ggplot2::geom_point(
      data = chl_summary,
      ggplot2::aes(x = .data$longitude, y = .data$latitude,
                   color = .data$chl_mean),
      size = 5
    ) +
    ggrepel::geom_text_repel(
      data = chl_summary,
      ggplot2::aes(x = .data$longitude, y = .data$latitude,
                   label = .data$station_short),
      size = 3, min.segment.length = Inf, segment.color = NA,
      box.padding = 0.5
    ) +
    cmocean::scale_color_cmocean(
      name = "algae"
    ) +
    ggplot2::labs(
      color = expression(paste("Chl (", mu, "g/L)"))
    ) +
    ggplot2::guides(
      color = ggplot2::guide_colorbar(
        direction = "vertical",
        title.position = "top"
      )
    ) +
    ggplot2::ggtitle(title)
}

#' Create an image concentration map from cruise metadata
#'
#' Plots per-sample image concentration (images per litre) along the cruise
#' track, showing spatial distribution of cell abundance as measured by the
#' IFCB.
#'
#' @param image_counts Data frame from \code{fetch_image_counts()} with
#'   columns: latitude, longitude, n_images, ml_analyzed.
#' @param legend_position Legend position. Default \code{"right"}.
#' @param title Optional plot title string.
#' @return A ggplot object.
#' @export
create_image_count_map <- function(image_counts, legend_position = "right",
                                   title = NULL) {
  scientific_math_labels <- function(x) {
    labs <- lapply(x, function(val) {
      if (is.na(val)) return(quote(NA))
      if (val == 0) return(quote(0))

      expn <- floor(log10(abs(val)))
      mant <- signif(val / (10^expn), 3)

      if (isTRUE(all.equal(abs(mant), 1, tolerance = 1e-12))) {
        if (mant < 0) {
          bquote(-10^.(expn))
        } else {
          bquote(10^.(expn))
        }
      } else {
        bquote(.(mant) %*% 10^.(expn))
      }
    })
    as.expression(labs)
  }

  plot_data <- image_counts
  if ("ml_analyzed" %in% names(plot_data) &&
      all(!is.na(plot_data$ml_analyzed)) &&
      all(plot_data$ml_analyzed > 0)) {
    plot_data$images_per_liter <- plot_data$n_images /
      (plot_data$ml_analyzed / 1000)
    # Two-line label keeps the colorbar title within the legend column width
    legend_name <- "Images\n(counts/L)"
  } else {
    plot_data$images_per_liter <- plot_data$n_images
    legend_name <- "Images"
  }

  vertical_legend <- identical(legend_position, "right")

  base_map <- base_sweden_map()

  base_map +
    ggplot2::geom_point(
      data = plot_data,
      ggplot2::aes(x = .data$longitude, y = .data$latitude,
                   color = .data$images_per_liter),
      size = 2.5, alpha = 0.8
    ) +
    ggplot2::scale_color_viridis_c(
      option = "plasma",
      name = legend_name,
      labels = scientific_math_labels
    ) +
    ggplot2::guides(color = ggplot2::guide_colorbar(
      direction = if (vertical_legend) "vertical" else "horizontal",
      title.position = "top"
    )) +
    ggplot2::theme(
      legend.position  = legend_position,
      legend.direction = if (vertical_legend) "vertical" else "horizontal",
      legend.key.width  = ggplot2::unit(if (vertical_legend) 0.6 else 1.5, "cm"),
      legend.key.height = ggplot2::unit(if (vertical_legend) 1.8 else 0.4, "cm")
    ) +
    if (!is.null(title)) ggplot2::ggtitle(title) else NULL
}

#' Get HAB species names from taxa lookup
#'
#' @param taxa_lookup A data.frame with columns \code{name} and \code{HAB}.
#'   If NULL or missing \code{HAB} column, returns empty character vector.
#' @return Character vector of scientific names flagged as HAB.
#' @keywords internal
get_hab_species <- function(taxa_lookup) {
  if (is.null(taxa_lookup) || !"HAB" %in% names(taxa_lookup)) {
    return(character(0))
  }
  is_hab <- taxa_lookup$HAB == TRUE & !is.na(taxa_lookup$HAB)
  sflag <- if ("sflag" %in% names(taxa_lookup)) taxa_lookup$sflag else rep("", nrow(taxa_lookup))
  sflag[is.na(sflag)] <- ""
  unique(trimws(paste(taxa_lookup$name[is_hab], sflag[is_hab])))
}
