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
      size = 3, min.segment.length = 0, segment.color = "gray50",
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
#' @param legend_position Legend position. Default \code{"bottom"}.
#' @return A ggplot object.
#' @export
create_image_count_map <- function(image_counts, legend_position = "bottom") {
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

  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

  plot_data <- image_counts
  if ("ml_analyzed" %in% names(plot_data) &&
      all(!is.na(plot_data$ml_analyzed)) &&
      all(plot_data$ml_analyzed > 0)) {
    plot_data$images_per_liter <- plot_data$n_images /
      (plot_data$ml_analyzed / 1000)
    legend_name <- "Abundance\n(counts/L)"
  } else {
    plot_data$images_per_liter <- plot_data$n_images
    legend_name <- "Abundance"
  }

  vertical_legend <- identical(legend_position, "right")

  ggplot2::ggplot() +
    ggplot2::geom_sf(data = world, fill = "gray95", color = "gray70") +
    ggplot2::coord_sf(xlim = c(10, 22), ylim = c(54, 60), expand = FALSE) +
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
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "aliceblue"),
      axis.title = ggplot2::element_blank(),
      legend.position = legend_position,
      legend.direction = if (vertical_legend) "vertical" else "horizontal",
      legend.key.width = ggplot2::unit(if (vertical_legend) 0.6 else 1.5, "cm"),
      legend.key.height = ggplot2::unit(if (vertical_legend) 1.8 else 0.4, "cm"),
      legend.title.position = "top",
      legend.title = ggplot2::element_text(hjust = 0.5)
    )
}

#' Create a heatmap of biovolume by species and station
#'
#' HAB species are marked with a red asterisk (*) on the y-axis labels.
#'
#' @param wide_summary Wide-format data from \code{create_wide_summary()}.
#' @param taxa_lookup Optional taxa lookup table with \code{HAB} column. If
#'   provided, HAB species are annotated with a red asterisk on the y-axis.
#' @param title Plot title.
#' @param sample_counts Optional named integer vector mapping station_date
#'   column names to number of samples. If provided, \code{n = X} is appended
#'   to each x-axis label.
#' @return A ggplot object.
#' @export
create_heatmap <- function(wide_summary, taxa_lookup = NULL, title = "",
                           sample_counts = NULL) {
  station_date_order <- names(wide_summary)[-1]

  long_data <- tidyr::pivot_longer(
    wide_summary,
    cols = -"scientific_name",
    names_to = "station_date",
    values_to = "biovolume"
  )

  long_data$station_date <- factor(long_data$station_date,
                                   levels = station_date_order)

  species_order <- stats::aggregate(
    biovolume ~ scientific_name,
    data = long_data,
    FUN = sum,
    na.rm = TRUE
  )
  species_order <- species_order$scientific_name[
    order(species_order$biovolume, decreasing = TRUE)
  ]

  # Identify HAB species
  hab_species <- get_hab_species(taxa_lookup)
  hab_in_plot <- intersect(species_order, hab_species)

  # Build y-axis labels: plain text with sflag, asterisk suffix for HAB
  base_labels <- format_taxon_labels(species_order, taxa_lookup, format = "plain")
  display_labels <- ifelse(
    species_order %in% hab_species,
    paste0(base_labels, "*"),
    base_labels
  )
  names(display_labels) <- species_order
  label_colors <- ifelse(species_order %in% hab_species, "red", "black")

  p <- ggplot2::ggplot(long_data, ggplot2::aes(
    x = .data$station_date,
    y = factor(.data$scientific_name, levels = species_order),
    fill = .data$biovolume
  )) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_x_discrete(
      labels = function(x) {
        base <- sub("_", "\n", x)
        if (!is.null(sample_counts)) {
          n <- sample_counts[x]
          base <- ifelse(
            !is.na(n),
            paste0(base, "\nn = ", n),
            base
          )
        }
        base
      }
    ) +
    ggplot2::scale_y_discrete(labels = display_labels) +
    ggplot2::scale_fill_viridis_c(option = "viridis", na.value = "grey90") +
    ggplot2::labs(x = "", y = "",
                  fill = expression(paste("Biovolume (mm"^3, "/L)")),
                  title = title) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 45, hjust = 1, vjust = 1, lineheight = 0.9, size = 9
      ),
      axis.text.y = ggplot2::element_text(size = 10, color = label_colors),
      panel.grid = ggplot2::element_blank(),
      plot.caption = ggtext::element_markdown()
    )

  if (length(hab_in_plot) > 0) {
    p <- p + ggplot2::labs(
      caption = "<span style='color:red'>*</span> Potentially harmful taxon"
    )
  }

  p
}

#' Create a stacked bar chart of relative biovolume
#'
#' HAB species are marked with a red asterisk (*) in the legend.
#'
#' @param wide_summary Wide-format data from \code{create_wide_summary()}.
#' @param taxa_lookup Optional taxa lookup table with \code{HAB} column.
#' @param n_top Number of top taxa to show individually. Default 10.
#' @param title Plot title.
#' @return A ggplot object.
#' @export
create_stacked_bar <- function(wide_summary, taxa_lookup = NULL,
                               n_top = 10, title = "") {
  station_date_order <- names(wide_summary)[-1]

  long_data <- tidyr::pivot_longer(
    wide_summary,
    cols = -"scientific_name",
    names_to = "station_date",
    values_to = "biovolume"
  )

  # Get top taxa
  taxa_totals <- stats::aggregate(
    biovolume ~ scientific_name,
    data = long_data,
    FUN = sum,
    na.rm = TRUE
  )
  taxa_totals <- taxa_totals[order(taxa_totals$biovolume, decreasing = TRUE), ]
  top_taxa <- utils::head(taxa_totals$scientific_name, n_top)

  # Group remainder as "Other taxa"
  long_data$scientific_name <- ifelse(
    long_data$scientific_name %in% top_taxa,
    long_data$scientific_name, "Other taxa"
  )
  long_data$station_date <- factor(long_data$station_date,
                                   levels = station_date_order)

  # Compute relative biovolume
  station_totals <- stats::aggregate(
    biovolume ~ station_date,
    data = long_data,
    FUN = sum,
    na.rm = TRUE
  )
  names(station_totals)[2] <- "total_bv"
  long_data <- merge(long_data, station_totals, by = "station_date")
  long_data$rel_biovolume <- ifelse(
    long_data$total_bv > 0,
    (long_data$biovolume / long_data$total_bv) * 100, 0
  )

  # Aggregate (in case multiple rows per taxon-station after grouping)
  plot_data <- stats::aggregate(
    rel_biovolume ~ scientific_name + station_date,
    data = long_data,
    FUN = sum,
    na.rm = TRUE
  )
  plot_data$scientific_name <- factor(
    plot_data$scientific_name,
    levels = c(top_taxa, "Other taxa")
  )

  # Labels: station on first line, date on second
  plot_data$label <- sub("_", "\n", as.character(plot_data$station_date))
  label_order <- sub("_", "\n", station_date_order)
  plot_data$label <- factor(plot_data$label, levels = label_order)

  fill_colors <- c(viridis::viridis(length(top_taxa)), "grey70")

  # Annotate legend with HTML italic and red asterisk for HAB
  hab_species <- get_hab_species(taxa_lookup)
  legend_labels <- c(top_taxa, "Other taxa")
  base_legend <- format_taxon_labels(legend_labels, taxa_lookup)
  display_legend <- ifelse(
    legend_labels %in% hab_species,
    paste0("<span style='color:red'>", base_legend, "*</span>"),
    base_legend
  )
  names(fill_colors) <- legend_labels
  names(display_legend) <- legend_labels

  hab_in_plot <- intersect(top_taxa, hab_species)

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(
    x = .data$label,
    y = .data$rel_biovolume,
    fill = .data$scientific_name
  )) +
    ggplot2::geom_bar(stat = "identity", color = "white") +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(x, "%")
    ) +
    ggplot2::scale_fill_manual(values = fill_colors, labels = display_legend,
                               drop = FALSE) +
    ggplot2::labs(x = "", y = "Relative Biovolume (%)",
                  fill = paste0("Top ", n_top, " taxa"),
                  title = title) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 45, hjust = 1, vjust = 1, lineheight = 0.9, size = 9
      ),
      panel.grid.major.x = ggplot2::element_blank(),
      legend.text = ggtext::element_markdown(),
      plot.caption = ggtext::element_markdown()
    )

  if (length(hab_in_plot) > 0) {
    p <- p + ggplot2::labs(
      caption = "<span style='color:red'>*</span> Potentially harmful taxon"
    )
  }

  p
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
