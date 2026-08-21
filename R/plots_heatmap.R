#' Order heatmap taxa by phytoplankton group, then alphabetically
#'
#' Groups follow the canonical order used elsewhere in AlgAware (Diatoms,
#' Dinoflagellates, Cyanobacteria, Cryptophytes, \emph{Mesodinium} spp.,
#' Silicoflagellates), then any additional groups alphabetically, with
#' "Other" always last.  Taxa are sorted alphabetically within each group;
#' taxa missing from \code{phyto_groups} fall into "Other".
#'
#' @param scientific_names Character vector of taxon names to order.
#' @param phyto_groups Data frame with columns \code{name} and
#'   \code{phyto_group}.
#' @return \code{scientific_names} reordered (top-to-bottom display order).
#' @keywords internal
order_taxa_by_group <- function(scientific_names, phyto_groups) {
  canonical <- c("Diatoms", "Dinoflagellates", "Cyanobacteria",
                 "Cryptophytes", "Mesodinium spp.", "Silicoflagellates")

  # Heatmap row names carry the sflag suffix ("Actinocyclus spp."), while
  # phyto_groups$name is the bare WoRMS name ("Actinocyclus"): try the exact
  # name first, then retry with any trailing sflag stripped.
  stripped <- sub(" (sp\\.|spp\\.|group)$", "", scientific_names)
  group <- phyto_groups$phyto_group[
    match(scientific_names, phyto_groups$name)
  ]
  miss <- is.na(group)
  group[miss] <- phyto_groups$phyto_group[
    match(stripped[miss], phyto_groups$name)
  ]
  group[is.na(group)] <- "Other"

  extra <- sort(setdiff(group, c(canonical, "Other")))
  group_levels <- c(canonical, extra, "Other")

  scientific_names[
    order(match(group, group_levels), scientific_names)
  ]
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
#' @param phyto_groups Optional data frame with columns \code{name} and
#'   \code{phyto_group} (as built from \code{assign_phyto_groups()}). If
#'   provided, taxa are ordered by phytoplankton group and alphabetically
#'   within each group; otherwise by total biovolume (descending).
#' @return A ggplot object.
#' @export
create_heatmap <- function(wide_summary, taxa_lookup = NULL, title = "",
                           sample_counts = NULL, phyto_groups = NULL) {
  station_date_order <- names(wide_summary)[-1]

  long_data <- tidyr::pivot_longer(
    wide_summary,
    cols = -"scientific_name",
    names_to = "station_date",
    values_to = "biovolume"
  )

  long_data$station_date <- factor(long_data$station_date,
                                   levels = station_date_order)

  if (!is.null(phyto_groups) && nrow(phyto_groups) > 0) {
    # rev(): factor levels run bottom-to-top on a discrete y-axis, so the
    # first group (Diatoms) and A->Z order read top-to-bottom in the plot.
    species_order <- rev(order_taxa_by_group(
      unique(long_data$scientific_name), phyto_groups
    ))
  } else {
    species_order <- stats::aggregate(
      biovolume ~ scientific_name,
      data = long_data,
      FUN = sum,
      na.rm = TRUE
    )
    species_order <- species_order$scientific_name[
      order(species_order$biovolume, decreasing = TRUE)
    ]
  }

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

  long_data$scientific_name <- factor(long_data$scientific_name,
                                      levels = species_order)

  p <- ggplot2::ggplot(long_data, ggplot2::aes(
    x = .data$station_date,
    y = .data$scientific_name,
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
