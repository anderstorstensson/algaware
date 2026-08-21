#' Resolve the heatmap group of each taxon
#'
#' Looks up each name in \code{phyto_groups} (trying the exact name first,
#' then with any trailing sflag such as "spp." stripped, since heatmap row
#' names carry the suffix while \code{phyto_groups$name} is the bare WoRMS
#' name). Unmatched taxa become "Other".
#'
#' Unlike the pie charts, the heatmap shows all ciliates as one block, so
#' the pie-chart-only group \code{"Mesodinium spp."} is folded into
#' \code{"Ciliates"} here.
#'
#' @param scientific_names Character vector of taxon names.
#' @param phyto_groups Data frame with columns \code{name} and
#'   \code{phyto_group}.
#' @return Character vector of group names, one per input name.
#' @keywords internal
heatmap_group_of <- function(scientific_names, phyto_groups) {
  stripped <- sub(" (sp\\.|spp\\.|group)$", "", scientific_names)
  group <- phyto_groups$phyto_group[
    match(scientific_names, phyto_groups$name)
  ]
  miss <- is.na(group)
  group[miss] <- phyto_groups$phyto_group[
    match(stripped[miss], phyto_groups$name)
  ]
  group[is.na(group)] <- "Other"
  group[group == "Mesodinium spp."] <- "Ciliates"
  group
}

#' Canonical heatmap group order
#'
#' Diatoms, Dinoflagellates, Cyanobacteria, Cryptophytes, Ciliates,
#' Silicoflagellates, then any additional groups alphabetically, with
#' "Other" always last.
#'
#' @param groups Character vector of group names present.
#' @return Character vector of group levels (top-to-bottom display order).
#' @keywords internal
heatmap_group_levels <- function(groups) {
  canonical <- c("Diatoms", "Dinoflagellates", "Cyanobacteria",
                 "Cryptophytes", "Ciliates", "Silicoflagellates")
  extra <- sort(setdiff(groups, c(canonical, "Other")))
  c(canonical, extra, "Other")
}

#' Order heatmap taxa by phytoplankton group, then alphabetically
#'
#' Groups follow \code{heatmap_group_levels()}; taxa are sorted
#' alphabetically within each group.
#'
#' @param scientific_names Character vector of taxon names to order.
#' @param phyto_groups Data frame with columns \code{name} and
#'   \code{phyto_group}.
#' @return \code{scientific_names} reordered (top-to-bottom display order).
#' @keywords internal
order_taxa_by_group <- function(scientific_names, phyto_groups) {
  group <- heatmap_group_of(scientific_names, phyto_groups)
  levels <- heatmap_group_levels(group)
  scientific_names[order(match(group, levels), scientific_names)]
}

#' Create a heatmap of biovolume by species and station
#'
#' HAB species are marked with a red asterisk (*) on the y-axis labels.
#' When \code{phyto_groups} is supplied, rows are split into one panel per
#' phytoplankton group with a coloured group label on the left (colours
#' match the pie-chart group map).
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
#'   provided, taxa are grouped into labelled panels by phytoplankton group
#'   and ordered alphabetically within each group; otherwise by total
#'   biovolume (descending).
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

  use_groups <- !is.null(phyto_groups) && nrow(phyto_groups) > 0
  if (use_groups) {
    taxa <- unique(long_data$scientific_name)
    group_of <- stats::setNames(heatmap_group_of(taxa, phyto_groups), taxa)
    group_levels <- heatmap_group_levels(group_of)
    long_data$phyto_group <- factor(group_of[long_data$scientific_name],
                                    levels = group_levels)
    # rev(): factor levels run bottom-to-top on a discrete y-axis, so A->Z
    # order reads top-to-bottom within each panel.
    species_order <- rev(order_taxa_by_group(taxa, phyto_groups))
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

  if (use_groups) {
    p <- p + heatmap_group_facets(group_levels)
  }

  if (length(hab_in_plot) > 0) {
    p <- p + ggplot2::labs(
      caption = "<span style='color:red'>*</span> Potentially harmful taxon"
    )
  }

  p
}

#' Facet layers that draw coloured group strips on the heatmap
#'
#' One panel per group (rows sized to the number of taxa), with the group
#' name on the left, coloured with the shared pie-chart palette. Labels are
#' horizontal (not rotated) so single-row groups cannot clip the text.
#' A thin grey background band separates the panels. Groups without a
#' palette entry fall back to the "Other" grey.
#'
#' @param group_levels Character vector of group levels in display order.
#' @return A list of ggplot2 components to add to a plot.
#' @keywords internal
heatmap_group_facets <- function(group_levels) {
  palette <- phyto_group_colors()
  colors <- palette[group_levels]
  colors[is.na(colors)] <- palette[["Other"]]
  strip_labels <- stats::setNames(
    paste0("<span style='color:", colors, "'><b>", group_levels,
           "</b></span>"),
    group_levels
  )

  list(
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data$phyto_group),
      scales = "free_y",
      space = "free_y",
      switch = "y",
      labeller = ggplot2::as_labeller(strip_labels)
    ),
    ggplot2::theme(
      strip.placement = "outside",
      strip.text.y.left = ggtext::element_markdown(
        angle = 0, hjust = 1, size = 9,
        margin = ggplot2::margin(r = 5, l = 4)
      ),
      strip.background = ggplot2::element_rect(fill = "grey95",
                                               colour = NA),
      panel.spacing.y = ggplot2::unit(3, "pt")
    )
  )
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
