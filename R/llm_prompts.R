#' Attach collapsed text groups to station summary data
#'
#' @param x Station-level data frame with \code{name} and optionally
#'   \code{AphiaID}.
#' @param phyto_groups Group assignment table passed through
#'   \code{collapse_phyto_groups_for_text()}.
#' @return \code{x} with an added \code{text_group} column.
#' @keywords internal
attach_text_groups <- function(x, phyto_groups = NULL) {
  x$detailed_group <- "Other"
  x$text_group <- "Other"
  groups <- normalize_phyto_groups_for_text(phyto_groups)
  if (is.null(groups) || nrow(groups) == 0) {
    return(x)
  }

  by_cols <- intersect(c("name", "AphiaID"), names(x))
  by_cols <- intersect(by_cols, names(groups))
  if (length(by_cols) == 0) {
    by_cols <- "name"
  }

  merged <- merge(
    x,
    groups,
    by = by_cols,
    all.x = TRUE,
    suffixes = c("", ".group")
  )
  merged$detailed_group <- merged$detailed_group.group
  merged$text_group <- merged$text_group.group
  merged$detailed_group.group <- NULL
  merged$text_group.group <- NULL
  merged$detailed_group[is.na(merged$detailed_group)] <- "Other"
  merged$text_group[is.na(merged$text_group)] <- "Other"

  # Restore original row order after merge.
  if (".row_id_tmp" %in% names(merged)) {
    merged <- merged[order(merged$.row_id_tmp), , drop = FALSE]
    merged$.row_id_tmp <- NULL
  }
  merged
}

#' Build a station-level group summary block for LLM prompts
#'
#' @param station_data Station summary rows for one visit, including
#'   \code{detailed_group} or \code{text_group}.
#' @param group_col Column name to summarize.
#' @param heading Heading line for the block.
#' @param n_top Optional integer: limit to the top N groups.
#' @return Character string.
#' @keywords internal
build_station_group_summary <- function(station_data, group_col, heading,
                                        n_top = NULL) {
  if (!group_col %in% names(station_data) || nrow(station_data) == 0) {
    return("")
  }

  totals <- stats::aggregate(
    biovolume_mm3_per_liter ~ group,
    data = data.frame(
      group = station_data[[group_col]],
      biovolume_mm3_per_liter = station_data$biovolume_mm3_per_liter
    ),
    FUN = sum,
    na.rm = TRUE
  )
  totals <- totals[totals$biovolume_mm3_per_liter > 0, , drop = FALSE]
  if (nrow(totals) == 0) return("")
  totals <- totals[order(-totals$biovolume_mm3_per_liter), , drop = FALSE]
  if (!is.null(n_top)) totals <- utils::head(totals, n_top)

  total_bv <- sum(station_data$biovolume_mm3_per_liter, na.rm = TRUE)
  group_lines <- vapply(seq_len(nrow(totals)), function(i) {
    grp <- totals$group[i]
    grp_data <- station_data[station_data[[group_col]] == grp, , drop = FALSE]
    grp_data <- grp_data[order(-grp_data$biovolume_mm3_per_liter), , drop = FALSE]
    top_names <- unique(grp_data$display_name)
    top_names <- utils::head(top_names[nzchar(top_names)], 3)
    pct <- if (total_bv > 0) {
      sprintf("%.1f%%", 100 * totals$biovolume_mm3_per_liter[i] / total_bv)
    } else {
      "0%"
    }
    sprintf("  %s: %.3f mm3/L (%s of total); top taxa: %s",
            grp, totals$biovolume_mm3_per_liter[i], pct,
            paste(top_names, collapse = ", "))
  }, character(1))

  paste0(
    "\n", heading, "\n",
    paste(group_lines, collapse = "\n")
  )
}

#' Format station data as a text summary for the LLM prompt
#'
#' @param station_data Data frame with station_summary rows for one visit.
#' @param taxa_lookup Optional taxa lookup with \code{HAB} and
#'   \code{warning_level} columns.
#' @param unclassified_pct Optional numeric; percentage of unclassified
#'   detections at this station, used for context notes.
#' @param phyto_groups Optional phytoplankton group table used to provide
#'   explicit group assignments in the prompt text.
#' @return Character string describing the station data.
#' @keywords internal
format_station_data_for_prompt <- function(station_data, taxa_lookup = NULL,
                                           unclassified_pct = NULL,
                                           phyto_groups = NULL) {
  station_data$.row_id_tmp <- seq_len(nrow(station_data))
  station_data <- attach_text_groups(station_data, phyto_groups)
  station_name <- station_data$STATION_NAME_SHORT[1]
  full_name <- station_data$STATION_NAME[1]
  coast <- station_data$COAST[1]
  visit_date <- as.character(station_data$visit_date[1])
  region <- if (coast == "EAST") "Baltic Sea" else "West Coast (Skagerrak/Kattegat)"

  # Sort by biovolume descending
  station_data <- station_data[order(-station_data$biovolume_mm3_per_liter), ]

  # Total biomass

  total_carbon <- sum(station_data$carbon_ug_per_liter, na.rm = TRUE)
  total_biovolume <- sum(station_data$biovolume_mm3_per_liter, na.rm = TRUE)
  total_counts <- sum(station_data$counts_per_liter, na.rm = TRUE)
  n_taxa <- nrow(station_data)

  # Chlorophyll if available
  chl_info <- ""
  if ("chl_mean" %in% names(station_data) &&
      !all(is.na(station_data$chl_mean))) {
    chl_val <- station_data$chl_mean[1]
    if (!is.na(chl_val)) {
      chl_info <- sprintf("Chlorophyll fluorescence (mean): %.2f", chl_val)
    }
  }

  # Top taxa by biovolume
  top_n <- min(15, nrow(station_data))
  top_taxa <- station_data[seq_len(top_n), ]

  # Compute display names (name + sflag) for each row
  make_display <- function(df) {
    sflag <- if ("sflag" %in% names(df)) df$sflag else ""
    sflag[is.na(sflag)] <- ""
    trimws(paste(df$name, sflag))
  }
  station_data$display_name <- make_display(station_data)
  top_taxa$display_name <- station_data$display_name[seq_len(nrow(top_taxa))]
  dominant_group_block <- build_station_group_summary(
    station_data,
    group_col = "detailed_group",
    heading = "All phytoplankton groups present (use these exact group names; do not infer group membership from scientific names):"
  )
  key_species_group_block <- build_station_group_summary(
    station_data,
    group_col = "text_group",
    heading = "Top 3 groups by biovolume (provide detailed species breakdown for these groups only; mention remaining groups only briefly):",
    n_top = 3
  )

  # Identify HAB species (using display names)
  hab_species <- character(0)
  if (!is.null(taxa_lookup) && "HAB" %in% names(taxa_lookup)) {
    sflag_lu <- if ("sflag" %in% names(taxa_lookup)) taxa_lookup$sflag else ""
    sflag_lu[is.na(sflag_lu)] <- ""
    hab_display <- trimws(paste(taxa_lookup$name, sflag_lu))[taxa_lookup$HAB == TRUE]
    hab_species <- intersect(station_data$display_name, hab_display)
  }

  # Build a named numeric vector: display_name -> warning_level threshold (cells/L)
  # Only includes taxa that have a non-NA warning_level in taxa_lookup.
  warning_thresholds <- numeric(0)
  if (!is.null(taxa_lookup) && "warning_level" %in% names(taxa_lookup)) {
    sflag_lu <- if ("sflag" %in% names(taxa_lookup)) taxa_lookup$sflag else ""
    sflag_lu[is.na(sflag_lu)] <- ""
    lu_display <- trimws(paste(taxa_lookup$name, sflag_lu))
    wl <- taxa_lookup$warning_level
    has_wl <- !is.na(wl) & nzchar(as.character(wl))
    warning_thresholds <- setNames(as.numeric(wl[has_wl]), lu_display[has_wl])
  }

  # Build taxa table
  taxa_lines <- vapply(seq_len(nrow(top_taxa)), function(i) {
    row <- top_taxa[i, ]
    hab_flag <- if (row$display_name %in% hab_species) " [HAB]" else ""
    # Flag taxa that exceed their warning threshold at this station
    warn_flag <- ""
    if (row$display_name %in% names(warning_thresholds)) {
      thresh <- warning_thresholds[[row$display_name]]
      if (!is.na(thresh) && row$counts_per_liter >= thresh) {
        warn_flag <- sprintf(" [WARNING: %.0f cells/L, threshold %.0f cells/L]",
                             row$counts_per_liter, thresh)
      }
    }
    pct <- if (total_biovolume > 0) {
      sprintf("%.1f%%", row$biovolume_mm3_per_liter / total_biovolume * 100)
    } else {
      "0%"
    }
    sprintf("  %s%s%s: %.3f mm3/L (%s of total), %.0f counts/L",
            row$display_name, hab_flag, warn_flag,
            row$biovolume_mm3_per_liter, pct,
            row$counts_per_liter)
  }, character(1))

  # Additional HAB species in lower abundances (not in top_n)
  hab_in_sample <- station_data$display_name[station_data$display_name %in% hab_species]
  hab_not_in_top <- setdiff(hab_in_sample, top_taxa$display_name)
  hab_extra <- ""
  if (length(hab_not_in_top) > 0) {
    hab_extra <- paste0(
      "\nOther HAB species present (lower abundance): ",
      paste(hab_not_in_top, collapse = ", ")
    )
  }

  # Collect all taxa (including those outside the top_n) that exceed a warning level
  warning_exceeded <- character(0)
  if (length(warning_thresholds) > 0) {
    for (dn in names(warning_thresholds)) {
      idx <- which(station_data$display_name == dn)
      if (length(idx) > 0) {
        counts <- station_data$counts_per_liter[idx[1]]
        thresh <- warning_thresholds[[dn]]
        if (!is.na(counts) && !is.na(thresh) && counts >= thresh) {
          warning_exceeded <- c(warning_exceeded,
            sprintf("%s (%.0f cells/L, threshold %.0f cells/L)", dn, counts, thresh))
        }
      }
    }
  }
  warning_note <- ""
  if (length(warning_exceeded) > 0) {
    warning_note <- paste0(
      "\nIMPORTANT -- Taxa exceeding recommended warning levels at this station:\n",
      paste0("  ", warning_exceeded, collapse = "\n"),
      "\nYou MUST explicitly state in the description that the abundance of ",
      "these taxa exceeds the recommended warning level."
    )
  }

  # Note about high unclassified fraction
  unclass_note <- ""
  if (!is.null(unclassified_pct) && unclassified_pct > 80) {
    unclass_note <- sprintf(
      "\nNOTE: %.0f%% of all images at this station were unclassified. ",
      unclassified_pct
    )
    unclass_note <- paste0(
      unclass_note,
      "This means the classified taxa above represent only a small fraction ",
      "of the total phytoplankton community. Mention this in the description."
    )
  }

  paste0(
    "Station: ", station_name, " (", full_name, ")\n",
    "Region: ", region, "\n",
    "Date: ", visit_date, "\n",
    "Number of taxa: ", n_taxa, "\n",
    "Total counts per liter: ", sprintf("%.0f", total_counts), "\n",
    "Total biovolume: ", sprintf("%.4f", total_biovolume), " mm3/L\n",
    "Total carbon biomass: ", sprintf("%.2f", total_carbon), " ug/L\n",
    if (nzchar(chl_info)) paste0(chl_info, "\n") else "",
    "\nTop taxa by biovolume:\n",
    paste(taxa_lines, collapse = "\n"),
    dominant_group_block,
    key_species_group_block,
    hab_extra,
    warning_note,
    unclass_note
  )
}

#' Format cruise-level data summary for LLM prompt
#'
#' @param station_summary Full station_summary data frame.
#' @param taxa_lookup Optional taxa lookup with \code{HAB} and
#'   \code{warning_level} columns.
#' @param unclassified_fractions Optional named list of per-visit unclassified
#'   percentages used to annotate station lines.
#' @param phyto_groups Optional phytoplankton group table used to provide
#'   explicit group assignments in the prompt text.
#' @return Character string with cruise-level overview.
#' @keywords internal
format_cruise_summary_for_prompt <- function(station_summary, taxa_lookup = NULL,
                                             unclassified_fractions = NULL,
                                             phyto_groups = NULL) {
  station_summary$.row_id_tmp <- seq_len(nrow(station_summary))
  station_summary <- attach_text_groups(station_summary, phyto_groups)
  visits <- unique(station_summary[, c("STATION_NAME_SHORT", "COAST",
                                        "visit_date", "visit_id")])
  visits <- visits[order(visits$COAST, visits$visit_date), ]

  # HAB species lookup (using display names)
  hab_species <- character(0)
  if (!is.null(taxa_lookup) && "HAB" %in% names(taxa_lookup)) {
    sflag_lu <- if ("sflag" %in% names(taxa_lookup)) taxa_lookup$sflag else ""
    sflag_lu[is.na(sflag_lu)] <- ""
    hab_species <- trimws(paste(taxa_lookup$name, sflag_lu))[taxa_lookup$HAB == TRUE]
  }

  # Warning level lookup: named numeric vector display_name -> threshold (cells/L)
  warning_thresholds <- numeric(0)
  if (!is.null(taxa_lookup) && "warning_level" %in% names(taxa_lookup)) {
    sflag_lu <- if ("sflag" %in% names(taxa_lookup)) taxa_lookup$sflag else ""
    sflag_lu[is.na(sflag_lu)] <- ""
    lu_display <- trimws(paste(taxa_lookup$name, sflag_lu))
    wl <- taxa_lookup$warning_level
    has_wl <- !is.na(wl) & nzchar(as.character(wl))
    warning_thresholds <- setNames(as.numeric(wl[has_wl]), lu_display[has_wl])
  }

  # Helper to compute display names for a data frame
  make_display <- function(df) {
    sflag <- if ("sflag" %in% names(df)) df$sflag else ""
    sflag[is.na(sflag)] <- ""
    trimws(paste(df$name, sflag))
  }

  # Per-station summaries
  station_lines <- vapply(seq_len(nrow(visits)), function(i) {
    v <- visits[i, ]
    sdata <- station_summary[station_summary$visit_id == v$visit_id, ]
    sdata <- sdata[order(-sdata$biovolume_mm3_per_liter), ]
    sdata$display_name <- make_display(sdata)

    total_bv <- sum(sdata$biovolume_mm3_per_liter, na.rm = TRUE)
    total_carbon <- sum(sdata$carbon_ug_per_liter, na.rm = TRUE)
    total_counts <- sum(sdata$counts_per_liter, na.rm = TRUE)
    n_taxa <- nrow(sdata)

    # Top 5 species
    top5 <- head(sdata, 5)
    top_names <- vapply(seq_len(nrow(top5)), function(j) {
      hab_flag <- if (top5$display_name[j] %in% hab_species) "*" else ""
      paste0(top5$display_name[j], hab_flag)
    }, character(1))

    # HAB present
    hab_found <- intersect(sdata$display_name, hab_species)
    hab_note <- if (length(hab_found) > 0) {
      paste0(" | HAB: ", paste(hab_found, collapse = ", "))
    } else {
      ""
    }

    # Warning level exceedances at this station
    warn_exceeded <- character(0)
    if (length(warning_thresholds) > 0) {
      for (dn in names(warning_thresholds)) {
        idx <- which(sdata$display_name == dn)
        if (length(idx) > 0) {
          counts <- sdata$counts_per_liter[idx[1]]
          thresh <- warning_thresholds[[dn]]
          if (!is.na(counts) && !is.na(thresh) && counts >= thresh) {
            warn_exceeded <- c(warn_exceeded,
              sprintf("%s (%.0f>=%.0f cells/L)", dn, counts, thresh))
          }
        }
      }
    }
    warn_note <- if (length(warn_exceeded) > 0) {
      paste0(" | WARNING EXCEEDED: ", paste(warn_exceeded, collapse = ", "))
    } else {
      ""
    }

    summarize_groups <- function(group_col, label) {
      group_totals <- stats::aggregate(
        biovolume_mm3_per_liter ~ group,
        data = data.frame(
          group = sdata[[group_col]],
          biovolume_mm3_per_liter = sdata$biovolume_mm3_per_liter
        ),
        FUN = sum,
        na.rm = TRUE
      )
      group_totals <- group_totals[group_totals$biovolume_mm3_per_liter > 0, ,
                                   drop = FALSE]
      group_totals <- group_totals[order(-group_totals$biovolume_mm3_per_liter), ,
                                   drop = FALSE]
      if (nrow(group_totals) == 0) return("")
      paste0(" | ", label, ": ",
             paste(sprintf("%s %.3f mm3/L",
                           group_totals$group,
                           group_totals$biovolume_mm3_per_liter),
                   collapse = ", "))
    }
    dominant_group_note <- summarize_groups("detailed_group", "Dominant groups")
    key_species_group_note <- summarize_groups("text_group", "Key-species groups")

    # Chlorophyll
    chl_note <- ""
    if ("chl_mean" %in% names(sdata) && !all(is.na(sdata$chl_mean))) {
      chl_val <- sdata$chl_mean[1]
      if (!is.na(chl_val)) {
        chl_note <- sprintf(" | Chl: %.2f", chl_val)
      }
    }

    region <- if (v$COAST == "EAST") "Baltic" else "West"

    # Unclassified note if >80%
    unclass_note <- ""
    if (!is.null(unclassified_fractions)) {
      pct <- unclassified_fractions[[v$visit_id]]
      if (!is.null(pct) && pct > 80) {
        unclass_note <- sprintf(" | UNCLASSIFIED: %.0f%%", pct)
      }
    }

    sprintf("  %s (%s, %s): %d taxa, %.0f counts/L, %.4f mm3/L biovol, %.2f ug/L carbon | Top: %s%s%s%s%s%s%s",
            v$STATION_NAME_SHORT, region, v$visit_date,
            n_taxa, total_counts, total_bv, total_carbon,
            paste(top_names, collapse = ", "),
            hab_note, warn_note, dominant_group_note, key_species_group_note,
            chl_note, unclass_note)
  }, character(1))

  # Add a cruise-level warning summary block so the LLM sees all exceedances
  # prominently regardless of where they appear in the per-station lines.
  all_warnings <- character(0)
  if (length(warning_thresholds) > 0) {
    station_summary$display_name_tmp <- make_display(station_summary)
    for (dn in names(warning_thresholds)) {
      thresh <- warning_thresholds[[dn]]
      idx <- which(station_summary$display_name_tmp == dn)
      if (length(idx) == 0) next
      for (ii in idx) {
        counts <- station_summary$counts_per_liter[ii]
        if (!is.na(counts) && !is.na(thresh) && counts >= thresh) {
          stn <- station_summary$STATION_NAME_SHORT[ii]
          all_warnings <- c(all_warnings,
            sprintf("%s at %s (%.0f cells/L, threshold %.0f cells/L)",
                    dn, stn, counts, thresh))
        }
      }
    }
    station_summary$display_name_tmp <- NULL
  }

  warning_block <- ""
  if (length(all_warnings) > 0) {
    warning_block <- paste0(
      "\nIMPORTANT -- Taxa exceeding recommended warning levels during this cruise:\n",
      paste0("  ", unique(all_warnings), collapse = "\n"),
      "\nYou MUST explicitly mention each of these in the summary text, ",
      "stating that the abundance exceeds the recommended warning level."
    )
  }

  paste0(paste(station_lines, collapse = "\n"), warning_block)
}
