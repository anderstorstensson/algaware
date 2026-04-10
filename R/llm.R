#' Ensure all HAB taxa in text are followed by an asterisk
#'
#' Post-processes LLM-generated text to add a trailing \code{*} after any HAB
#' taxon name that is missing one. This makes HAB marking deterministic
#' regardless of whether the LLM remembered to include the asterisk.
#'
#' Genus names (single word, e.g. \emph{Pseudochattonella}) also match their
#' \emph{spp.} / \emph{sp.} form so the asterisk lands after the full mention
#' rather than mid-name.  Abbreviated species names (e.g. \emph{D. acuminata})
#' are matched alongside the full form.
#'
#' @param text Character string of plain text.
#' @param taxa_lookup Data frame with columns \code{name} and \code{HAB}.
#' @return Character string with asterisks added after every HAB taxon mention.
#' @keywords internal
ensure_hab_asterisks <- function(text, taxa_lookup) {
  if (is.null(taxa_lookup) || nrow(taxa_lookup) == 0) return(text)
  if (!"HAB" %in% names(taxa_lookup)) return(text)

  hab_rows <- taxa_lookup[taxa_lookup$HAB == TRUE & nzchar(taxa_lookup$name), ]
  if (nrow(hab_rows) == 0) return(text)

  hab_names <- unique(hab_rows$name)

  # Generate abbreviated forms for two-word species names (e.g. "D. acuminata")
  species_names <- hab_names[grepl(" ", hab_names)]
  abbreviations <- vapply(species_names, function(sp) {
    parts <- strsplit(sp, " ", fixed = TRUE)[[1]]
    paste0(substr(parts[1], 1, 1), ". ", paste(parts[-1], collapse = " "))
  }, character(1))

  all_names <- unique(c(hab_names, abbreviations))
  # Match longest names first to prevent partial-name substitutions
  all_names <- all_names[order(-nchar(all_names))]

  for (nm in all_names) {
    escaped <- gsub("([.\\\\|()\\[\\]\\{\\}^$+?])", "\\\\\\1", nm)
    if (!grepl(" ", nm)) {
      # Single-word genus names need three passes to avoid regex backtracking
      # that would otherwise cause double asterisks like "Genus* spp.*".
      #
      # Pass 1: Remove a stray asterisk placed between the genus and spp. by
      #         the LLM ("Genus* spp.*" -> "Genus spp.*").
      text <- gsub(paste0("(", escaped, ")\\*(\\s+spp?\\.)"),
                   "\\1\\2", text, perl = TRUE)
      # Pass 2: Add * after the full "Genus spp." / "Genus sp." form if absent.
      text <- gsub(paste0("(", escaped, "\\s+spp?\\.)(?!\\*)"),
                   "\\1*", text, perl = TRUE)
      # Pass 3: Add * after a bare genus that is not followed by spp. / sp.
      #         and not already marked.
      text <- gsub(paste0("(", escaped, ")(?!\\s+spp?\\.)(?!\\*)"),
                   "\\1*", text, perl = TRUE)
    } else {
      pattern <- paste0("(", escaped, ")(?!\\*)")
      text <- gsub(pattern, "\\1*", text, perl = TRUE)
    }
  }

  text
}

#' Build a formatted paragraph with italic species names and red HAB asterisks
#'
#' Parses plain text and returns an \code{officer::fpar} object with italic
#' formatting for recognized species/genus names and red bold formatting for
#' HAB asterisks.
#'
#' @param text Character string of plain text.
#' @param taxa_lookup Data frame with columns \code{name}, \code{italic},
#'   and optionally \code{HAB}.
#' @return An \code{officer::fpar} object.
#' @keywords internal
format_report_paragraph <- function(text, taxa_lookup = NULL) {
  # Font properties for the Word report. Adobe Garamond Pro is the standard
  # font used in SMHI AlgAware publications. If not installed, Word will
  # substitute a similar serif font.
  normal_prop <- officer::fp_text(font.size = 11, font.family = "Adobe Garamond Pro")
  italic_prop <- officer::fp_text(font.size = 11, italic = TRUE, font.family = "Adobe Garamond Pro")
  red_prop <- officer::fp_text(font.size = 11, color = "red", bold = TRUE, font.family = "Adobe Garamond Pro")

  if (is.null(taxa_lookup) || nrow(taxa_lookup) == 0) {
    return(officer::fpar(officer::ftext(text, normal_prop)))
  }

  # Build list of names to match: full names + abbreviated forms
  italic_names <- taxa_lookup$name[taxa_lookup$italic == TRUE &
                                     nzchar(taxa_lookup$name)]
  italic_names <- unique(italic_names)

  # Generate abbreviated forms for species (two-word names)
  species_names <- italic_names[grepl(" ", italic_names)]
  abbreviations <- vapply(species_names, function(sp) {
    parts <- strsplit(sp, " ", fixed = TRUE)[[1]]
    paste0(substr(parts[1], 1, 1), ". ", paste(parts[-1], collapse = " "))
  }, character(1))

  # Combine: match longest first to avoid partial matches
  all_names <- c(italic_names, abbreviations)
  all_names <- unique(all_names[nzchar(all_names)])
  all_names <- all_names[order(-nchar(all_names))]

  # Escape regex special chars in names, allow optional * suffix for HAB
  patterns <- vapply(all_names, function(n) {
    escaped <- gsub("([.\\\\|()\\[\\]\\{\\}^$+?])", "\\\\\\1", n)
    paste0("(", escaped, ")(\\*?)")
  }, character(1))

  combined_pattern <- paste(patterns, collapse = "|")

  # Split text on species name matches
  pieces <- list()
  remaining <- text
  while (nzchar(remaining)) {
    m <- regexpr(combined_pattern, remaining, perl = TRUE)
    if (m == -1) {
      pieces <- c(pieces, list(list(text = remaining, type = "normal")))
      break
    }

    # Text before the match
    if (m > 1) {
      before <- substr(remaining, 1, m - 1)
      pieces <- c(pieces, list(list(text = before, type = "normal")))
    }

    match_len <- attr(m, "match.length")
    matched <- substr(remaining, m, m + match_len - 1)

    # Check if it ends with HAB asterisk
    if (grepl("\\*$", matched)) {
      species_part <- sub("\\*$", "", matched)
      pieces <- c(pieces, list(
        list(text = species_part, type = "italic"),
        list(text = "*", type = "hab")
      ))
    } else {
      pieces <- c(pieces, list(list(text = matched, type = "italic")))
    }

    remaining <- substr(remaining, m + match_len, nchar(remaining))
  }

  # Post-process: any bare asterisk remaining in "normal" pieces is a HAB
  # marker the LLM placed without attaching it to a recognised species name.
  # Extract each * and reclassify it as "hab" so all asterisks are red.
  pieces <- unlist(lapply(pieces, function(p) {
    if (p$type != "normal" || !grepl("\\*", p$text)) return(list(p))
    result <- list()
    remaining <- p$text
    while (nzchar(remaining)) {
      ast <- regexpr("\\*", remaining)
      if (ast == -1) {
        result <- c(result, list(list(text = remaining, type = "normal")))
        break
      }
      if (ast > 1) {
        result <- c(result, list(
          list(text = substr(remaining, 1, ast - 1), type = "normal")
        ))
      }
      result <- c(result, list(list(text = "*", type = "hab")))
      remaining <- substr(remaining, ast + 1L, nchar(remaining))
    }
    result
  }), recursive = FALSE)

  # Build fpar from pieces
  ftext_args <- lapply(pieces, function(p) {
    prop <- switch(p$type,
      italic = italic_prop,
      hab = red_prop,
      normal_prop
    )
    officer::ftext(p$text, prop)
  })

  do.call(officer::fpar, ftext_args)
}

#' Add a formatted paragraph to a Word document
#'
#' Wrapper around \code{officer::body_add_fpar} that applies species name
#' formatting when taxa_lookup is provided. An empty paragraph is inserted
#' between consecutive paragraphs so that multi-region summaries (Baltic /
#' West Coast) are visually separated in the Word document.
#'
#' @param doc An \code{rdocx} object.
#' @param text Plain text string.
#' @param taxa_lookup Optional taxa lookup for formatting.
#' @param style Paragraph style name.
#' @return The modified \code{rdocx} object.
#' @keywords internal
add_formatted_par <- function(doc, text, taxa_lookup = NULL,
                              style = "Normal") {
  # Deterministically add * after any HAB taxon the LLM forgot to mark
  if (!is.null(taxa_lookup)) {
    text <- ensure_hab_asterisks(text, taxa_lookup)
  }

  # Split on double newlines to preserve paragraph breaks from LLM output
  paragraphs <- strsplit(text, "\n\\s*\n")[[1]]
  paragraphs <- trimws(paragraphs)
  paragraphs <- paragraphs[nzchar(paragraphs)]

  for (i in seq_along(paragraphs)) {
    # Insert blank separator between consecutive paragraphs (e.g. between
    # the Baltic Sea and West Coast sections in the summaries)
    if (i > 1) {
      doc <- officer::body_add_par(doc, "", style = "Normal")
    }
    if (is.null(taxa_lookup)) {
      doc <- officer::body_add_par(doc, paragraphs[[i]], style = style)
    } else {
      fp <- format_report_paragraph(paragraphs[[i]], taxa_lookup)
      doc <- officer::body_add_fpar(doc, fp, style = style)
    }
  }
  doc
}

#' Check if LLM text generation is available
#'
#' @return TRUE if OPENAI_API_KEY or GEMINI_API_KEY is set, FALSE otherwise.
#' @export
llm_available <- function() {
  nzchar(Sys.getenv("OPENAI_API_KEY", "")) ||
    nzchar(Sys.getenv("GEMINI_API_KEY", ""))
}

#' List available LLM providers
#'
#' @return Character vector of provider names with valid API keys.
#' @export
llm_providers <- function() {
  providers <- character(0)
  if (nzchar(Sys.getenv("OPENAI_API_KEY", ""))) {
    providers <- c(providers, "openai")
  }
  if (nzchar(Sys.getenv("GEMINI_API_KEY", ""))) {
    providers <- c(providers, "gemini")
  }
  providers
}

#' Detect the default LLM provider
#'
#' @return Character string: \code{"openai"}, \code{"gemini"}, or \code{"none"}.
#'   When both keys are set, OpenAI is preferred.
#' @export
llm_provider <- function() {
  providers <- llm_providers()
  if (length(providers) == 0) return("none")
  providers[1]
}

#' Get the model name for a provider
#'
#' Uses the environment variable \code{OPENAI_MODEL} or \code{GEMINI_MODEL}
#' if set, otherwise falls back to built-in defaults.
#'
#' @param provider Character string: \code{"openai"} or \code{"gemini"}.
#'   Defaults to the active provider.
#' @return Character string with the model name.
#' @export
llm_model_name <- function(provider = llm_provider()) {
  defaults <- c(openai = "gpt-5.1", gemini = "gemini-2.5-flash")
  env_vars <- c(openai = "OPENAI_MODEL", gemini = "GEMINI_MODEL")

  if (!provider %in% names(defaults)) return("none")

  env_val <- Sys.getenv(env_vars[[provider]], "")
  if (nzchar(env_val)) env_val else defaults[[provider]]
}

#' Load the report writing guide
#'
#' @return Character string with the writing guide content.
#' @keywords internal
load_writing_guide <- function() {
  guide_path <- system.file("extdata", "report_writing_guide.md",
                            package = "algaware")
  if (!nzchar(guide_path) || !file.exists(guide_path)) {
    return("")
  }
  paste(readLines(guide_path, warn = FALSE), collapse = "\n")
}

#' Normalize phytoplankton groups for report text
#'
#' Standardises the group column name from either \code{phyto_group} or
#' \code{phyto_group.plankton_group} into \code{detailed_group} and
#' \code{text_group} (identical \u2014 all groups from the YAML config are
#' preserved in both columns).
#'
#' @param phyto_groups Data frame with columns \code{name}, \code{AphiaID},
#'   and either \code{phyto_group} or \code{phyto_group.plankton_group}.
#' @return Data frame with columns \code{name}, \code{AphiaID},
#'   \code{detailed_group}, and \code{text_group}.
#' @keywords internal
normalize_phyto_groups_for_text <- function(phyto_groups) {
  if (is.null(phyto_groups) || nrow(phyto_groups) == 0) {
    return(NULL)
  }

  group_col <- if ("phyto_group" %in% names(phyto_groups)) {
    "phyto_group"
  } else if ("phyto_group.plankton_group" %in% names(phyto_groups)) {
    "phyto_group.plankton_group"
  } else {
    return(NULL)
  }

  out <- phyto_groups[, intersect(c("name", "AphiaID"), names(phyto_groups)),
                      drop = FALSE]
  out$detailed_group <- as.character(phyto_groups[[group_col]])
  out$detailed_group[is.na(out$detailed_group)] <- "Other"
  out$text_group <- out$detailed_group
  unique(out)
}

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

#' Call the OpenAI API
#'
#' @param system_prompt System prompt string.
#' @param user_prompt User prompt string.
#' @param model OpenAI model name (default: "gpt-4.1").
#' @param temperature Sampling temperature (default: 0.3). Lower values
#'   produce more deterministic, factual output; higher values are more
#'   creative. 0.3 is a good balance for scientific report text.
#' @return Character string with the generated text.
#' @keywords internal
call_openai <- function(system_prompt, user_prompt,
                        model = llm_model_name("openai"),
                        temperature = 0.3) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required for LLM API calls. ",
         "Install it with: install.packages(\"httr2\")", call. = FALSE)
  }
  api_key <- Sys.getenv("OPENAI_API_KEY", "")
  if (!nzchar(api_key)) {
    stop("OPENAI_API_KEY environment variable is not set.", call. = FALSE)
  }

  body <- list(
    model = model,
    temperature = temperature,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = user_prompt)
    )
  )

  # 120s timeout because report text generation can be slow for long prompts.
  # Retries twice with 5s backoff to handle transient API errors.
  resp <- httr2::request("https://api.openai.com/v1/chat/completions") |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_timeout(120) |>
    httr2::req_retry(max_tries = 2, backoff = ~ 5) |>
    httr2::req_perform()

  result <- httr2::resp_body_json(resp)
  text <- result$choices[[1]]$message$content
  strip_markdown(text)
}

#' Call the Gemini API (OpenAI-compatible endpoint)
#'
#' Uses Google's OpenAI-compatible chat completions endpoint. Includes a
#' rate-limit delay to stay within the free tier (5 RPM for Pro, 15 RPM
#' for Flash). On 429 responses, waits and retries up to 3 times with
#' increasing backoff.
#'
#' @param system_prompt System prompt string.
#' @param user_prompt User prompt string.
#' @param model Gemini model name (default: "gemini-2.5-flash").
#' @param temperature Sampling temperature (default: 0.3).
#' @return Character string with the generated text.
#' @keywords internal
call_gemini <- function(system_prompt, user_prompt,
                        model = llm_model_name("gemini"),
                        temperature = 0.3) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required for LLM API calls. ",
         "Install it with: install.packages(\"httr2\")", call. = FALSE)
  }
  api_key <- Sys.getenv("GEMINI_API_KEY", "")
  if (!nzchar(api_key)) {
    stop("GEMINI_API_KEY environment variable is not set.", call. = FALSE)
  }

  body <- list(
    model = model,
    temperature = temperature,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = user_prompt)
    )
  )

  # Rate-limit delay: Gemini free tier allows 5 RPM for 2.5 Pro (one
  # request per 12s). Default 15s provides margin for variable response
  # times. Override with options(algaware.gemini_delay = <seconds>).
  gemini_delay <- getOption("algaware.gemini_delay", 15)
  last_call <- .llm_state$gemini_last_call
  if (!is.null(last_call)) {
    elapsed <- as.numeric(difftime(Sys.time(), last_call, units = "secs"))
    if (elapsed < gemini_delay) {
      Sys.sleep(gemini_delay - elapsed)
    }
  }

  .llm_state$gemini_last_call <- Sys.time()

  resp <- httr2::request("https://generativelanguage.googleapis.com/v1beta/openai/chat/completions") |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_timeout(180) |>
    httr2::req_retry(
      max_tries = 5,
      # 429 = rate-limited; 503 = service overload -- both are transient
      is_transient = \(resp) httr2::resp_status(resp) %in% c(429L, 503L),
      # Exponential backoff: 15 s, 30 s, 60 s, 120 s (capped)
      backoff = \(attempt) min(gemini_delay * 2^(attempt - 1L), 120)
    ) |>
    httr2::req_perform()

  .llm_state$gemini_last_call <- Sys.time()

  result <- httr2::resp_body_json(resp)
  text <- result$choices[[1]]$message$content
  strip_markdown(text)
}

# Package-level mutable state for rate limiting (environment, not exported)
.llm_state <- new.env(parent = emptyenv())
.llm_state$gemini_last_call <- NULL

#' Call an LLM provider
#'
#' Dispatches to \code{call_openai} or \code{call_gemini}. When
#' \code{provider} is NULL, auto-detects from available API keys.
#'
#' @param system_prompt System prompt string.
#' @param user_prompt User prompt string.
#' @param provider Character string: \code{"openai"} or \code{"gemini"}.
#'   NULL (default) auto-detects.
#' @param temperature Sampling temperature (default: 0.3).
#' @return Character string with the generated text.
#' @keywords internal
call_llm <- function(system_prompt, user_prompt, provider = NULL,
                     temperature = 0.3) {
  if (is.null(provider)) provider <- llm_provider()
  switch(provider,
    openai = call_openai(system_prompt, user_prompt,
                         temperature = temperature),
    gemini = call_gemini(system_prompt, user_prompt,
                         temperature = temperature),
    stop("No LLM API key configured. Set OPENAI_API_KEY or GEMINI_API_KEY.",
         call. = FALSE)
  )
}

#' Strip markdown formatting from LLM output
#'
#' Removes markdown bold/italic markers while preserving HAB asterisks
#' (asterisk directly after a word with no space).
#'
#' @param text Character string.
#' @return Cleaned character string.
#' @keywords internal
strip_markdown <- function(text) {
  # Remove **bold** markers
  text <- gsub("\\*\\*([^*]+)\\*\\*", "\\1", text)
  # Remove *italic* markers (asterisk-word-asterisk with no adjacent letter)
  # But preserve HAB markers like "species_name*" (no closing asterisk)
  text <- gsub("(?<![\\w])\\*([^*]+)\\*(?![\\w])", "\\1", text, perl = TRUE)
  # Remove any leading/trailing whitespace
  trimws(text)
}

#' Detect bloom conditions and return language-specific prompt instructions
#'
#' Checks whether cruise data meets the criteria for a spring bloom (West Coast,
#' Jan-Feb: diatom-dominated, chl > 3 ug/L) or a cyanobacterial bloom (Baltic,
#' Jun-Aug: cyanobacteria-dominated, chl > 3 ug/L) and returns a block of extra
#' instructions to append to the LLM prompt when either condition is met.
#'
#' @param station_summary Full station_summary data frame.
#' @param phyto_groups Optional phytoplankton group table.
#' @param lang \code{"en"} (English) or \code{"sv"} (Swedish).
#' @return Character string with bloom alert instructions, or \code{""}.
#' @keywords internal
bloom_alert_note <- function(station_summary, phyto_groups = NULL,
                             lang = "en") {
  if (is.null(station_summary) || nrow(station_summary) == 0) return("")

  # Extract cruise month from the earliest visit date available
  date_col <- if ("visit_date" %in% names(station_summary)) "visit_date"
              else if ("median_time" %in% names(station_summary)) "median_time"
              else NULL
  if (is.null(date_col)) return("")
  cruise_month <- as.integer(format(as.Date(station_summary[[date_col]][1]), "%m"))

  # Attach group labels (text_group: Diatoms / Dinoflagellates / Cyanobacteria / Other)
  ss <- attach_text_groups(station_summary, phyto_groups)

  alerts <- character(0)

  # Helper: TRUE if diatoms/cyano exceed `threshold` fraction of biovolume at
  # ANY single station (visit_id) in the supplied subset.
  any_station_dominant <- function(df, group, threshold = 0.5) {
    if (nrow(df) == 0 || !"visit_id" %in% names(df)) return(FALSE)
    any(vapply(unique(df$visit_id), function(vid) {
      rows <- df[df$visit_id == vid, , drop = FALSE]
      total <- sum(rows$biovolume_mm3_per_liter, na.rm = TRUE)
      if (total <= 0) return(FALSE)
      grp <- sum(rows$biovolume_mm3_per_liter[rows$text_group == group],
                 na.rm = TRUE)
      (grp / total) > threshold
    }, logical(1L)))
  }

  # --- Spring bloom: West Coast, January-February ---
  if (cruise_month %in% 1:2) {
    wc <- ss[!is.na(ss$COAST) & ss$COAST != "EAST", , drop = FALSE]
    if (nrow(wc) > 0) {
      high_chl       <- any(!is.na(wc$chl_mean) & wc$chl_mean > 3)
      diatom_dominant <- any_station_dominant(wc, "Diatoms", 0.5)
      if (high_chl && diatom_dominant) alerts <- c(alerts, "spring_bloom")
    }
  }

  # --- Cyanobacterial bloom: Baltic Sea, June-August ---
  if (cruise_month %in% 6:8) {
    bal <- ss[!is.na(ss$COAST) & ss$COAST == "EAST", , drop = FALSE]
    if (nrow(bal) > 0) {
      high_chl      <- any(!is.na(bal$chl_mean) & bal$chl_mean > 3)
      cyano_dominant <- any_station_dominant(bal, "Cyanobacteria", 0.5)
      if (high_chl && cyano_dominant) alerts <- c(alerts, "cyano_bloom")
    }
  }

  if (length(alerts) == 0) return("")

  notes <- character(0)
  if ("spring_bloom" %in% alerts) {
    notes <- c(notes, if (lang == "sv") {
      paste0("DATA INDIKERAR V\u00c5RBLOMNING p\u00e5 v\u00e4stkusten (kiselalger dominerar, ",
             "klorofyll > 3 \u00b5g/L). Framh\u00e4v explicit att en v\u00e5rblomning ",
             "p\u00e5g\u00e5r i stycket om v\u00e4stkusten.")
    } else {
      paste0("DATA INDICATES A SPRING BLOOM on the West Coast (diatoms dominant, ",
             "chlorophyll > 3 \u00b5g/L). Explicitly highlight that a spring bloom ",
             "is occurring in the West Coast paragraph.")
    })
  }
  if ("cyano_bloom" %in% alerts) {
    notes <- c(notes, if (lang == "sv") {
      paste0("DATA INDIKERAR CYANOBAKTERIABLOMNING i \u00d6stersj\u00f6n (cyanobakterier dominerar, ",
             "klorofyll > 3 \u00b5g/L). Framh\u00e4v explicit att en cyanobakteriablomning ",
             "p\u00e5g\u00e5r i stycket om \u00d6stersj\u00f6n.")
    } else {
      paste0("DATA INDICATES A CYANOBACTERIAL BLOOM in the Baltic Sea (cyanobacteria dominant, ",
             "chlorophyll > 3 \u00b5g/L). Explicitly highlight that a cyanobacterial bloom ",
             "is occurring in the Baltic Sea paragraph.")
    })
  }

  paste0("\n\nBLOOM ALERTS -- MUST BE MENTIONED:\n",
         paste(notes, collapse = "\n"))
}

#' Generate the Swedish summary text
#'
#' @param station_summary Full station_summary data frame.
#' @param taxa_lookup Optional taxa lookup table.
#' @param cruise_info Cruise info string.
#' @param phyto_groups Optional phytoplankton group table used to provide
#'   explicit group assignments in the prompt text.
#' @param provider LLM provider (\code{"openai"} or \code{"gemini"}).
#'   NULL auto-detects.
#' @param unclassified_fractions Optional per-sample fractions of unclassified detections supplied to the prompt.
#' @return Character string with Swedish summary.
#' @export
generate_swedish_summary <- function(station_summary, taxa_lookup = NULL,
                                     cruise_info = "", phyto_groups = NULL,
                                     provider = NULL,
                                     unclassified_fractions = NULL) {
  guide <- load_writing_guide()
  cruise_data <- format_cruise_summary_for_prompt(station_summary, taxa_lookup,
                                                  unclassified_fractions = unclassified_fractions,
                                                  phyto_groups = phyto_groups)
  bloom_note <- bloom_alert_note(station_summary, phyto_groups, lang = "sv")

  system_prompt <- paste0(
    "You are a marine biologist writing phytoplankton monitoring reports ",
    "for the Swedish AlgAware programme (SMHI). You write in Swedish. ",
    "Follow the writing guide exactly.\n\n",
    "WRITING GUIDE:\n", guide
  )

  user_prompt <- paste0(
    "Write the Swedish summary (Sammanfattning) for this AlgAware cruise report.\n\n",
    "Cruise: ", cruise_info, "\n\n",
    "Station data overview:\n", cruise_data, "\n\n",
    "Write the summary in Swedish, using one paragraph per region ",
    "(West Coast and Baltic Sea), clearly separated. Follow the style described in the ",
    "writing guide. Mark potentially harmful taxa (potentiellt skadliga taxa) ",
    "with an asterisk (*). Never use the term 'HAB species' or 'HAB-arter'; ",
    "say 'potentiellt skadligt taxon' (singular) or 'potentiellt skadliga taxa' ",
    "(plural) instead. Use 'potentiellt skadlig art' only when referring to a ",
    "specific species. ",
    "Use correct Swedish terminology: 'klorofyll' (not 'chlorophyll'), ",
    "'klorofyllfluorescens' (not 'chlorophyll fluorescence'), ",
    "'biovolym' (not 'biovolume'), ",
    "'kiselalger' (not 'diatom\u00e9er' or 'diatomeer') for diatoms, ",
    "'v\u00e4xtplankton' (not 'fytoplankton'), ",
    "'expedition' (not 'kryssningen'), ",
    "'kryptomonader' (not 'kryptofyter'). ",
    "Use the provided phytoplankton group assignments in the prompt data. ",
    "Do not infer group membership from scientific names yourself. ",
    "Om ett potentiellt skadligt taxon dominerar biovolymen vid en station, ",
    "beskriv det som dominant \u2014 tona inte ned dess f\u00f6rekomst enbart p\u00e5 grund av att det \u00e4r potentiellt skadligt. ",
    "Compare klorofyllfluorescens between stations and relate it to the ",
    "IFCB biovolume data where relevant. ",
    "If any station data lines contain 'WARNING EXCEEDED', you MUST explicitly ",
    "state in the summary that the abundance of the named taxon exceeds the ",
    "recommended warning level (varningsniv\u00e5) at that station, and include ",
    "the actual abundance in cells/L and the threshold value. ",
    "Output ONLY the summary text, no headings. ",
    "Output plain text only -- no markdown formatting whatsoever. ",
    "The only asterisk allowed is the harmful taxon marker placed after the FULL taxon mention. ",
    "Place it after 'spp.' or 'sp.' when present (e.g. 'Pseudochattonella spp.*', ",
    "NOT 'Pseudochattonella* spp.*'). Never place an asterisk after the genus name alone ",
    "when 'spp.' or 'sp.' follows.",
    bloom_note
  )

  call_llm(system_prompt, user_prompt, provider = provider)
}

#' Translate an English summary to Swedish
#'
#' @param english_text Character string with the English summary to translate.
#' @param provider LLM provider (\code{"openai"} or \code{"gemini"}).
#'   NULL auto-detects.
#' @return Character string with Swedish translation.
#' @export
translate_summary_to_swedish <- function(english_text, provider = NULL) {
  system_prompt <- paste0(
    "You are a translator specializing in marine biology reports for the Swedish ",
    "AlgAware programme (SMHI). Translate scientific phytoplankton monitoring text ",
    "from English to Swedish accurately and completely."
  )

  user_prompt <- paste0(
    "Translate the following English phytoplankton monitoring summary to Swedish. ",
    "Rules:\n",
    "- Translate every sentence; do not omit or condense any content.\n",
    "- Use correct Swedish scientific terminology: ",
    "'klorofyllfluorescens' (not 'chlorophyll fluorescence'), ",
    "'biovolym' (not 'biovolume'), ",
    "'kiselalger' (not 'diatom\u00e9er' or 'diatomeer'), ",
    "'v\u00e4xtplankton' (not 'fytoplankton'), ",
    "'expedition' (not 'kryssningen'), ",
    "'kryptomonader' (not 'kryptofyter'), ",
    "'klorofyll' (not 'chlorophyll').\n",
    "- Keep all species names in Latin (italics not needed in plain text).\n",
    "- Keep the asterisk (*) immediately after any species name that has one ",
    "(it marks potentially harmful taxa).\n",
    "- Output plain text only -- no markdown, no headings, no extra commentary.\n\n",
    "English text to translate:\n\n",
    english_text
  )

  call_llm(system_prompt, user_prompt, provider = provider)
}

#' Generate the English summary text
#'
#' @param station_summary Full station_summary data frame.
#' @param taxa_lookup Optional taxa lookup table.
#' @param cruise_info Cruise info string.
#' @param phyto_groups Optional phytoplankton group table used to provide
#'   explicit group assignments in the prompt text.
#' @param provider LLM provider (\code{"openai"} or \code{"gemini"}).
#'   NULL auto-detects.
#' @param unclassified_fractions Optional per-sample unclassified percentages
#'   for contextualizing the summary.
#' @return Character string with English summary.
#' @export
generate_english_summary <- function(station_summary, taxa_lookup = NULL,
                                     cruise_info = "", phyto_groups = NULL,
                                     provider = NULL,
                                     unclassified_fractions = NULL) {
  guide <- load_writing_guide()
  cruise_data <- format_cruise_summary_for_prompt(station_summary, taxa_lookup,
                                                  unclassified_fractions = unclassified_fractions,
                                                  phyto_groups = phyto_groups)
  bloom_note <- bloom_alert_note(station_summary, phyto_groups, lang = "en")

  system_prompt <- paste0(
    "You are a marine biologist writing phytoplankton monitoring reports ",
    "for the Swedish AlgAware programme (SMHI). You write in English. ",
    "Follow the writing guide exactly.\n\n",
    "WRITING GUIDE:\n", guide
  )

  user_prompt <- paste0(
    "Write the English summary (Abstract) for this AlgAware cruise report.\n\n",
    "Cruise: ", cruise_info, "\n\n",
    "Station data overview:\n", cruise_data, "\n\n",
    "Write in English. Follow the writing guide exactly, including the ",
    "harmful taxa terminology section. ",
    "Use the provided phytoplankton group assignments in the prompt data. ",
    "Do not infer group membership from scientific names yourself. ",
    "If a potentially harmful taxon dominates by biovolume at a station, name it as ",
    "dominant in the community description \u2014 do not downplay its abundance simply because ",
    "it is potentially harmful. ",
    "If any station data lines contain 'WARNING EXCEEDED', you MUST explicitly ",
    "state in the summary that the abundance of the named taxon exceeds the ",
    "recommended warning level at that station, and include the actual abundance ",
    "in cells/L and the threshold value. ",
    "Output ONLY the summary text, no headings or extra commentary. ",
    "Output plain text only -- no markdown formatting whatsoever. ",
    "The only asterisk allowed is the harmful taxon marker placed after the FULL taxon mention. ",
    "Place it after 'spp.' or 'sp.' when present (e.g. 'Pseudochattonella spp.*', ",
    "NOT 'Pseudochattonella* spp.*'). Never place an asterisk after the genus name alone ",
    "when 'spp.' or 'sp.' follows.",
    bloom_note
  )

  call_llm(system_prompt, user_prompt, provider = provider)
}

#' Generate a station description
#'
#' @param station_data Data frame with station_summary rows for one visit.
#' @param taxa_lookup Optional taxa lookup table.
#' @param all_stations_summary Optional full station_summary for context.
#' @param phyto_groups Optional phytoplankton group table used to provide
#'   explicit group assignments in the prompt text.
#' @param provider LLM provider (\code{"openai"} or \code{"gemini"}).
#'   NULL auto-detects.
#' @param unclassified_pct Optional per-class unclassified percentage info used for context.
#' @return Character string with station description in English.
#' @export
generate_station_description <- function(station_data, taxa_lookup = NULL,
                                         all_stations_summary = NULL,
                                         phyto_groups = NULL,
                                         provider = NULL,
                                         unclassified_pct = NULL) {
  guide <- load_writing_guide()
  station_text <- format_station_data_for_prompt(station_data, taxa_lookup,
                                                 unclassified_pct = unclassified_pct,
                                                 phyto_groups = phyto_groups)

  # Provide cruise-wide context so the LLM can make relative statements
  context <- ""
  if (!is.null(all_stations_summary)) {
    all_visits <- unique(all_stations_summary[, c("visit_id",
                                                   "STATION_NAME_SHORT",
                                                   "COAST")])
    context_lines <- vapply(seq_len(nrow(all_visits)), function(i) {
      vid <- all_visits$visit_id[i]
      d <- all_stations_summary[all_stations_summary$visit_id == vid, ]
      n_taxa <- nrow(d)
      total_counts <- sum(d$counts_per_liter, na.rm = TRUE)
      total_bv <- sum(d$biovolume_mm3_per_liter, na.rm = TRUE)
      region <- if (all_visits$COAST[i] == "EAST") "Baltic" else "West"
      current <- if (vid == unique(station_data$visit_id)[1]) " <-- this station" else ""
      sprintf("  %s (%s): %d taxa, %.0f counts/L, %.4f mm3/L biovolume%s",
              all_visits$STATION_NAME_SHORT[i], region,
              n_taxa, total_counts, total_bv, current)
    }, character(1))
    context <- paste0(
      "\n\nCruise context -- compare this station relative to the others:\n",
      paste(context_lines, collapse = "\n")
    )
  }

  system_prompt <- paste0(
    "You are a marine biologist writing phytoplankton monitoring reports ",
    "for the Swedish AlgAware programme (SMHI). You write in English. ",
    "Follow the writing guide exactly.\n\n",
    "WRITING GUIDE:\n", guide
  )

  user_prompt <- paste0(
    "Write a station description for the following station visit.\n\n",
    station_text,
    context,
    "\n\nWrite in English. Follow the station description pattern and ",
    "harmful taxa terminology in the writing guide exactly. ",
    "Base your characterization of diversity (high/moderate/low) and abundance ",
    "(high/moderate/low) on the cruise context data above -- describe this station ",
    "relative to the other stations visited during this cruise. ",
    "Use the provided phytoplankton group assignments in the prompt data. ",
    "Do not infer group membership from scientific names yourself. ",
    "Use the group names consistently throughout: if a taxon belongs to Silicoflagellates, ",
    "call it Silicoflagellates everywhere \u2014 never refer to it as 'Others' in one sentence ",
    "and Silicoflagellates in another. ",
    "Provide a detailed species breakdown only for the top 3 groups listed in the prompt. ",
    "Mention any remaining groups in a single brief sentence. ",
    "Taxa marked [HAB] in the data are potentially harmful and must be mentioned. ",
    "If a [HAB] taxon ranks among the dominant taxa by biovolume, describe it as dominant ",
    "in the community description \u2014 do not omit it from the dominance narrative just because ",
    "it also appears in the potentially harmful taxa section. ",
    "If the station data contains an IMPORTANT section about taxa exceeding warning ",
    "levels, you MUST state in the description the actual abundance in cells/L and ",
    "the threshold value (e.g. '2 000 cells/L, exceeding the warning level of ",
    "1 500 cells/L'). ",
    "Output ONLY the description text, no headings or station name. ",
    "Output plain text only -- no markdown formatting whatsoever. ",
    "The only asterisk allowed is the harmful taxon marker placed after the FULL taxon mention. ",
    "Place it after 'spp.' or 'sp.' when present (e.g. 'Pseudochattonella spp.*', ",
    "NOT 'Pseudochattonella* spp.*'). Never place an asterisk after the genus name alone ",
    "when 'spp.' or 'sp.' follows."
  )

  call_llm(system_prompt, user_prompt, provider = provider)
}
