#' Save a ggplot to a temp file and add centered to document
#'
#' @param doc An rdocx object.
#' @param plot A ggplot object.
#' @param cleanup Environment with a \code{files} character vector.
#' @param width,height Plot dimensions in inches for ggsave.
#' @param display_width,display_height Display dimensions in the document.
#' @return The modified rdocx object.
#' @keywords internal
add_centered_plot <- function(doc, plot, cleanup,
                              width, height, display_width, display_height) {
  f <- tempfile(fileext = ".png")
  ggplot2::ggsave(f, plot, width = width, height = height, dpi = 300)
  cleanup$files <- c(cleanup$files, f)
  officer::body_add_fpar(doc, officer::fpar(
    officer::external_img(f, width = display_width, height = display_height),
    fp_p = officer::fp_par(text.align = "center")
  ))
}

#' Save a ggplot to a temp file and add to document
#'
#' @param doc An rdocx object.
#' @param plot A ggplot object.
#' @param cleanup Environment with a \code{files} character vector for
#'   tracking temp files.
#' @param width,height Plot dimensions in inches for ggsave.
#' @param display_width,display_height Display dimensions in the document.
#' @return The modified rdocx object.
#' @keywords internal
add_plot_to_doc <- function(doc, plot, cleanup,
                            width, height, display_width, display_height) {
  f <- tempfile(fileext = ".png")
  ggplot2::ggsave(f, plot, width = width, height = height, dpi = 300)
  cleanup$files <- c(cleanup$files, f)
  officer::body_add_img(doc, f, width = display_width, height = display_height)
}

#' Build a named vector of sample counts per station_date
#'
#' @param station_summary Aggregated station data with \code{STATION_NAME_SHORT},
#'   \code{visit_date}, and \code{n_samples} columns.
#' @return Named integer vector mapping station_date strings to sample counts,
#'   or NULL if \code{n_samples} is not available.
#' @keywords internal
build_sample_counts <- function(station_summary) {
  if (!"n_samples" %in% names(station_summary)) return(NULL)
  visits <- unique(station_summary[, c("STATION_NAME_SHORT", "visit_date",
                                        "n_samples")])
  keys <- paste(visits$STATION_NAME_SHORT, visits$visit_date, sep = "_")
  counts <- as.integer(visits$n_samples)
  stats::setNames(counts, keys)
}

#' Add a heatmap section to the report (own page)
#' @keywords internal
add_heatmap_section <- function(doc, wide_data, taxa_lookup, title,
                                fig_num, cleanup, sample_counts = NULL) {
  if (nrow(wide_data) > 0 && ncol(wide_data) > 1) {
    doc <- officer::body_add_break(doc)
    doc <- officer::body_add_par(doc, paste(title, "- Biovolume heatmap"),
                                 style = "heading 2")
    hm <- create_heatmap(wide_data, taxa_lookup = taxa_lookup,
                          sample_counts = sample_counts)
    hm_file <- tempfile(fileext = ".png")
    hm_height <- max(4, min(12, nrow(wide_data) * 0.25 + 2))
    ggplot2::ggsave(hm_file, hm, width = 8, height = hm_height, dpi = 300)
    cleanup$files <- c(cleanup$files, hm_file)
    display_h <- hm_height * 6 / 8
    doc <- officer::body_add_fpar(doc, officer::fpar(
      officer::external_img(hm_file, width = 6, height = display_h),
      fp_p = officer::fp_par(text.align = "center")
    ))
    doc <- officer::body_add_fpar(doc, officer::fpar(
      officer::ftext(
        paste0("Figure ", fig_num,
               ". Biovolume heatmap for ", title, " stations."),
        officer::fp_text(font.size = 10, font.family = "Adobe Garamond Pro")
      ), fp_p = officer::fp_par(text.align = "left")
    ))
    doc <- officer::body_add_par(doc, "")
    fig_num <- fig_num + 1L
  }
  list(doc = doc, fig_num = fig_num)
}

#' Add a stacked bar chart to the report (no page break, added by caller)
#' @keywords internal
add_stacked_bar_section <- function(doc, wide_data, taxa_lookup, title,
                                    fig_num, cleanup) {
  if (nrow(wide_data) > 0 && ncol(wide_data) > 1) {
    sb <- create_stacked_bar(wide_data, taxa_lookup = taxa_lookup)
    sb_file <- tempfile(fileext = ".png")
    ggplot2::ggsave(sb_file, sb, width = 8, height = 5, dpi = 300)
    cleanup$files <- c(cleanup$files, sb_file)
    doc <- officer::body_add_fpar(doc, officer::fpar(
      officer::external_img(sb_file, width = 6, height = 3.75),
      fp_p = officer::fp_par(text.align = "center")
    ))
    doc <- officer::body_add_fpar(doc, officer::fpar(
      officer::ftext(
        paste0("Figure ", fig_num,
               ". Relative biovolume of top 10 taxa at ", title, " stations."),
        officer::fp_text(font.size = 10, font.family = "Adobe Garamond Pro")
      ), fp_p = officer::fp_par(text.align = "left")
    ))
    doc <- officer::body_add_par(doc, "")
    fig_num <- fig_num + 1L
  }
  list(doc = doc, fig_num = fig_num)
}

#' Add station report sections to document
#' @keywords internal
add_station_sections <- function(doc, station_summary,
                                 taxa_lookup = NULL, use_llm = FALSE,
                                 phyto_groups = NULL,
                                 llm_provider = NULL,
                                 on_llm_progress = NULL,
                                 unclassified_fractions = NULL) {
  doc <- officer::body_add_break(doc)
  doc <- officer::body_add_par(doc, "Station reports", style = "heading 2")

  visits <- unique(station_summary[, c("STATION_NAME", "STATION_NAME_SHORT",
                                       "COAST", "visit_date", "visit_id")])
  visits <- visits[order(visits$COAST, visits$visit_date,
                         visits$STATION_NAME), ]

  current_region <- ""
  for (i in seq_len(nrow(visits))) {
    region <- ifelse(visits$COAST[i] == "EAST", "Baltic Sea", "West Coast")
    if (region != current_region) {
      doc <- officer::body_add_par(doc, region, style = "heading 3")
      current_region <- region
    }

    station_header <- paste0(visits$STATION_NAME_SHORT[i], " - ",
                             visits$visit_date[i])
    doc <- officer::body_add_par(doc, station_header, style = "heading 3")

    description <- "[Write station description here.]"
    if (use_llm) {
      if (is.function(on_llm_progress)) {
        on_llm_progress(visits$STATION_NAME_SHORT[i])
      }
      station_data <- station_summary[
        station_summary$visit_id == visits$visit_id[i], ]
      visit_unclass_pct <- unclassified_fractions[[visits$visit_id[i]]]
      description <- tryCatch(
        generate_station_description(station_data, taxa_lookup,
                                     station_summary,
                                     phyto_groups = phyto_groups,
                                     provider = llm_provider,
                                     unclassified_pct = visit_unclass_pct),
        error = function(e) {
          warning("LLM station description failed for ",
                  visits$STATION_NAME_SHORT[i], ": ", e$message,
                  call. = FALSE)
          "[Write station description here. (LLM generation failed)]"
        }
      )
    }

    doc <- add_formatted_par(doc, description, taxa_lookup, style = "Normal")
    doc <- officer::body_add_par(doc, "")
  }
  doc
}

#' Add mosaic section to the report
#' @keywords internal
add_mosaic_section <- function(doc, mosaics, hab_species, region_label,
                               cleanup, taxa_lookup = NULL) {
  if (length(mosaics) == 0) return(doc)

  doc <- officer::body_add_break(doc)
  doc <- officer::body_add_par(doc, "Image mosaics", style = "heading 2")
  doc <- officer::body_add_par(doc, region_label, style = "heading 3")
  mosaic_num <- 1L

  for (taxon in names(mosaics)) {
    mosaic_file <- tempfile(fileext = ".png")
    magick::image_write(mosaics[[taxon]], mosaic_file)
    cleanup$files <- c(cleanup$files, mosaic_file)
    info <- magick::image_info(mosaics[[taxon]])
    display_width <- min(6, info$width / 300)
    display_height <- display_width * info$height / info$width
    if (display_height > 5) {
      display_height <- 5
      display_width <- display_height * info$width / info$height
    }
    hab_note <- if (taxon %in% hab_species) " *" else ""
    doc <- officer::body_add_par(doc, paste0(taxon, hab_note),
                                 style = "heading 3")
    doc <- officer::body_add_fpar(doc, officer::fpar(
      officer::external_img(mosaic_file, width = display_width,
                            height = display_height),
      fp_p = officer::fp_par(text.align = "center")
    ))
    cap_prop <- officer::fp_text(font.size = 10,
                                 font.family = "Adobe Garamond Pro")
    cap_italic <- officer::fp_text(font.size = 10, italic = TRUE,
                                   font.family = "Adobe Garamond Pro")
    taxon_ftexts <- make_taxon_ftexts(taxon, taxa_lookup, cap_prop, cap_italic)
    hab_ftext <- if (taxon %in% hab_species) {
      list(officer::ftext(" * Potentially harmful taxon.", cap_prop))
    } else {
      list()
    }
    doc <- officer::body_add_fpar(doc, do.call(officer::fpar, c(
      list(officer::ftext(paste0("Mosaic ", mosaic_num,
                                 ". Example images of "), cap_prop)),
      taxon_ftexts,
      list(officer::ftext(paste0(" from ", region_label,
                                 " stations. Black bars represent 5 \u00b5m."),
                          cap_prop)),
      hab_ftext,
      list(fp_p = officer::fp_par(text.align = "center"))
    )))
    doc <- officer::body_add_par(doc, "")
    mosaic_num <- mosaic_num + 1L
  }

  doc
}

#' Add CTD profile and time series figures to the report
#' @keywords internal
add_ctd_report_section <- function(doc, ctd_data_full, lims_data_full,
                                   chl_stats, fig_num, cleanup) {
  standard_stations <- load_standard_stations()

  dates <- ctd_data_full$sample_date[!is.na(ctd_data_full$sample_date)]
  current_year <- if (length(dates) > 0) {
    as.integer(format(max(dates), "%Y"))
  } else {
    as.integer(format(Sys.Date(), "%Y"))
  }

  left_pp <- officer::fp_par(text.align = "left")
  has_lims <- !is.null(lims_data_full) && nrow(lims_data_full) > 0

  # Regions in YAML order
  yaml_regions <- unique(standard_stations$region)
  present_regions <- unique(ctd_data_full$region)
  regions <- c(yaml_regions[yaml_regions %in% present_regions],
               setdiff(present_regions, yaml_regions))

  for (region in regions) {
    region_ctd <- ctd_data_full[ctd_data_full$region == region, ]
    if (nrow(region_ctd) == 0) next

    fig <- create_ctd_region_figure(
      ctd_data_full     = ctd_data_full,
      lims_data_full    = lims_data_full,
      chl_stats         = chl_stats,
      standard_stations = standard_stations,
      region            = region,
      current_year      = current_year,
      force_two_columns = TRUE   # keep profile width consistent
    )
    if (is.null(fig)) next

    # Page break before each region figure; no Word heading (title is in plot)
    doc <- officer::body_add_break(doc)

    n_stations <- length(unique(region_ctd$canonical_name))
    fig_height <- max(4, min(16, n_stations * 2.5 + 1))
    fig_width  <- 11  # always full width (profiles in col 1/4, ts in 3/4)

    fig_file <- tempfile(fileext = ".png")
    ggplot2::ggsave(fig_file, fig, width = fig_width,
                    height = fig_height, dpi = 300)
    cleanup$files <- c(cleanup$files, fig_file)

    display_w <- min(6.5, fig_width * 0.65)
    display_h <- display_w * fig_height / fig_width

    doc <- officer::body_add_fpar(doc, officer::fpar(
      officer::external_img(fig_file, width = display_w,
                            height = display_h),
      fp_p = officer::fp_par(text.align = "center")
    ))

    caption_detail <- if (has_lims) {
      " and Chl-a time series (with 1991-2020 monthly statistics)"
    } else {
      ""
    }

    doc <- officer::body_add_fpar(doc, officer::fpar(
      officer::ftext(
        paste0("Figure ", fig_num,
               ". CTD fluorescence profiles", caption_detail,
               " for ", region, " stations."),
        officer::fp_text(font.size = 10, font.family = "Adobe Garamond Pro")
      ), fp_p = left_pp
    ))
    doc <- officer::body_add_par(doc, "")
    fig_num <- fig_num + 1L
  }

  list(doc = doc, fig_num = fig_num)
}
