#' Generate the AlgAware Word report
#'
#' Creates a Word document with overview maps, heatmaps, stacked bar charts,
#' station sections, and image mosaics. Optionally includes a front page with
#' logo, diary number placeholder, a phytoplankton pie map, and two summary
#' mosaics.
#'
#' @param output_path Path for the output .docx file.
#' @param station_summary Aggregated station data.
#' @param baltic_wide Wide-format Baltic summary.
#' @param westcoast_wide Wide-format West Coast summary.
#' @param baltic_mosaics Named list of mosaic images (Baltic).
#' @param westcoast_mosaics Named list of mosaic images (West Coast).
#' @param taxa_lookup Optional taxa lookup table with \code{HAB} column.
#' @param cruise_info Character string with cruise/date information for title.
#' @param classifier_name Optional character string with the classifier model
#'   name used for automated classification.
#' @param use_llm Logical; if TRUE and an LLM API key is configured
#'   (OPENAI_API_KEY or GEMINI_API_KEY), generate report text using an
#'   LLM. Default FALSE uses placeholder text.
#' @param annotator Character string with the analyst name for the
#'   introduction statement.
#' @param image_counts Optional data frame from \code{fetch_image_counts()}
#'   with cruise-wide image counts and coordinates for the track map.
#' @param total_bio_images Optional integer; total number of biological images
#'   used in the report (excluding non-biological classes).
#' @param llm_model Optional character string; name of the LLM model used for
#'   text generation (e.g. \code{"gpt-4.1"}).
#' @param n_station_samples Optional integer; total number of IFCB samples
#'   matched to AlgAware stations.
#' @param llm_provider Optional character string; LLM provider to use
#'   (\code{"openai"} or \code{"gemini"}). NULL auto-detects.
#' @param on_llm_progress Optional callback function called before each LLM
#'   request with arguments \code{(step, total, detail)} for progress
#'   reporting.
#' @param frontpage_baltic_mosaic Optional magick image for the front page
#'   Baltic Sea mosaic. If NULL (together with West Coast), no front page
#'   is generated.
#' @param frontpage_westcoast_mosaic Optional magick image for the front page
#'   West Coast mosaic.
#' @param frontpage_baltic_taxa Optional character vector of taxa names
#'   matching the numbered images in the Baltic mosaic (for captions).
#' @param frontpage_westcoast_taxa Optional character vector of taxa names
#'   matching the numbered images in the West Coast mosaic.
#' @param report_number Optional report issue number (e.g. \code{"1"}).
#' @param report_dnr Optional diarienummer string for the front page
#'   (e.g. \code{"2026-1234"}).
#' @param unclassified_fractions Optional named list or data frame of
#'   unclassified proportion estimates passed to the LLM prompts for context.
#' @param ctd_data Optional CTD profile data from \code{read_cnv_folder()}
#'   (AlgAware-matched, used for the chlorophyll map).
#' @param ctd_data_full Optional full CTD profile data from
#'   \code{read_cnv_folder_all()} (all standard stations, used for the CTD
#'   section with regional fluorescence profiles and time series).
#' @param lims_data Optional LIMS discrete Chl-a data from
#'   \code{read_lims_data()} (AlgAware-matched, used for the chlorophyll map).
#' @param lims_data_full Optional LIMS data from \code{read_lims_data_all()}
#'   (all standard stations, used for the CTD section time series).
#' @param chl_stats Optional historical statistics from
#'   \code{load_chl_statistics()}.
#' @param chl_map_source Character; active chlorophyll map source:
#'   \code{"ferrybox"}, \code{"ctd"}, \code{"lims"} (bottle, 0-20 m), or
#'   \code{"lims_hose"} (hose integrated, 0-10 m).
#' @param phyto_groups Optional data frame with columns \code{name},
#'   \code{AphiaID}, and \code{phyto_group} (typically from
#'   \code{SHARK4R::assign_phytoplankton_group()}). Used to render the
#'   per-station phytoplankton group composition pie map. If \code{NULL},
#'   the assignments are computed on demand via SHARK4R when available.
#' @return Invisible path to the created document.
#' @export
generate_report <- function(output_path, station_summary,
                            baltic_wide, westcoast_wide,
                            baltic_mosaics = list(),
                            westcoast_mosaics = list(),
                            taxa_lookup = NULL,
                            cruise_info = "",
                            classifier_name = NULL,
                            use_llm = FALSE,
                            annotator = "",
                            image_counts = NULL,
                            total_bio_images = NULL,
                            llm_model = NULL,
                            n_station_samples = NULL,
                            llm_provider = NULL,
                            on_llm_progress = NULL,
                            frontpage_baltic_mosaic = NULL,
                            frontpage_westcoast_mosaic = NULL,
                            unclassified_fractions = NULL,
                            frontpage_baltic_taxa = NULL,
                            frontpage_westcoast_taxa = NULL,
                            report_number = NULL,
                            report_dnr = NULL,
                            ctd_data = NULL,
                            ctd_data_full = NULL,
                            lims_data = NULL,
                            lims_data_full = NULL,
                            chl_stats = NULL,
                            chl_map_source = "ferrybox",
                            phyto_groups = NULL) {
  template <- system.file("templates", "report_template.docx",
                          package = "algaware")
  if (!nzchar(template)) {
    stop("Report template not found. Reinstall the algaware package.",
         call. = FALSE)
  }

  # Enrich station_summary with the active chlorophyll source so LLM prompts
  # receive the same data used for the chl map and report figures.
  # FerryBox chl is already merged upstream; for CTD/LIMS we replace it here.
  chl_avg <- switch(chl_map_source,
    ctd       = if (!is.null(ctd_data))  compute_ctd_chl_avg(ctd_data)   else NULL,
    lims      = if (!is.null(lims_data)) compute_lims_chl_avg(lims_data)  else NULL,
    lims_hose = if (!is.null(lims_data)) compute_lims_hose_avg(lims_data) else NULL,
    NULL  # ferrybox: already present in station_summary
  )
  if (!is.null(chl_avg) && nrow(chl_avg) > 0) {
    station_summary$chl_mean <- NULL
    station_summary <- merge(
      station_summary,
      chl_avg[, c("station_short", "chl_mean")],
      by.x = "STATION_NAME_SHORT", by.y = "station_short",
      all.x = TRUE
    )
  }

  # Track temp files for cleanup via environment (survives object copies)
  cleanup <- new.env(parent = emptyenv())
  cleanup$files <- character(0)
  on.exit(unlink(cleanup$files), add = TRUE)

  # Paragraph properties reused for caption alignment
  center_pp <- officer::fp_par(text.align = "center")
  left_pp <- officer::fp_par(text.align = "left")

  # Compute phytoplankton group assignments on demand if the caller didn't
  # provide them, so the same pie map can be reused on the front page.
  if (is.null(phyto_groups) &&
      requireNamespace("SHARK4R", quietly = TRUE) &&
      !is.null(station_summary) && nrow(station_summary) > 0) {
    taxa <- unique(station_summary[, c("name", "AphiaID")])
    taxa <- taxa[!is.na(taxa$name), ]
    if (nrow(taxa) > 0) {
      groups <- tryCatch(
        assign_phyto_groups(
          scientific_names = taxa$name,
          aphia_ids        = taxa$AphiaID
        ),
        error = function(e) NULL
      )
      if (!is.null(groups)) {
        phyto_groups <- data.frame(
          name        = taxa$name,
          AphiaID     = taxa$AphiaID,
          phyto_group = groups,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  group_map_plot <- NULL
  if (!is.null(phyto_groups) && nrow(phyto_groups) > 0) {
    group_map_plot <- tryCatch(
      create_group_map(station_summary, phyto_groups),
      error = function(e) NULL
    )
  }

  doc <- officer::read_docx(template)

  # The cover page is always generated. The separate mosaic overview page is
  # optional and depends on whether overview mosaics are available.
  has_cover_page <- TRUE
  has_mosaic_overview <- !is.null(frontpage_baltic_mosaic) ||
    !is.null(frontpage_westcoast_mosaic)

  doc <- add_front_page(doc, cleanup,
                        taxa_lookup = taxa_lookup,
                        cruise_info = cruise_info,
                        report_number = report_number,
                        report_dnr = report_dnr,
                        image_counts = image_counts,
                        group_map_plot = group_map_plot)

  # End front page section (no header/footer, no page number)
  doc <- officer::body_end_block_section(doc, officer::block_section(
    officer::prop_section(type = "nextPage")
  ))

  # Introduction
  doc <- officer::body_add_par(doc, "IFCB report overview", style = "heading 2")
  intro <- paste0(
    "This report is based on images collected by an Imaging FlowCytobot ",
    "(IFCB) onboard R/V Svea. Samples are collected continuously ",
    "approximately every 25 minutes from surface water at the ship\u2019s ",
    "water intake for the FerryBox system. The water passes through a ",
    "150 \u00b5m mesh filter before reaching the IFCB, which generally ",
    "excludes larger organisms. The instrument ",
    "triggers image capture on chlorophyll fluorescence, which means that ",
    "heterotrophic organisms and other non-chlorophyll-containing cells ",
    "may be underrepresented. In this report we summarize the findings ",
    "from selected monitoring stations. ",
    paste0(c(
      "Phytoplankton identification is based on ",
      "automated image classification by an AI model, validated by ",
      if (nzchar(annotator)) paste0(annotator, ", SMHI") else "SMHI",
      ".",
      if (use_llm) " Report text was drafted with the assistance of a large language model." else NULL
    ), collapse = "")
  )
  has_hab <- !is.null(taxa_lookup) &&
    "HAB" %in% names(taxa_lookup) &&
    any(taxa_lookup$HAB == TRUE, na.rm = TRUE)

  normal_prop <- officer::fp_text(font.size = 11,
                                  font.family = "Adobe Garamond Pro")
  red_prop    <- officer::fp_text(font.size = 11, color = "red",
                                  bold = TRUE,
                                  font.family = "Adobe Garamond Pro")
  intro_runs <- list(officer::ftext(intro, normal_prop))
  if (has_hab) {
    intro_runs <- c(intro_runs, list(
      officer::ftext(" Taxa marked with ", normal_prop),
      officer::ftext("*", red_prop),
      officer::ftext(" are potentially toxic or harmful.", normal_prop)
    ))
  }
  doc <- officer::body_add_fpar(doc,
    do.call(officer::fpar, intro_runs),
    style = "Normal"
  )

  doc <- officer::body_add_break(doc)

  # Count total LLM steps for progress reporting
  n_stations <- length(unique(station_summary$visit_id))
  llm_total <- 2L + n_stations  # Swedish + English + per-station
  llm_step <- 0L
  report_progress <- function(detail) {
    llm_step <<- llm_step + 1L
    if (is.function(on_llm_progress)) {
      on_llm_progress(llm_step, llm_total, detail)
    }
  }

  # English summary (generated first; Swedish is a translation of this)
  english_text <- "[Write English summary here.]"
  if (use_llm) {
    report_progress("English summary")
    english_text <- tryCatch(
      generate_english_summary(station_summary, taxa_lookup, cruise_info,
                               phyto_groups = phyto_groups,
                               provider = llm_provider,
                               unclassified_fractions = unclassified_fractions),
      error = function(e) {
        warning("LLM English summary failed: ", e$message, call. = FALSE)
        "[Write English summary here. (LLM generation failed)]"
      }
    )
  }

  # Swedish summary (translation of the English summary)
  doc <- officer::body_add_par(doc, "Sammanfattning", style = "heading 2")
  swedish_text <- "[Skriv sammanfattning pa svenska har.]"
  if (use_llm) {
    report_progress("Swedish summary")
    swedish_text <- tryCatch(
      translate_summary_to_swedish(english_text, provider = llm_provider),
      error = function(e) {
        warning("LLM Swedish translation failed: ", e$message, call. = FALSE)
        "[Skriv sammanfattning pa svenska har. (LLM generation failed)]"
      }
    )
  }
  doc <- add_formatted_par(doc, swedish_text, taxa_lookup, style = "Normal")
  doc <- officer::body_add_par(doc, "")

  doc <- officer::body_add_par(doc, "Summary", style = "heading 2")
  doc <- add_formatted_par(doc, english_text, taxa_lookup, style = "Normal")

  # Summary table
  summary_rows <- data.frame(
    Parameter = character(0), Value = character(0),
    stringsAsFactors = FALSE
  )
  if (!is.null(image_counts) && nrow(image_counts) > 0) {
    summary_rows <- rbind(summary_rows, data.frame(
      Parameter = "Total samples collected (cruise)",
      Value = as.character(nrow(image_counts)),
      stringsAsFactors = FALSE
    ))
  }
  if (!is.null(n_station_samples)) {
    summary_rows <- rbind(summary_rows, data.frame(
      Parameter = "Samples from AlgAware stations",
      Value = as.character(n_station_samples),
      stringsAsFactors = FALSE
    ))
  }
  if (!is.null(total_bio_images)) {
    summary_rows <- rbind(summary_rows, data.frame(
      Parameter = "Biological images analysed",
      Value = format(total_bio_images, big.mark = ","),
      stringsAsFactors = FALSE
    ))
  }
  if (!is.null(classifier_name) && nzchar(classifier_name)) {
    summary_rows <- rbind(summary_rows, data.frame(
      Parameter = "Classification model (PyTorch)",
      Value = classifier_name,
      stringsAsFactors = FALSE
    ))
  }
  if (!is.null(llm_model) && nzchar(llm_model)) {
    summary_rows <- rbind(summary_rows, data.frame(
      Parameter = "LLM model (text generation)",
      Value = llm_model,
      stringsAsFactors = FALSE
    ))
  }
  if (nrow(summary_rows) > 0) {
    month_year <- extract_month_year(cruise_info)
    table_caption <- if (nzchar(month_year)) {
      paste0(
        "Table 1. Summary of data used to generate the report for the ",
        month_year, " cruise."
      )
    } else {
      "Table 1. Summary of data used to generate the report for the selected cruise."
    }
    doc <- officer::body_add_par(doc, "")
    doc <- officer::body_add_fpar(doc, officer::fpar(
      officer::ftext(
        table_caption,
        officer::fp_text(font.size = 10, font.family = "Adobe Garamond Pro")
      ),
      fp_p = officer::fp_par(text.align = "left")
    ))

    doc <- officer::body_add_table(doc, summary_rows, style = "table_template")
  }

  # Frontpage mosaics are shown after the summaries and table on their own page.
  if (has_mosaic_overview) {
    doc <- officer::body_add_break(doc)
    doc <- add_mosaic_overview(doc, frontpage_baltic_mosaic,
                               frontpage_westcoast_mosaic, cleanup,
                               baltic_taxa = frontpage_baltic_taxa,
                               westcoast_taxa = frontpage_westcoast_taxa,
                               taxa_lookup = taxa_lookup)
  }

  # ---- Maps ----
  fig_num <- 1L
  maps <- create_biomass_maps(station_summary)
  month_year <- extract_month_year(cruise_info)

  doc <- officer::body_add_break(doc)
  doc <- officer::body_add_par(doc, "Spatial biomass distribution", style = "heading 2")
  doc <- officer::body_add_par(doc, "")

  if (!is.null(image_counts) && nrow(image_counts) > 0) {
    img_map <- create_image_count_map(image_counts,
                                      title = "IFCB image concentration")
    doc <- add_centered_plot(doc, img_map, cleanup,
      width = 7, height = 4, display_width = 5.8, display_height = 3.2)
    total_images <- format(sum(image_counts$n_images, na.rm = TRUE),
                           big.mark = ",")
    doc <- officer::body_add_fpar(doc, officer::fpar(
      officer::ftext(
        paste0("Figure ", fig_num,
               ". IFCB image concentration along the cruise track. ",
               "Total images collected: ", total_images, "."),
        officer::fp_text(font.size = 10, font.family = "Adobe Garamond Pro")
      ), fp_p = left_pp
    ))
    fig_num <- fig_num + 1L
    doc <- officer::body_add_par(doc, "")
    doc <- officer::body_add_par(doc, "")
  }

  # Use the correct chl map based on active source
  chl_map_plot <- if (chl_map_source == "ctd" && !is.null(ctd_data)) {
    create_chl_map(compute_ctd_chl_avg(ctd_data),
                   title = "CTD chlorophyll fluorescence (0-20 m avg)")
  } else if (chl_map_source == "lims" && !is.null(lims_data)) {
    create_chl_map(compute_lims_chl_avg(lims_data),
                   title = "Bottle Chl-a (0-20 m avg)")
  } else if (chl_map_source == "lims_hose" && !is.null(lims_data)) {
    create_chl_map(compute_lims_hose_avg(lims_data),
                   title = "Hose Chl-a (0-10 m integrated)")
  } else {
    maps$chl_map
  }

  chl_caption_source <- switch(chl_map_source,
    ctd       = "CTD chlorophyll fluorescence (0-20 m average)",
    lims      = "Bottle chlorophyll-a (0-20 m average)",
    lims_hose = "Hose chlorophyll-a (0-10 m integrated)",
    "FerryBox chlorophyll fluorescence"
  )

  doc <- add_centered_plot(doc, chl_map_plot, cleanup,
    width = 7, height = 4, display_width = 5.8, display_height = 3.2)
  doc <- officer::body_add_fpar(doc, officer::fpar(
    officer::ftext(
      paste0("Figure ", fig_num,
             ". ", chl_caption_source, " at AlgAware stations",
             if (nzchar(month_year)) {
               paste0(" during the ", month_year, " cruise.")
             } else {
               "."
             }),
      officer::fp_text(font.size = 10, font.family = "Adobe Garamond Pro")
    ), fp_p = left_pp
  ))
  fig_num <- fig_num + 1L

  # Build sample counts per station_date for heatmap labels
  sample_counts <- build_sample_counts(station_summary)

  # ---- Heatmaps (each on its own page) ----
  result <- add_heatmap_section(doc, baltic_wide, taxa_lookup, "Baltic Sea",
                                fig_num, cleanup, sample_counts)
  doc <- result$doc
  fig_num <- result$fig_num

  result <- add_heatmap_section(doc, westcoast_wide, taxa_lookup, "West Coast",
                                fig_num, cleanup, sample_counts)
  doc <- result$doc
  fig_num <- result$fig_num

  # ---- Stacked bar charts (both on one page) ----
  has_baltic_bar <- nrow(baltic_wide) > 0 && ncol(baltic_wide) > 1
  has_westcoast_bar <- nrow(westcoast_wide) > 0 && ncol(westcoast_wide) > 1

  if (has_baltic_bar || has_westcoast_bar) {
    doc <- officer::body_add_break(doc)
    doc <- officer::body_add_par(doc, "Relative biovolume", style = "heading 2")

    if (has_baltic_bar) {
      result <- add_stacked_bar_section(doc, baltic_wide, taxa_lookup,
                                        "Baltic Sea", fig_num, cleanup)
      doc <- result$doc
      fig_num <- result$fig_num
    }
    if (has_westcoast_bar) {
      result <- add_stacked_bar_section(doc, westcoast_wide, taxa_lookup,
                                        "West Coast", fig_num, cleanup)
      doc <- result$doc
      fig_num <- result$fig_num
    }
  }

  # Station sections
  doc <- add_station_sections(doc, station_summary, taxa_lookup, use_llm,
                               phyto_groups = phyto_groups,
                               llm_provider = llm_provider,
                               on_llm_progress = report_progress,
                               unclassified_fractions = unclassified_fractions)

  # CTD regional figures (optional, if CTD data loaded)
  ctd_full_for_report <- if (!is.null(ctd_data_full) && nrow(ctd_data_full) > 0) {
    ctd_data_full
  } else if (!is.null(ctd_data) && nrow(ctd_data) > 0) {
    # Fallback: legacy AlgAware-matched data (no region column — skip)
    NULL
  } else {
    NULL
  }
  if (!is.null(ctd_full_for_report)) {
    result <- add_ctd_report_section(
      doc, ctd_full_for_report,
      if (!is.null(lims_data_full) && nrow(lims_data_full) > 0) lims_data_full else NULL,
      chl_stats, fig_num, cleanup
    )
    doc <- result$doc
    fig_num <- result$fig_num
  }

  # Image mosaics (optional per-class mosaics)
  hab_species <- get_hab_species(taxa_lookup)
  doc <- add_mosaic_section(doc, baltic_mosaics, hab_species, "Baltic Sea",
                            cleanup, taxa_lookup)
  doc <- add_mosaic_section(doc, westcoast_mosaics, hab_species,
                            "West Coast", cleanup, taxa_lookup)

  # ---- End content section with page number footer ----
  page_footer <- officer::block_list(
    officer::fpar(
      officer::run_word_field("PAGE"),
      fp_p = officer::fp_par(text.align = "center")
    )
  )
  doc <- officer::body_end_block_section(doc, officer::block_section(
    officer::prop_section(
      type = "continuous",
      footer_default = page_footer,
      footer_even = page_footer,
      footer_first = page_footer
    )
  ))

  # ---- Back page with institutional logos ----
  doc <- add_back_page(doc, cleanup)

  print(doc, target = output_path)

  # Post-process: set page numbering to start at 1 for the content section
  if (has_cover_page) {
    fix_page_numbering(output_path)
  }

  invisible(output_path)
}

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

#' Add a banner with SMHI and AlgAware logos
#'
#' Reads SMHI and ALGAWARE logo files from \code{inst/templates/logos/},
#' trims whitespace, optionally adds baseline-alignment padding, and
#' inserts them side-by-side as inline images in a centred paragraph.
#' Falls back to the legacy \code{ALGAWARE_title.PNG} banner if the
#' individual logos are unavailable.
#'
#' @param doc An rdocx object.
#' @param center_pp A centred \code{fp_par} paragraph property object.
#' @param cleanup Environment with a \code{files} character vector for
#'   tracking temp files (may be NULL).
#' @param logo_scale Numeric scaling factor applied to the base logo height.
#'   Default 1.
#' @return The modified rdocx object.
#' @keywords internal
add_report_banner <- function(doc, center_pp, cleanup = NULL,
                              logo_scale = 1) {
  prep_logo <- function(path, fallback_ratio, bottom_pad_px = 0L) {
    tryCatch({
      img <- magick::image_read(path)
      img <- magick::image_trim(img)
      img <- magick::image_border(img, color = "white", geometry = "8x8")
      if (bottom_pad_px > 0) {
        info0 <- magick::image_info(img)
        new_geom <- paste0(
          as.integer(info0$width[1]), "x",
          as.integer(info0$height[1]) + as.integer(bottom_pad_px)
        )
        # Add extra whitespace below the logo to nudge it upward relative to
        # neighboring logos when inline-baseline aligned in Word.
        img <- magick::image_extent(img, geometry = new_geom,
                                    gravity = "north", color = "white")
      }
      out <- tempfile(fileext = ".png")
      magick::image_write(img, path = out, format = "png")
      if (!is.null(cleanup) && exists("files", envir = cleanup, inherits = FALSE)) {
        cleanup$files <- c(cleanup$files, out)
      }
      info <- magick::image_info(img)
      list(path = out, ratio = as.numeric(info$width[1]) / as.numeric(info$height[1]))
    }, error = function(e) fallback_ratio)
  }

  smhi_logo <- system.file("templates", "logos", "smhi.png",
                           package = "algaware")
  algaware_logo <- system.file("templates", "logos", "ALGAWARE.PNG",
                               package = "algaware")

  has_smhi <- nzchar(smhi_logo)
  has_algaware <- nzchar(algaware_logo)

  if (has_smhi || has_algaware) {
    logo_height <- 0.55 * logo_scale
    parts <- list()

    if (has_smhi) {
      # Compensate for the descender in "AlgAware" so SMHI aligns to the
      # main letter baseline rather than the bottom of the "g".
      smhi_prepped <- prep_logo(smhi_logo, fallback_ratio = 305 / 152,
                                bottom_pad_px = 28L)
      smhi_path <- if (is.list(smhi_prepped)) smhi_prepped$path else smhi_logo
      smhi_ratio <- if (is.list(smhi_prepped)) smhi_prepped$ratio else smhi_prepped
      parts <- c(parts, list(
        officer::external_img(
          smhi_path,
          width = logo_height * smhi_ratio,
          height = logo_height
        )
      ))
    }
    if (has_smhi && has_algaware) {
      parts <- c(parts, list(
        officer::ftext("              ", officer::fp_text(font.size = 11))
      ))
    }
    if (has_algaware) {
      algaware_prepped <- prep_logo(algaware_logo, fallback_ratio = 4)
      algaware_path <- if (is.list(algaware_prepped)) {
        algaware_prepped$path
      } else {
        algaware_logo
      }
      algaware_ratio <- if (is.list(algaware_prepped)) {
        algaware_prepped$ratio
      } else {
        algaware_prepped
      }
      parts <- c(parts, list(
        officer::external_img(
          algaware_path,
          width = logo_height * algaware_ratio,
          height = logo_height
        )
      ))
    }

    return(officer::body_add_fpar(
      doc,
      do.call(officer::fpar, c(parts, list(fp_p = center_pp)))
    ))
  }

  legacy_banner <- system.file("templates", "ALGAWARE_title.PNG",
                               package = "algaware")
  if (nzchar(legacy_banner)) {
    doc <- officer::body_add_fpar(doc, officer::fpar(
      officer::external_img(legacy_banner, width = 6, height = 6 * 106 / 859),
      fp_p = center_pp
    ))
  }
  doc
}

#' Add front page to the report
#'
#' Inserts a professional cover page with the AlgAware logo, report title,
#' issue number, diary number placeholder, cruise information, and cruise
#' phytoplankton group-composition map. Mosaics are placed on the following page via
#' \code{add_mosaic_overview()}.
#'
#' @param doc An rdocx object.
#' @param cleanup Environment with a \code{files} character vector.
#' @param taxa_lookup Optional taxa lookup table with \code{italic} column.
#' @param cruise_info Cruise information string (e.g. "RV Svea March cruise,
#'   2026-03-15 to 2026-03-22").
#' @param report_number Optional report issue number (e.g. "1").
#' @param report_dnr Optional diarienummer string (e.g. "2026-1234").
#' @param image_counts Optional image counts data for the cruise track map.
#' @param group_map_plot Optional phytoplankton group composition pie map for
#'   the front page.
#' @return The modified rdocx object.
#' @keywords internal
add_front_page <- function(doc, cleanup,
                           taxa_lookup = NULL, cruise_info = "",
                           report_number = NULL, report_dnr = NULL,
                           image_counts = NULL,
                           group_map_plot = NULL) {
  font <- "Adobe Garamond Pro"
  center_pp <- officer::fp_par(text.align = "center")

  title_prop <- officer::fp_text(font.size = 18, bold = TRUE, color = "#1b3a4b",
                                  font.family = font)
  subtitle_prop <- officer::fp_text(font.size = 14, color = "#1b3a4b",
                                     font.family = font)
  info_prop <- officer::fp_text(font.size = 11, color = "#555555",
                                 font.family = font)
  dnr_prop <- officer::fp_text(font.size = 10, color = "#888888",
                                font.family = font)

  # -- Banner (SMHI + ALGAWARE logos, centered) --
  doc <- add_report_banner(doc, center_pp, cleanup, logo_scale = 1.1)

  doc <- officer::body_add_par(doc, "")
  doc <- officer::body_add_par(doc, "")

  # -- Title (centered) --
  doc <- officer::body_add_fpar(doc, officer::fpar(
    officer::ftext("ALGAL SITUATION IN MARINE WATERS SURROUNDING SWEDEN",
                   title_prop),
    fp_p = center_pp
  ))
  doc <- officer::body_add_par(doc, "")

  # -- Issue line: "Report No X, Month Year" (centered) --
  month_year <- extract_month_year(cruise_info)
  report_no <- if (!is.null(report_number) && nzchar(trimws(report_number))) {
    trimws(report_number)
  } else {
    "X"
  }
  issue_text <- paste0("Report No ", report_no)
  if (nzchar(month_year)) issue_text <- paste0(issue_text, ", ", month_year)

  doc <- officer::body_add_fpar(doc, officer::fpar(
    officer::ftext(issue_text, subtitle_prop),
    fp_p = center_pp
  ))

  # -- Cruise info (centered) --
  if (nzchar(cruise_info)) {
    cruise_info_front <- sub(
      "^RV Svea\\s+[A-Za-z]+\\s+cruise,\\s*",
      "RV Svea cruise, ",
      cruise_info
    )
    doc <- officer::body_add_fpar(doc, officer::fpar(
      officer::ftext(cruise_info_front, info_prop),
      fp_p = center_pp
    ))
  }

  # -- Dnr line (centered, lighter) --
  dnr_text <- if (!is.null(report_dnr) && nzchar(trimws(report_dnr))) {
    paste0("Dnr: ", trimws(report_dnr))
  } else {
    "Dnr: ____/____/____"
  }
  doc <- officer::body_add_fpar(doc, officer::fpar(
    officer::ftext(dnr_text, dnr_prop),
    fp_p = center_pp
  ))

  doc <- officer::body_add_par(doc, "")
  doc <- officer::body_add_par(doc, "")

  # -- Phytoplankton group composition map (centered) --
  if (!is.null(group_map_plot)) {
    doc <- add_centered_plot(doc, group_map_plot, cleanup,
      width = 8, height = 5.6, display_width = 6.1, display_height = 4.3)

    doc <- officer::body_add_fpar(doc, officer::fpar(
      officer::ftext(
        paste0(
          "Phytoplankton group composition at AlgAware stations based on carbon concentration. ",
          "Pie slices show relative group composition; pie size shows relative total carbon concentration among stations."
        ),
        officer::fp_text(font.size = 10, font.family = font)
      ),
      fp_p = center_pp
    ))
  }

  doc
}

#' Build ftext objects for a taxon display name
#'
#' Returns a list of \code{officer::ftext} objects for use in an
#' \code{officer::fpar} caption. If the taxon is flagged as italic in
#' \code{taxa_lookup}, the genus/species part is formatted with
#' \code{italic_prop} and any trailing sflag (e.g. "spp.") with
#' \code{normal_prop}. Unknown taxa receive a single normal ftext.
#'
#' @param display_name Character display name (name + sflag combined).
#' @param taxa_lookup Taxa lookup data frame with \code{name}, \code{sflag},
#'   and \code{italic} columns.
#' @param normal_prop An \code{officer::fp_text} properties object for normal text.
#' @param italic_prop An \code{officer::fp_text} properties object for italic text.
#' @return A list of \code{officer::ftext} objects.
#' @noRd
make_taxon_ftexts <- function(display_name, taxa_lookup, normal_prop, italic_prop) {
  if (is.null(taxa_lookup) || nrow(taxa_lookup) == 0) {
    return(list(officer::ftext(display_name, normal_prop)))
  }
  sflag_col <- if ("sflag" %in% names(taxa_lookup)) taxa_lookup$sflag else ""
  sflag_col[is.na(sflag_col)] <- ""
  dn <- trimws(paste(taxa_lookup$name, sflag_col))
  idx <- match(display_name, dn)
  if (is.na(idx) || !isTRUE(taxa_lookup$italic[idx])) {
    return(list(officer::ftext(display_name, normal_prop)))
  }
  name_part <- taxa_lookup$name[idx]
  sflag_part <- taxa_lookup$sflag[idx]
  if (is.na(sflag_part)) sflag_part <- ""
  if (nzchar(sflag_part)) {
    list(
      officer::ftext(name_part, italic_prop),
      officer::ftext(paste0(" ", sflag_part), normal_prop)
    )
  } else {
    list(officer::ftext(name_part, italic_prop))
  }
}

#' Add front page mosaic overview page to the report
#'
#' @param doc An rdocx object.
#' @param baltic_mosaic Magick image for the Baltic Sea mosaic, or NULL.
#' @param westcoast_mosaic Magick image for the West Coast mosaic, or NULL.
#' @param cleanup Environment with a \code{files} character vector.
#' @param baltic_taxa Optional character vector of taxa names for Baltic caption.
#' @param westcoast_taxa Optional character vector of taxa names for West Coast.
#' @param taxa_lookup Optional taxa lookup table with \code{italic} column.
#' @return The modified rdocx object.
#' @keywords internal
add_mosaic_overview <- function(doc, baltic_mosaic, westcoast_mosaic, cleanup,
                                baltic_taxa = NULL, westcoast_taxa = NULL,
                                taxa_lookup = NULL) {
  font <- "Adobe Garamond Pro"
  center_pp <- officer::fp_par(text.align = "center")

  mosaic_heading_prop <- officer::fp_text(font.size = 12, bold = TRUE,
                                           color = "#1b3a4b",
                                           font.family = font)
  caption_prop <- officer::fp_text(font.size = 9, font.family = font)
  caption_italic <- officer::fp_text(font.size = 9, italic = TRUE,
                                      font.family = font)

  italic_names <- if (!is.null(taxa_lookup) &&
                      "italic" %in% names(taxa_lookup)) {
    is_italic <- taxa_lookup$italic == TRUE & !is.na(taxa_lookup$italic)
    unique(taxa_lookup$name[is_italic & nzchar(taxa_lookup$name)])
  } else {
    character(0)
  }

  add_one_mosaic <- function(doc, mosaic, label, taxa = NULL,
                             add_spacing = FALSE) {
    if (is.null(mosaic)) return(doc)
    if (add_spacing) doc <- officer::body_add_par(doc, "")

    doc <- officer::body_add_fpar(doc, officer::fpar(
      officer::ftext(label, mosaic_heading_prop),
      fp_p = center_pp
    ))

    mosaic_file <- tempfile(fileext = ".png")
    magick::image_write(mosaic, mosaic_file)
    cleanup$files <- c(cleanup$files, mosaic_file)

    info <- magick::image_info(mosaic)
    display_width <- min(5.8, info$width / 300)
    display_height <- display_width * info$height / info$width
    if (display_height > 3.3) {
      display_height <- 3.3
      display_width <- display_height * info$width / info$height
    }

    doc <- officer::body_add_fpar(doc, officer::fpar(
      officer::external_img(mosaic_file, width = display_width,
                            height = display_height),
      fp_p = center_pp
    ))

    if (!is.null(taxa) && length(taxa) > 0) {
      intro_text <- if (length(taxa) == 1L) {
        paste0("The most common taxon by biovolume concentration in the ",
               label, " was: ")
      } else {
        paste0("The most common taxa by biovolume concentration in the ",
               label, " were: ")
      }
      parts <- lapply(seq_along(taxa), function(i) {
        sep <- if (i < length(taxa)) ", " else ""
        taxon_ft <- make_taxon_ftexts(taxa[i], taxa_lookup,
                                      caption_prop, caption_italic)
        c(
          list(officer::ftext(paste0(i, ". "), caption_prop)),
          taxon_ft,
          list(officer::ftext(sep, caption_prop))
        )
      })
      fp <- do.call(
        officer::fpar,
        c(
          list(officer::ftext(intro_text, caption_prop)),
          unlist(parts, recursive = FALSE),
          list(officer::ftext(". Black bars represent 5 \u00b5m.", caption_prop)),
          list(fp_p = center_pp)
        )
      )
      doc <- officer::body_add_fpar(doc, fp)
    }
    doc
  }

  doc <- add_one_mosaic(doc, baltic_mosaic, "Baltic Sea images", baltic_taxa)
  doc <- add_one_mosaic(doc, westcoast_mosaic, "West Coast images", westcoast_taxa,
                        add_spacing = TRUE)
  doc
}

#' Add back page with institutional logos at the bottom
#'
#' Adds a final page with SMHI and HaV logos anchored to the page footer
#' so they always appear at the bottom regardless of content length.
#'
#' @param doc An rdocx object.
#' @param cleanup Environment with a \code{files} character vector.
#' @return The modified rdocx object.
#' @keywords internal
add_back_page <- function(doc, cleanup) {
  smhi_logo <- system.file("templates", "logos", "smhi.png",
                           package = "algaware")
  hav_candidates <- c(
    "hav-logo_en_1000px.png",
    "hav-logo_en_color_1000px.png",
    "hav-logo_en_rgb_1000px.png",
    "hav-logo_en_black_1000px.png"
  )
  hav_paths <- vapply(
    hav_candidates,
    function(x) system.file("templates", "logos", x, package = "algaware"),
    character(1)
  )
  hav_paths <- hav_paths[nzchar(hav_paths)]
  hav_logo <- if (length(hav_paths) > 0) hav_paths[[1]] else ""

  has_smhi <- nzchar(smhi_logo)
  has_hav <- nzchar(hav_logo)
  if (!has_smhi && !has_hav) return(doc)

  center_pp <- officer::fp_par(text.align = "center")

  parts <- list()
  if (has_smhi) {
    parts <- c(parts, list(
      officer::external_img(smhi_logo, width = 1.5, height = 1.5 * 152 / 305)
    ))
  }
  if (has_smhi && has_hav) {
    parts <- c(parts, list(
      officer::ftext("          ", officer::fp_text(font.size = 11))
    ))
  }
  if (has_hav) {
    parts <- c(parts, list(
      officer::external_img(hav_logo, width = 2.5, height = 2.5 * 361 / 1000)
    ))
  }

  logo_fpar <- do.call(officer::fpar, c(parts, list(fp_p = center_pp)))
  logo_footer <- officer::block_list(logo_fpar)

  # Create exactly one final page, then attach a dedicated footer section
  # to that page so logos stay at the bottom without adding a trailing page.
  doc <- officer::body_add_break(doc)
  doc <- officer::body_end_block_section(doc, officer::block_section(
    officer::prop_section(
      type = "continuous",
      footer_default = logo_footer,
      footer_even = logo_footer,
      footer_first = logo_footer
    )
  ))

  doc
}

#' Fix page numbering to start at 1 after the front page
#'
#' Post-processes the .docx file XML to add \code{pgNumType} with
#' \code{start="1"} to the second section (first content section).
#'
#' @param docx_path Path to the .docx file.
#' @keywords internal
fix_page_numbering <- function(docx_path) {
  tryCatch({
    tmp_dir <- tempfile("docx_fix_")
    dir.create(tmp_dir)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    utils::unzip(docx_path, exdir = tmp_dir)

    doc_xml_path <- file.path(tmp_dir, "word", "document.xml")
    xml <- readLines(doc_xml_path, warn = FALSE)
    xml_text <- paste(xml, collapse = "\n")

    # Find the second <w:sectPr (first content section after front page)
    # and inject pgNumType start="1"
    sect_positions <- gregexpr("<w:sectPr", xml_text)[[1]]
    if (length(sect_positions) >= 2) {
      # Insert pgNumType right after the second <w:sectPr...> opening tag
      second_pos <- sect_positions[2]
      # Find the closing > of this tag
      close_pos <- regexpr(">", substring(xml_text, second_pos))
      insert_at <- second_pos + close_pos - 1
      xml_text <- paste0(
        substring(xml_text, 1, insert_at),
        '<w:pgNumType w:start="1"/>',
        substring(xml_text, insert_at + 1)
      )
      writeLines(xml_text, doc_xml_path)

      # Re-zip
      old_wd <- getwd()
      on.exit(setwd(old_wd), add = TRUE)
      setwd(tmp_dir)
      files <- list.files(".", recursive = TRUE, all.files = TRUE)
      file.remove(docx_path)
      utils::zip(docx_path, files, flags = "-r9Xq")
    }
  }, error = function(e) {
    warning("Could not fix page numbering: ", e$message, call. = FALSE)
  })
}

#' Extract month and year from cruise info string
#'
#' Parses a string like "RV Svea March cruise, 2026-03-15 to 2026-03-22"
#' and returns "March 2026".
#'
#' @param cruise_info Cruise information string.
#' @return Character string with month and year, or empty string.
#' @keywords internal
extract_month_year <- function(cruise_info) {
  if (!nzchar(cruise_info)) return("")
  months <- c("January", "February", "March", "April", "May", "June",
              "July", "August", "September", "October", "November", "December")
  month <- ""
  for (m in months) {
    if (grepl(m, cruise_info, fixed = TRUE)) {
      month <- m
      break
    }
  }
  year_match <- regmatches(cruise_info, regexpr("\\d{4}", cruise_info))
  year <- if (length(year_match) > 0) year_match[1] else ""
  trimws(paste(month, year))
}
