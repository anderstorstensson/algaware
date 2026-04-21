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
  chl_avg <- switch(chl_map_source,
    ctd       = if (!is.null(ctd_data))  compute_ctd_chl_avg(ctd_data)   else NULL,
    lims      = if (!is.null(lims_data)) compute_lims_chl_avg(lims_data)  else NULL,
    lims_hose = if (!is.null(lims_data)) compute_lims_hose_avg(lims_data) else NULL,
    NULL
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

  cleanup <- new.env(parent = emptyenv())
  cleanup$files <- character(0)
  on.exit(unlink(cleanup$files), add = TRUE)

  center_pp <- officer::fp_par(text.align = "center")
  left_pp <- officer::fp_par(text.align = "left")

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

  doc <- officer::body_end_block_section(doc, officer::block_section(
    officer::prop_section(type = "nextPage")
  ))

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

  n_stations <- length(unique(station_summary$visit_id))
  llm_total <- 2L + n_stations
  llm_step <- 0L
  report_progress <- function(detail) {
    llm_step <<- llm_step + 1L
    if (is.function(on_llm_progress)) {
      on_llm_progress(llm_step, llm_total, detail)
    }
  }

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

  if (has_mosaic_overview) {
    doc <- officer::body_add_break(doc)
    doc <- add_mosaic_overview(doc, frontpage_baltic_mosaic,
                               frontpage_westcoast_mosaic, cleanup,
                               baltic_taxa = frontpage_baltic_taxa,
                               westcoast_taxa = frontpage_westcoast_taxa,
                               taxa_lookup = taxa_lookup)
  }

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

  sample_counts <- build_sample_counts(station_summary)

  result <- add_heatmap_section(doc, baltic_wide, taxa_lookup, "Baltic Sea",
                                fig_num, cleanup, sample_counts)
  doc <- result$doc
  fig_num <- result$fig_num

  result <- add_heatmap_section(doc, westcoast_wide, taxa_lookup, "West Coast",
                                fig_num, cleanup, sample_counts)
  doc <- result$doc
  fig_num <- result$fig_num

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

  doc <- add_station_sections(doc, station_summary, taxa_lookup, use_llm,
                               phyto_groups = phyto_groups,
                               llm_provider = llm_provider,
                               on_llm_progress = report_progress,
                               unclassified_fractions = unclassified_fractions)

  ctd_full_for_report <- if (!is.null(ctd_data_full) && nrow(ctd_data_full) > 0) {
    ctd_data_full
  } else if (!is.null(ctd_data) && nrow(ctd_data) > 0) {
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

  hab_species <- get_hab_species(taxa_lookup)
  doc <- add_mosaic_section(doc, baltic_mosaics, hab_species, "Baltic Sea",
                            cleanup, taxa_lookup)
  doc <- add_mosaic_section(doc, westcoast_mosaics, hab_species,
                            "West Coast", cleanup, taxa_lookup)

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

  doc <- add_back_page(doc, cleanup)

  print(doc, target = output_path)

  if (has_cover_page) {
    fix_page_numbering(output_path)
  }

  invisible(output_path)
}
