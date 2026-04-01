#' Generate the AlgAware Word report
#'
#' Creates a Word document with biomass maps, heatmaps, stacked bar charts,
#' station sections, and image mosaics. Optionally includes a front page with
#' logo, diary number placeholder, and two summary mosaics.
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
                            report_number = NULL) {
  template <- system.file("templates", "report_template.docx",
                          package = "algaware")
  if (!nzchar(template)) {
    stop("Report template not found. Reinstall the algaware package.",
         call. = FALSE)
  }

  # Track temp files for cleanup via environment (survives object copies)
  cleanup <- new.env(parent = emptyenv())
  cleanup$files <- character(0)
  on.exit(unlink(cleanup$files), add = TRUE)

  # Centered paragraph property used for all figures
  center_pp <- officer::fp_par(text.align = "center")

  doc <- officer::read_docx(template)

  # Front page (optional: only when at least one frontpage mosaic is provided)
  has_frontpage <- !is.null(frontpage_baltic_mosaic) ||
    !is.null(frontpage_westcoast_mosaic)

  if (has_frontpage) {
    doc <- add_front_page(doc, cleanup,
                          taxa_lookup = taxa_lookup,
                          cruise_info = cruise_info,
                          report_number = report_number,
                          image_counts = image_counts)

    # End front page section (no header/footer, no page number)
    doc <- officer::body_end_block_section(doc, officer::block_section(
      officer::prop_section(type = "nextPage")
    ))

    # Mosaic overview page (page 2)
    doc <- add_mosaic_overview(doc, frontpage_baltic_mosaic,
                               frontpage_westcoast_mosaic, cleanup,
                               baltic_taxa = frontpage_baltic_taxa,
                               westcoast_taxa = frontpage_westcoast_taxa,
                               taxa_lookup = taxa_lookup)
  }

  # Logo header and title (only when there is no front page)
  if (!has_frontpage) {
    logo_path <- system.file("templates", "ALGAWARE_title.PNG",
                             package = "algaware")
    if (nzchar(logo_path)) {
      doc <- officer::body_add_fpar(doc, officer::fpar(
        officer::external_img(logo_path, width = 6, height = 6 * 106 / 859),
        fp_p = center_pp
      ))
      doc <- officer::body_add_par(doc, "")
    }
    doc <- officer::body_add_par(doc, "AlgAware Report", style = "heading 1")
    doc <- officer::body_add_par(doc, cruise_info, style = "Normal")
    doc <- officer::body_add_par(doc, "")
  }

  # Introduction
  doc <- officer::body_add_par(doc, "Introduction", style = "heading 2")
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
    "Phytoplankton identification is based on ",
    "automated image classification by an AI model, validated by ",
    if (nzchar(annotator)) paste0(annotator, ", SMHI") else "SMHI",
    "."
  )
  if (use_llm) {
    intro <- paste0(
      intro,
      " Report text was drafted with the assistance of a large language model ",
      "and reviewed by ",
      if (nzchar(annotator)) paste0(annotator, ".") else "the analyst."
    )
  }
  doc <- officer::body_add_par(doc, intro, style = "Normal")
  doc <- officer::body_add_par(doc, "")

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

  # Swedish summary
  doc <- officer::body_add_par(doc, "Sammanfattning", style = "heading 2")
  swedish_text <- "[Skriv sammanfattning pa svenska har.]"
  if (use_llm) {
    report_progress("Swedish summary")
    swedish_text <- tryCatch(
      generate_swedish_summary(station_summary, taxa_lookup, cruise_info,
                               provider = llm_provider,
                               unclassified_fractions = unclassified_fractions),
      error = function(e) {
        warning("LLM Swedish summary failed: ", e$message, call. = FALSE)
        "[Skriv sammanfattning pa svenska har. (LLM generation failed)]"
      }
    )
  }
  doc <- add_formatted_par(doc, swedish_text, taxa_lookup, style = "Normal")
  doc <- officer::body_add_par(doc, "")

  # English summary
  doc <- officer::body_add_par(doc, "Summary", style = "heading 2")
  english_text <- "[Write English summary here.]"
  if (use_llm) {
    report_progress("English summary")
    english_text <- tryCatch(
      generate_english_summary(station_summary, taxa_lookup, cruise_info,
                               provider = llm_provider,
                               unclassified_fractions = unclassified_fractions),
      error = function(e) {
        warning("LLM English summary failed: ", e$message, call. = FALSE)
        "[Write English summary here. (LLM generation failed)]"
      }
    )
  }
  doc <- add_formatted_par(doc, english_text, taxa_lookup, style = "Normal")
  doc <- officer::body_add_par(doc, "")

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
    doc <- officer::body_add_table(doc, summary_rows, style = "table_template")
    doc <- officer::body_add_par(doc, "")
  }

  # ---- Maps (each on its own page) ----
  fig_num <- 1L
  maps <- create_biomass_maps(station_summary)

  # Image count map — skip if already shown on the front page
  if (!has_frontpage && !is.null(image_counts) && nrow(image_counts) > 0) {
    doc <- officer::body_add_break(doc)
    doc <- officer::body_add_par(doc, "Maps", style = "heading 2")
    img_map <- create_image_count_map(image_counts)
    doc <- add_centered_plot(doc, img_map, cleanup,
      width = 7, height = 5, display_width = 6, display_height = 4.3)
    total_images <- format(sum(image_counts$n_images, na.rm = TRUE),
                           big.mark = ",")
    doc <- officer::body_add_fpar(doc, officer::fpar(
      officer::ftext(
        paste0("Figure ", fig_num,
               ". IFCB image concentration along the cruise track. ",
               "Total images collected: ", total_images, "."),
        officer::fp_text(font.size = 10, font.family = "Adobe Garamond Pro")
      ), fp_p = center_pp
    ))
    fig_num <- fig_num + 1L
  }

  # Biomass map (own page, with heading if first map)
  doc <- officer::body_add_break(doc)
  if (has_frontpage || is.null(image_counts) || nrow(image_counts) == 0) {
    doc <- officer::body_add_par(doc, "Maps", style = "heading 2")
  }

  doc <- add_centered_plot(doc, maps$biomass_map, cleanup,
    width = 7, height = 5, display_width = 6, display_height = 4.3)
  doc <- officer::body_add_fpar(doc, officer::fpar(
    officer::ftext(
      paste0("Figure ", fig_num,
             ". Total carbon biomass at AlgAware stations."),
      officer::fp_text(font.size = 10, font.family = "Adobe Garamond Pro")
    ), fp_p = center_pp
  ))
  fig_num <- fig_num + 1L

  # Chlorophyll map (own page)
  doc <- officer::body_add_break(doc)
  doc <- add_centered_plot(doc, maps$chl_map, cleanup,
    width = 7, height = 5, display_width = 6, display_height = 4.3)
  doc <- officer::body_add_fpar(doc, officer::fpar(
    officer::ftext(
      paste0("Figure ", fig_num,
             ". Chlorophyll fluorescence at AlgAware stations."),
      officer::fp_text(font.size = 10, font.family = "Adobe Garamond Pro")
    ), fp_p = center_pp
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
    doc <- officer::body_add_par(doc, "Relative Biovolume", style = "heading 2")

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
                               llm_provider = llm_provider,
                               on_llm_progress = report_progress,
                               unclassified_fractions = unclassified_fractions)

  # Image mosaics (optional per-class mosaics)
  hab_species <- get_hab_species(taxa_lookup)
  doc <- add_mosaic_section(doc, baltic_mosaics, hab_species, "Baltic Sea",
                            cleanup)
  doc <- add_mosaic_section(doc, westcoast_mosaics, hab_species,
                            "West Coast", cleanup)

  # ---- End content section with page number footer ----
  page_footer <- officer::block_list(
    officer::fpar(
      officer::run_word_field("PAGE"),
      fp_p = officer::fp_par(text.align = "center")
    )
  )
  doc <- officer::body_end_block_section(doc, officer::block_section(
    officer::prop_section(
      type = "nextPage",
      footer_default = page_footer
    )
  ))

  # ---- Back page with institutional logos ----
  doc <- add_back_page(doc, cleanup)

  print(doc, target = output_path)

  # Post-process: set page numbering to start at 1 for the content section
  if (has_frontpage) {
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
    doc <- officer::body_add_par(doc, paste(title, "- Biovolume Heatmap"),
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
      ), fp_p = officer::fp_par(text.align = "center")
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
      ), fp_p = officer::fp_par(text.align = "center")
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
                                 llm_provider = NULL,
                                 on_llm_progress = NULL,
                                 unclassified_fractions = NULL) {
  doc <- officer::body_add_break(doc)
  doc <- officer::body_add_par(doc, "Station Reports", style = "heading 2")

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
                               cleanup) {
  if (length(mosaics) == 0) return(doc)

  doc <- officer::body_add_break(doc)
  doc <- officer::body_add_par(doc, "Image Mosaics", style = "heading 2")
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
    caption <- paste0("Mosaic ", mosaic_num, ". Example images of ", taxon,
                      " from ", region_label, " stations.")
    if (taxon %in% hab_species) {
      caption <- paste0(caption, " * Potentially harmful taxon.")
    }
    doc <- officer::body_add_fpar(doc, officer::fpar(
      officer::ftext(caption,
        officer::fp_text(font.size = 10, font.family = "Adobe Garamond Pro")),
      fp_p = officer::fp_par(text.align = "center")
    ))
    doc <- officer::body_add_par(doc, "")
    mosaic_num <- mosaic_num + 1L
  }

  doc
}

#' Add front page to the report
#'
#' Inserts a professional cover page with the AlgAware logo, report title,
#' issue number, diary number placeholder, cruise information, and cruise
#' track map. Mosaics are placed on the following page via
#' \code{add_mosaic_overview()}.
#'
#' @param doc An rdocx object.
#' @param cleanup Environment with a \code{files} character vector.
#' @param taxa_lookup Optional taxa lookup table with \code{italic} column.
#' @param cruise_info Cruise information string (e.g. "RV Svea March cruise,
#'   2026-03-15 to 2026-03-22").
#' @param report_number Optional report issue number (e.g. "1").
#' @param image_counts Optional image counts data for the cruise track map.
#' @return The modified rdocx object.
#' @keywords internal
add_front_page <- function(doc, cleanup,
                           taxa_lookup = NULL, cruise_info = "",
                           report_number = NULL, image_counts = NULL) {
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

  # -- Logo (full width, centered) --
  logo_path <- system.file("templates", "ALGAWARE_title.PNG",
                           package = "algaware")
  if (nzchar(logo_path)) {
    doc <- officer::body_add_fpar(doc, officer::fpar(
      officer::external_img(logo_path, width = 6, height = 6 * 106 / 859),
      fp_p = center_pp
    ))
  }

  doc <- officer::body_add_par(doc, "")

  # -- Title (centered) --
  doc <- officer::body_add_fpar(doc, officer::fpar(
    officer::ftext("ALGAL SITUATION IN MARINE WATERS SURROUNDING SWEDEN",
                   title_prop),
    fp_p = center_pp
  ))

  # -- Issue line: "No X, Month Year" (centered) --
  month_year <- extract_month_year(cruise_info)
  issue_text <- ""
  if (!is.null(report_number) && nzchar(trimws(report_number))) {
    issue_text <- paste0("No ", trimws(report_number))
    if (nzchar(month_year)) issue_text <- paste0(issue_text, ", ", month_year)
  } else if (nzchar(month_year)) {
    issue_text <- month_year
  }
  if (nzchar(issue_text)) {
    doc <- officer::body_add_fpar(doc, officer::fpar(
      officer::ftext(issue_text, subtitle_prop),
      fp_p = center_pp
    ))
  }

  # -- Cruise info (centered) --
  if (nzchar(cruise_info)) {
    doc <- officer::body_add_fpar(doc, officer::fpar(
      officer::ftext(cruise_info, info_prop),
      fp_p = center_pp
    ))
  }

  # -- Dnr placeholder (centered, lighter) --
  doc <- officer::body_add_fpar(doc, officer::fpar(
    officer::ftext("Dnr: ____/____/____", dnr_prop),
    fp_p = center_pp
  ))

  doc <- officer::body_add_par(doc, "")

  # -- Cruise track map (centered) --
  if (!is.null(image_counts) && nrow(image_counts) > 0) {
    track_map <- create_image_count_map(image_counts)
    doc <- add_centered_plot(doc, track_map, cleanup,
      width = 7, height = 5, display_width = 6, display_height = 4.3)
  }

  doc
}

#' Add mosaic overview page to the report
#'
#' Inserts Baltic Sea and West Coast summary mosaics with numbered taxa
#' captions. Called after the front page section break.
#'
#' @param doc An rdocx object.
#' @param baltic_mosaic Optional magick image for Baltic Sea.
#' @param westcoast_mosaic Optional magick image for West Coast.
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
  mosaic_heading_note <- officer::fp_text(font.size = 10, color = "#666666",
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
      officer::ftext(paste0(label, " "), mosaic_heading_prop),
      officer::ftext("\u2014 most common taxa by image count",
                     mosaic_heading_note)
    ), style = "Normal")

    mosaic_file <- tempfile(fileext = ".png")
    magick::image_write(mosaic, mosaic_file)
    cleanup$files <- c(cleanup$files, mosaic_file)

    info <- magick::image_info(mosaic)
    display_width <- min(6, info$width / 300)
    display_height <- display_width * info$height / info$width
    if (display_height > 3.5) {
      display_height <- 3.5
      display_width <- display_height * info$width / info$height
    }

    doc <- officer::body_add_fpar(doc, officer::fpar(
      officer::external_img(mosaic_file, width = display_width,
                            height = display_height),
      fp_p = center_pp
    ))

    if (!is.null(taxa) && length(taxa) > 0) {
      parts <- lapply(seq_along(taxa), function(i) {
        sep <- if (i < length(taxa)) ", " else ""
        prop <- if (taxa[i] %in% italic_names) caption_italic else caption_prop
        list(
          officer::ftext(paste0(i, ". "), caption_prop),
          officer::ftext(paste0(taxa[i], sep), prop)
        )
      })
      fp <- do.call(officer::fpar, unlist(parts, recursive = FALSE))
      doc <- officer::body_add_fpar(doc, fp)
    }
    doc
  }

  doc <- add_one_mosaic(doc, baltic_mosaic, "Baltic Sea", baltic_taxa)
  doc <- add_one_mosaic(doc, westcoast_mosaic, "West Coast", westcoast_taxa,
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
  smhi_logo <- system.file("app", "www", "smhi.png", package = "algaware")
  hav_logo <- system.file("app", "www", "hav-logo_en_black_1000px.png",
                          package = "algaware")

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

  # Place logos in the footer of the back page section so they sit at the
  # bottom of the page regardless of body content length.
  # The document ends after this section, so no extra page is created.
  logo_footer <- officer::block_list(logo_fpar)

  # One empty paragraph as the back page body
  doc <- officer::body_add_par(doc, "")

  doc <- officer::body_end_block_section(doc, officer::block_section(
    officer::prop_section(
      type = "nextPage",
      footer_default = logo_footer
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
