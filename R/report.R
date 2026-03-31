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
                            frontpage_westcoast_mosaic = NULL) {
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

  doc <- officer::read_docx(template)

  # Front page (optional: only when at least one frontpage mosaic is provided)
  has_frontpage <- !is.null(frontpage_baltic_mosaic) ||
    !is.null(frontpage_westcoast_mosaic)

  if (has_frontpage) {
    doc <- add_front_page(doc, frontpage_baltic_mosaic,
                          frontpage_westcoast_mosaic, cleanup)
    doc <- officer::body_add_break(doc, value = "page")
  }

  # Logo header
  logo_path <- system.file("templates", "ALGAWARE_title.PNG", package = "algaware")
  if (nzchar(logo_path)) {
    doc <- officer::body_add_img(doc, logo_path, width = 6, height = 6 * 106 / 859)
    doc <- officer::body_add_par(doc, "")
  }

  # Title
  doc <- officer::body_add_par(doc, "AlgAware Report", style = "heading 1")
  doc <- officer::body_add_par(doc, cruise_info, style = "Normal")
  doc <- officer::body_add_par(doc, "")

  # Introduction
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
                               provider = llm_provider),
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
                               provider = llm_provider),
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

  # Maps
  doc <- officer::body_add_par(doc, "Maps", style = "heading 2")
  fig_num <- 1L

  # Image count map (cruise-wide)
  if (!is.null(image_counts) && nrow(image_counts) > 0) {
    img_map <- create_image_count_map(image_counts)
    doc <- add_plot_to_doc(doc, img_map, cleanup,
      width = 7, height = 5, display_width = 6, display_height = 4.3)
    total_images <- format(sum(image_counts$n_images, na.rm = TRUE),
                           big.mark = ",")
    doc <- officer::body_add_par(
      doc,
      paste0("Figure ", fig_num,
             ". IFCB image counts along the cruise track. ",
             "Total images collected: ", total_images, "."),
      style = "Normal"
    )
    doc <- officer::body_add_par(doc, "")
    fig_num <- fig_num + 1L
  }

  # Biomass map
  maps <- create_biomass_maps(station_summary)

  doc <- add_plot_to_doc(doc, maps$biomass_map, cleanup,
    width = 7, height = 5, display_width = 6, display_height = 4.3)
  doc <- officer::body_add_par(
    doc,
    paste0("Figure ", fig_num,
           ". Total carbon biomass at AlgAware stations."),
    style = "Normal"
  )
  doc <- officer::body_add_par(doc, "")
  fig_num <- fig_num + 1L

  # Chlorophyll map
  doc <- add_plot_to_doc(doc, maps$chl_map, cleanup,
    width = 7, height = 5, display_width = 6, display_height = 4.3)
  doc <- officer::body_add_par(
    doc,
    paste0("Figure ", fig_num,
           ". Chlorophyll fluorescence at AlgAware stations."),
    style = "Normal"
  )
  doc <- officer::body_add_par(doc, "")
  fig_num <- fig_num + 1L

  # Build sample counts per station_date for heatmap labels
  sample_counts <- build_sample_counts(station_summary)

  # Heatmaps and stacked bars
  result <- add_heatmap_section(doc, baltic_wide, taxa_lookup, "Baltic Sea",
                                fig_num, cleanup, sample_counts)
  doc <- result$doc
  fig_num <- result$fig_num

  result <- add_heatmap_section(doc, westcoast_wide, taxa_lookup, "West Coast",
                                fig_num, cleanup, sample_counts)
  doc <- result$doc
  fig_num <- result$fig_num

  result <- add_stacked_bar_section(doc, baltic_wide, taxa_lookup, "Baltic Sea",
                                    fig_num, cleanup)
  doc <- result$doc
  fig_num <- result$fig_num

  result <- add_stacked_bar_section(doc, westcoast_wide, taxa_lookup,
                                    "West Coast", fig_num, cleanup)
  doc <- result$doc
  fig_num <- result$fig_num

  # Station sections
  doc <- add_station_sections(doc, station_summary, taxa_lookup, use_llm,
                               llm_provider = llm_provider,
                               on_llm_progress = report_progress)

  # Image mosaics
  hab_species <- get_hab_species(taxa_lookup)
  doc <- add_mosaic_section(doc, baltic_mosaics, hab_species, "Baltic Sea",
                            cleanup)

  doc <- add_mosaic_section(doc, westcoast_mosaics, hab_species,
                            "West Coast", cleanup)

  print(doc, target = output_path)
  invisible(output_path)
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

#' Add a heatmap section to the report
#' @keywords internal
add_heatmap_section <- function(doc, wide_data, taxa_lookup, title,
                                fig_num, cleanup, sample_counts = NULL) {
  if (nrow(wide_data) > 0 && ncol(wide_data) > 1) {
    doc <- officer::body_add_par(doc, paste(title, "- Biovolume Heatmap"),
                                 style = "heading 2")
    hm <- create_heatmap(wide_data, taxa_lookup = taxa_lookup,
                          sample_counts = sample_counts)
    hm_file <- tempfile(fileext = ".png")
    hm_height <- max(4, min(12, nrow(wide_data) * 0.25 + 2))
    ggplot2::ggsave(hm_file, hm, width = 8, height = hm_height, dpi = 300)
    cleanup$files <- c(cleanup$files, hm_file)
    doc <- officer::body_add_img(doc, hm_file, width = 6,
                                 height = hm_height * 6 / 8)
    doc <- officer::body_add_par(
      doc,
      paste0("Figure ", fig_num,
             ". Biovolume heatmap for ", title, " stations."),
      style = "Normal"
    )
    doc <- officer::body_add_par(doc, "")
    fig_num <- fig_num + 1L
  }
  list(doc = doc, fig_num = fig_num)
}

#' Add a stacked bar section to the report
#' @keywords internal
add_stacked_bar_section <- function(doc, wide_data, taxa_lookup, title,
                                    fig_num, cleanup) {
  if (nrow(wide_data) > 0 && ncol(wide_data) > 1) {
    doc <- officer::body_add_par(doc, paste(title, "- Relative Biovolume"),
                                 style = "heading 2")
    sb <- create_stacked_bar(wide_data, taxa_lookup = taxa_lookup)
    sb_file <- tempfile(fileext = ".png")
    ggplot2::ggsave(sb_file, sb, width = 8, height = 5, dpi = 300)
    cleanup$files <- c(cleanup$files, sb_file)
    doc <- officer::body_add_img(doc, sb_file, width = 6, height = 3.75)
    doc <- officer::body_add_par(
      doc,
      paste0("Figure ", fig_num,
             ". Relative biovolume of top 10 taxa at ", title, " stations."),
      style = "Normal"
    )
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
                                 on_llm_progress = NULL) {
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
      description <- tryCatch(
        generate_station_description(station_data, taxa_lookup,
                                     station_summary,
                                     provider = llm_provider),
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
    # Cap at half page height (~5 inches on A4)
    if (display_height > 5) {
      display_height <- 5
      display_width <- display_height * info$width / info$height
    }
    hab_note <- if (taxon %in% hab_species) " *" else ""
    doc <- officer::body_add_par(doc, paste0(taxon, hab_note),
                                 style = "heading 3")
    doc <- officer::body_add_img(doc, mosaic_file,
                                 width = display_width,
                                 height = display_height)
    caption <- paste0("Mosaic ", mosaic_num, ". Example images of ", taxon,
                      " from ", region_label, " stations.")
    if (taxon %in% hab_species) {
      caption <- paste0(caption, " * Potentially harmful taxon.")
    }
    doc <- officer::body_add_par(doc, caption, style = "Normal")
    doc <- officer::body_add_par(doc, "")
    mosaic_num <- mosaic_num + 1L
  }

  doc
}

#' Add front page to the report
#'
#' Inserts the report front page with logo, diary number placeholder, and
#' two summary mosaics (Baltic Sea and West Coast).
#'
#' @param doc An rdocx object.
#' @param baltic_mosaic Optional magick image for Baltic Sea.
#' @param westcoast_mosaic Optional magick image for West Coast.
#' @param cleanup Environment with a \code{files} character vector.
#' @return The modified rdocx object.
#' @keywords internal
add_front_page <- function(doc, baltic_mosaic, westcoast_mosaic, cleanup) {
  # Logo

  logo_path <- system.file("templates", "ALGAWARE_title.PNG",
                           package = "algaware")
  if (nzchar(logo_path)) {
    doc <- officer::body_add_img(doc, logo_path, width = 6,
                                 height = 6 * 106 / 859)
  }

  # Diary number placeholder
  doc <- officer::body_add_par(doc, "")
  doc <- officer::body_add_par(doc, "Dnr: XXXX-XXXX", style = "Normal")
  doc <- officer::body_add_par(doc, "")

  # Helper to add a frontpage mosaic with region label
  add_fp_mosaic <- function(doc, mosaic, label) {
    if (is.null(mosaic)) return(doc)

    doc <- officer::body_add_par(doc, label, style = "heading 3")

    mosaic_file <- tempfile(fileext = ".png")
    magick::image_write(mosaic, mosaic_file)
    cleanup$files <- c(cleanup$files, mosaic_file)

    info <- magick::image_info(mosaic)
    # Scale to fit page width (max 6 inches), preserving aspect ratio
    display_width <- min(6, info$width / 300)
    display_height <- display_width * info$height / info$width
    # Cap height to leave room for both mosaics on the page
    if (display_height > 3.5) {
      display_height <- 3.5
      display_width <- display_height * info$width / info$height
    }

    doc <- officer::body_add_img(doc, mosaic_file,
                                 width = display_width,
                                 height = display_height)
    doc
  }

  doc <- add_fp_mosaic(doc, baltic_mosaic, "Baltic Sea")
  doc <- add_fp_mosaic(doc, westcoast_mosaic, "West Coast")

  doc
}
