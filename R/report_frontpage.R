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
#' phytoplankton group-composition map. Mosaics are placed on the following
#' page via \code{add_mosaic_overview()}.
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

  doc <- add_report_banner(doc, center_pp, cleanup, logo_scale = 1.1)

  doc <- officer::body_add_par(doc, "")
  doc <- officer::body_add_par(doc, "")

  doc <- officer::body_add_fpar(doc, officer::fpar(
    officer::ftext("ALGAL SITUATION IN MARINE WATERS SURROUNDING SWEDEN",
                   title_prop),
    fp_p = center_pp
  ))
  doc <- officer::body_add_par(doc, "")

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

    sect_positions <- gregexpr("<w:sectPr", xml_text)[[1]]
    if (length(sect_positions) >= 2) {
      second_pos <- sect_positions[2]
      close_pos <- regexpr(">", substring(xml_text, second_pos))
      insert_at <- second_pos + close_pos - 1
      xml_text <- paste0(
        substring(xml_text, 1, insert_at),
        '<w:pgNumType w:start="1"/>',
        substring(xml_text, insert_at + 1)
      )
      writeLines(xml_text, doc_xml_path)

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
