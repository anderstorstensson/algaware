#' Create an adaptive image mosaic for a taxon
#'
#' Produces a mosaic image where elongated organisms (chains) are shown in
#' single rows with fewer images, while compact organisms use a tighter grid.
#'
#' @param image_paths Character vector of PNG file paths.
#' @param n_images Maximum number of images to include. Default 32.
#' @param max_width_px Maximum mosaic width in pixels. Default 1800
#'   (fits A4 page at 300 dpi with margins).
#' @param target_height Base target height in pixels. Default 120.
#' @param max_height_px Maximum mosaic height in pixels. Default 1500
#'   (approximately half an A4 page at 300 dpi). Images are dropped to
#'   stay within this limit.
#' @return A \code{magick} image object.
#' @export
create_mosaic <- function(image_paths, n_images = 32L,
                          max_width_px = 1800L, target_height = 120L,
                          max_height_px = 1500L) {
  if (length(image_paths) == 0) {
    stop("No images provided for mosaic", call. = FALSE)
  }

  imgs_sample <- if (length(image_paths) > n_images) {
    sample(image_paths, n_images)
  } else {
    image_paths
  }

  # Read and resize to target height
  img_list <- lapply(imgs_sample, function(p) {
    magick::image_resize(magick::image_read(p), paste0("x", target_height))
  })

  # Compute median background color
  median_col <- compute_median_color(img_list)

  # Get dimensions
  widths <- vapply(img_list, function(img) {
    as.numeric(magick::image_info(img)$width)
  }, numeric(1))

  # Adaptive grid layout based on image shape:
  # Chain-forming diatoms produce very wide (elongated) images, while round
  # cells are more compact. The median aspect ratio determines how many
  # images we try to fit per row.
  aspect_ratios <- widths / target_height
  median_aspect <- stats::median(aspect_ratios)

  tile_cols <- if (median_aspect > 4) {
    1L   # Very elongated (e.g. long chains) -> one image per row
  } else if (median_aspect > 2.5) {
    2L   # Moderately elongated -> two per row
  } else if (median_aspect > 1.5) {
    3L   # Slightly wide -> three per row
  } else {
    4L   # Compact/round cells -> four per row
  }

  # Adjust: ensure row width doesn't exceed max_width_px
  # Sort images by width descending for better packing
  ord <- order(widths, decreasing = TRUE)
  img_list <- img_list[ord]
  widths <- widths[ord]

  # Build rows greedily: add images left-to-right until the row is full
  # (either max columns reached or pixel width exceeded), then start a new row.
  # 4px gap between images within a row, 2px gap between rows.
  rows <- list()
  width_rows <- list()
  current_row <- list()
  current_widths <- numeric(0)
  current_width <- 0

  for (i in seq_along(img_list)) {
    gap <- if (length(current_row) > 0) 4 else 0
    if (length(current_row) >= tile_cols ||
        (current_width + widths[i] + gap > max_width_px && length(current_row) > 0)) {
      rows <- c(rows, list(current_row))
      width_rows <- c(width_rows, list(current_widths))
      current_row <- list(img_list[[i]])
      current_widths <- widths[i]
      current_width <- widths[i]
    } else {
      current_row <- c(current_row, list(img_list[[i]]))
      current_widths <- c(current_widths, widths[i])
      current_width <- current_width + widths[i] + gap
    }
  }
  if (length(current_row) > 0) {
    rows <- c(rows, list(current_row))
    width_rows <- c(width_rows, list(current_widths))
  }

  # Limit total mosaic height so it fits on approximately half an A4 page
  row_height_with_gap <- target_height + 2  # 2px vertical gap between rows
  max_rows <- max(1L, floor(max_height_px / row_height_with_gap))
  if (length(rows) > max_rows) {
    rows <- rows[seq_len(max_rows)]
    width_rows <- width_rows[seq_len(max_rows)]
  }

  # Determine target row width (widest row, capped at max_width_px)
  total_widths <- vapply(width_rows, function(w) {
    sum(w) + max(0, (length(w) - 1) * 4)
  }, numeric(1))
  target_row_width <- min(max(total_widths), max_width_px)

  # Justify each row
  row_imgs <- mapply(
    justify_row,
    rows, width_rows,
    MoreArgs = list(
      target_row_width = target_row_width,
      target_height = target_height,
      bg_color = median_col
    ),
    SIMPLIFY = FALSE
  )

  # Stack rows vertically with a small gap
  gap_strip <- magick::image_blank(target_row_width, 2, color = median_col)
  parts <- list()
  for (i in seq_along(row_imgs)) {
    if (i > 1) parts <- c(parts, list(gap_strip))
    parts <- c(parts, list(row_imgs[[i]]))
  }

  magick::image_append(magick::image_join(parts), stack = TRUE)
}

#' Justify a row of images to a target width
#'
#' @param imgs_row List of magick image objects.
#' @param widths_row Numeric vector of image widths.
#' @param target_row_width Target total row width in pixels.
#' @param target_height Row height in pixels.
#' @param bg_color Background fill color (hex string).
#' @return A single magick image for the row.
#' @keywords internal
justify_row <- function(imgs_row, widths_row, target_row_width,
                        target_height, bg_color) {
  n <- length(imgs_row)
  total_img_width <- sum(widths_row)
  total_padding <- target_row_width - total_img_width

  if (n == 1) {
    left_pad <- floor(total_padding / 2)
    right_pad <- total_padding - left_pad
    parts <- list()
    if (left_pad > 0) {
      parts <- c(parts, list(
        magick::image_blank(left_pad, target_height, color = bg_color)))
    }
    parts <- c(parts, list(imgs_row[[1]]))
    if (right_pad > 0) {
      parts <- c(parts, list(
        magick::image_blank(right_pad, target_height, color = bg_color)))
    }
    return(magick::image_append(magick::image_join(parts), stack = FALSE))
  }

  if (total_padding <= 0) {
    return(magick::image_append(magick::image_join(imgs_row), stack = FALSE))
  }

  gaps <- n - 1
  base_pad <- floor(total_padding / gaps)
  remainder <- total_padding - base_pad * gaps

  parts <- list()
  for (i in seq_len(n)) {
    parts <- c(parts, list(imgs_row[[i]]))
    if (i < n) {
      this_pad <- base_pad + ifelse(i <= remainder, 1, 0)
      parts <- c(parts, list(
        magick::image_blank(this_pad, target_height, color = bg_color)))
    }
  }

  magick::image_append(magick::image_join(parts), stack = FALSE)
}

#' Compute median pixel color across images
#'
#' @param img_list List of magick image objects.
#' @return Hex color string.
#' @keywords internal
compute_median_color <- function(img_list) {
  tryCatch({
    arrays <- lapply(img_list, function(img) {
      as.integer(magick::image_data(img, channels = "rgb"))
    })
    r <- unlist(lapply(arrays, function(a) a[1, , ]))
    g <- unlist(lapply(arrays, function(a) a[2, , ]))
    b <- unlist(lapply(arrays, function(a) a[3, , ]))
    sprintf("#%02X%02X%02X", stats::median(r), stats::median(g), stats::median(b))
  }, error = function(e) {
    "#F0F0F0"
  })
}

#' Get top taxa by total biovolume from a wide summary
#'
#' @param wide_summary Wide-format summary from \code{create_wide_summary()}.
#' @param n_taxa Number of top taxa to return. Default 10.
#' @return Character vector of scientific names ordered by descending biovolume.
#' @export
get_top_taxa <- function(wide_summary, n_taxa = 10L) {
  data_cols <- names(wide_summary)[-1]
  if (length(data_cols) == 0) return(character(0))
  totals <- rowSums(wide_summary[, data_cols, drop = FALSE], na.rm = TRUE)
  idx <- order(totals, decreasing = TRUE)
  utils::head(wide_summary$scientific_name[idx], n_taxa)
}

#' Get ROI information for a specific taxon in a set of samples
#'
#' @param classifications Classification data.frame with \code{sample_name},
#'   \code{roi_number}, \code{class_name}.
#' @param taxa_lookup Taxa lookup table.
#' @param taxon_name Scientific name of the taxon.
#' @param sample_ids Character vector of sample PIDs to search within.
#' @return A data.frame subset of classifications matching the taxon.
#' @export
get_taxon_rois <- function(classifications, taxa_lookup, taxon_name,
                           sample_ids) {
  matching_classes <- taxa_lookup$clean_names[taxa_lookup$name == taxon_name]
  classifications[
    classifications$class_name %in% matching_classes &
      classifications$sample_name %in% sample_ids,
  ]
}

#' Extract a single random image for a taxon
#'
#' Picks one random ROI from the given taxon and extracts it as a PNG file.
#'
#' @param taxon_name Scientific name of the taxon.
#' @param classifications Classification data.frame.
#' @param taxa_lookup Taxa lookup table.
#' @param sample_ids Character vector of sample PIDs.
#' @param raw_data_path Path to raw data (for .roi files).
#' @param temp_dir Temporary directory for extracted PNGs.
#' @param exclude_rois Optional data.frame with \code{sample_name} and
#'   \code{roi_number} columns to exclude from selection.
#' @return A list with \code{path}, \code{taxon}, \code{sample_name},
#'   \code{roi_number}, and \code{n_available}, or NULL if no image found.
#' @export
extract_random_taxon_image <- function(taxon_name, classifications, taxa_lookup,
                                       sample_ids, raw_data_path, temp_dir,
                                       exclude_rois = NULL) {
  rois <- get_taxon_rois(classifications, taxa_lookup, taxon_name, sample_ids)
  if (nrow(rois) == 0) return(NULL)

  # Exclude previously seen ROIs if requested

  if (!is.null(exclude_rois) && nrow(exclude_rois) > 0) {
    key <- paste(rois$sample_name, rois$roi_number)
    excl_key <- paste(exclude_rois$sample_name, exclude_rois$roi_number)
    rois <- rois[!key %in% excl_key, ]
    if (nrow(rois) == 0) return(NULL)
  }

  idx <- sample(nrow(rois), 1)
  samp <- rois$sample_name[idx]
  roi_num <- rois$roi_number[idx]

  roi_file <- list.files(raw_data_path, pattern = paste0(samp, "\\.roi$"),
                         recursive = TRUE, full.names = TRUE)
  if (length(roi_file) == 0) return(NULL)

  out_folder <- file.path(temp_dir, "frontpage_extracted")
  tryCatch(
    iRfcb::ifcb_extract_pngs(roi_file[1], out_folder, ROInumbers = roi_num,
                              verbose = FALSE),
    error = function(e) return(NULL)
  )

  expected <- file.path(out_folder, samp,
                        paste0(samp, "_", sprintf("%05d", roi_num), ".png"))
  if (!file.exists(expected)) return(NULL)

  list(
    path = expected,
    taxon = taxon_name,
    sample_name = samp,
    roi_number = roi_num,
    n_available = nrow(rois)
  )
}

#' Create mosaics for top taxa in a region
#'
#' Extracts PNGs from .roi files and creates mosaic images for the top N taxa
#' by biovolume.
#'
#' @param wide_summary Wide-format summary from \code{create_wide_summary()}.
#' @param classifications Classification data.frame with \code{sample_name},
#'   \code{roi_number}, \code{class_name}.
#' @param sample_ids Character vector of sample PIDs for this region.
#' @param raw_data_path Path to raw data (for .roi files).
#' @param taxa_lookup Taxa lookup table.
#' @param n_taxa Number of top taxa to create mosaics for.
#' @param n_images Number of images per mosaic.
#' @param temp_dir Temporary directory for extracted PNGs.
#' @return A named list of magick image objects (names = taxon names).
#' @export
create_region_mosaics <- function(wide_summary, classifications, sample_ids,
                                  raw_data_path, taxa_lookup,
                                  n_taxa = 5L, n_images = 32L,
                                  temp_dir = tempdir()) {
  # Find top taxa by total biovolume
  data_cols <- names(wide_summary)[-1]
  if (length(data_cols) == 0) return(list())

  wide_summary$total_bv <- rowSums(
    wide_summary[, data_cols, drop = FALSE], na.rm = TRUE
  )
  top_taxa <- utils::head(
    wide_summary$scientific_name[order(wide_summary$total_bv, decreasing = TRUE)],
    n_taxa
  )

  # Map scientific names back to class names
  mosaics <- list()

  for (taxon in top_taxa) {
    # Find class names that map to this taxon
    matching_classes <- taxa_lookup$clean_names[taxa_lookup$name == taxon]

    # Get ROIs for this taxon from the region samples
    taxon_rois <- classifications[
      classifications$class_name %in% matching_classes &
      classifications$sample_name %in% sample_ids,
    ]

    if (nrow(taxon_rois) == 0) next

    # Extract PNGs for these ROIs
    out_folder <- file.path(temp_dir, "algaware_mosaics")
    samp_names <- unique(taxon_rois$sample_name)
    png_paths_list <- lapply(samp_names, function(samp) {
      roi_file <- list.files(raw_data_path, pattern = paste0(samp, "\\.roi$"),
                             recursive = TRUE, full.names = TRUE)
      if (length(roi_file) == 0) return(character(0))

      samp_rois <- taxon_rois$roi_number[taxon_rois$sample_name == samp]

      tryCatch(
        iRfcb::ifcb_extract_pngs(
          roi_file[1],
          out_folder,
          ROInumbers = samp_rois,
          verbose = FALSE
        ),
        error = function(e) NULL
      )

      # Only collect the specific PNGs for the requested ROIs
      expected_files <- file.path(
        out_folder, samp,
        paste0(samp, "_", sprintf("%05d", samp_rois), ".png")
      )
      expected_files[file.exists(expected_files)]
    })
    png_paths <- unlist(png_paths_list, use.names = FALSE)

    if (length(png_paths) > 0) {
      mosaics[[taxon]] <- create_mosaic(png_paths, n_images = n_images)
    }
  }

  mosaics
}
