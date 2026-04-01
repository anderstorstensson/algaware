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
#' @param max_cols Maximum images per row, or \code{NULL} (default) to
#'   auto-detect from median aspect ratio.  Set to a high value (e.g.
#'   \code{Inf}) to pack purely by width.
#' @param labels Optional character vector of labels (e.g. sequence numbers)
#'   to annotate on each image.  Must be the same length as
#'   \code{image_paths}.  Labels are drawn after resizing so font size is
#'   consistent.
#' @return A \code{magick} image object.
#' @export
create_mosaic <- function(image_paths, n_images = 32L,
                          max_width_px = 1800L, target_height = 120L,
                          max_height_px = 1500L, max_cols = NULL,
                          labels = NULL) {
  if (length(image_paths) == 0) {
    stop("No images provided for mosaic", call. = FALSE)
  }

  if (length(image_paths) > n_images) {
    idx <- sample(length(image_paths), n_images)
    imgs_sample <- image_paths[idx]
    if (!is.null(labels)) labels <- labels[idx]
  } else {
    imgs_sample <- image_paths
  }

  # Read and resize to target height
  img_list <- lapply(imgs_sample, function(p) {
    magick::image_resize(magick::image_read(p), paste0("x", target_height))
  })

  # Annotate images with labels (e.g. sequence numbers) after resize
  if (!is.null(labels)) {
    font_size <- max(16L, as.integer(target_height * 0.28))
    img_list <- mapply(function(img, lbl) {
      magick::image_annotate(img, lbl, size = font_size, color = "black",
                             location = "+3+1", weight = 700)
    }, img_list, labels, SIMPLIFY = FALSE)
  }

  # Compute median background color
  median_col <- compute_median_color(img_list)

  # Get dimensions
  widths <- vapply(img_list, function(img) {
    as.numeric(magick::image_info(img)$width)
  }, numeric(1))

  # Adaptive grid layout based on image shape:
  # Chain-forming diatoms produce very wide (elongated) images, while round
  # cells are more compact. The median aspect ratio determines how many
  # images we try to fit per row.  When max_cols is supplied the heuristic
  # is bypassed (useful for mixed-taxa mosaics where width alone suffices).
  if (!is.null(max_cols)) {
    tile_cols <- max_cols
  } else {
    aspect_ratios <- widths / target_height
    median_aspect <- stats::median(aspect_ratios)
    tile_cols <- if (median_aspect > 4) {
      1L
    } else if (median_aspect > 2.5) {
      2L
    } else if (median_aspect > 1.5) {
      3L
    } else {
      4L
    }
  }

  # Sort images by width descending
  ord <- order(widths, decreasing = TRUE)
  img_list <- img_list[ord]
  widths <- widths[ord]

  # Determine the minimum number of rows needed, accounting for 4px gaps
  total_content_width <- sum(widths) + max(0, (length(widths) - 1)) * 4
  n_rows <- max(1L, ceiling(total_content_width / max_width_px))

  # Distribute images across rows using Longest Processing Time (LPT)
  # scheduling: assign each image (widest first) to the row with the
  # smallest current total.  This balances row widths and minimises grey.
  rows <- vector("list", n_rows)
  width_rows <- vector("list", n_rows)
  row_totals <- rep(0, n_rows)
  for (k in seq_len(n_rows)) {
    rows[[k]] <- list()
    width_rows[[k]] <- numeric(0)
  }

  for (i in seq_along(img_list)) {
    w <- widths[i]

    # Find the row with the least total width that can still accept this image
    # (respects both tile_cols and max_width_px constraints)
    candidates <- which(
      vapply(rows, length, integer(1)) < tile_cols &
        row_totals + w + ifelse(vapply(rows, length, integer(1)) > 0, 4, 0) <=
          max_width_px
    )

    if (length(candidates) == 0) {
      # Need an extra row
      rows <- c(rows, list(list(img_list[[i]])))
      width_rows <- c(width_rows, list(w))
      row_totals <- c(row_totals, w)
      n_rows <- n_rows + 1L
    } else {
      # Pick the lightest row among valid candidates
      best <- candidates[which.min(row_totals[candidates])]
      gap <- if (length(rows[[best]]) > 0) 4 else 0
      rows[[best]] <- c(rows[[best]], list(img_list[[i]]))
      width_rows[[best]] <- c(width_rows[[best]], w)
      row_totals[best] <- row_totals[best] + w + gap
    }
  }

  # Drop any empty rows (can happen when n_rows was overestimated)
  non_empty <- vapply(rows, function(r) length(r) > 0, logical(1))
  rows <- rows[non_empty]
  width_rows <- width_rows[non_empty]

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

#' Generate a numbered frontpage mosaic
#'
#' Extracts one random image per top taxon, annotates each with a sequence
#' number, and composes them into a single mosaic.  Returns the mosaic image
#' together with the ordered taxa names for a figure caption.
#'
#' @param classifications Classification data.frame.
#' @param taxa_lookup Taxa lookup table.
#' @param samples Character vector of sample PIDs for this region.
#' @param raw_data_path Path to raw data directory (contains .roi files).
#' @param non_bio Character vector of non-biological class names to exclude.
#' @param n_images Number of taxa/images to include. Default 15.
#' @param temp_dir Temporary directory for extracted PNGs.
#' @return A list with \code{mosaic} (magick image) and \code{taxa}
#'   (character vector of taxa in numbered order), or \code{NULL}.
#' @export
generate_frontpage_mosaic <- function(classifications, taxa_lookup, samples,
                                      raw_data_path, non_bio,
                                      n_images = 15L,
                                      temp_dir = tempdir()) {
  region_class <- classifications[classifications$sample_name %in% samples, ]
  region_class <- region_class[!region_class$class_name %in% non_bio, ]

  name_map <- stats::setNames(taxa_lookup$name, taxa_lookup$clean_names)
  sci_names <- name_map[region_class$class_name]
  sci_names <- sci_names[!is.na(sci_names)]
  if (length(sci_names) == 0) return(NULL)

  top_taxa <- utils::head(names(sort(table(sci_names), decreasing = TRUE)),
                          n_images)

  fp_dir <- file.path(temp_dir, "algaware_frontpage_auto")
  dir.create(fp_dir, recursive = TRUE, showWarnings = FALSE)

  images <- list()
  for (taxon in top_taxa) {
    img_info <- extract_random_taxon_image(
      taxon, classifications, taxa_lookup, samples, raw_data_path, fp_dir
    )
    if (!is.null(img_info)) images[[taxon]] <- img_info
  }
  if (length(images) == 0) return(NULL)

  paths <- vapply(images, function(img) img$path, character(1))
  exists_mask <- file.exists(paths)
  paths <- paths[exists_mask]
  taxa_names <- names(images)[exists_mask]
  if (length(paths) == 0) return(NULL)

  mosaic <- tryCatch(
    create_mosaic(paths, n_images = length(paths),
                  max_width_px = 1800L, target_height = 120L,
                  max_height_px = 1100L, max_cols = Inf,
                  labels = as.character(seq_along(paths))),
    error = function(e) NULL
  )
  if (is.null(mosaic)) return(NULL)

  list(mosaic = mosaic, taxa = taxa_names)
}
