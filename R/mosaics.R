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
#' @param allow_taller_rows Logical; if TRUE, rows with spare horizontal
#'   space are allowed to grow taller (up to limits) to improve readability of
#'   larger organisms in mixed mosaics. Default FALSE.
#' @return A \code{magick} image object.
#' @export
create_mosaic <- function(image_paths, n_images = 32L,
                          max_width_px = 1800L, target_height = 120L,
                          max_height_px = 1500L, max_cols = NULL,
                          labels = NULL, allow_taller_rows = FALSE) {
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

  img_list <- lapply(imgs_sample, magick::image_read)
  img_info <- lapply(img_list, magick::image_info)
  widths_raw <- vapply(img_info, function(info) as.numeric(info$width[1]), numeric(1))
  heights_raw <- vapply(img_info, function(info) as.numeric(info$height[1]), numeric(1))

  # Compute median background color from the unscaled source images so later
  # padding matches the image backdrop.
  median_col <- compute_median_color(img_list)

  # Adaptive grid layout based on image shape:
  # Chain-forming diatoms produce very wide (elongated) images, while round
  # cells are more compact. The median aspect ratio determines how many
  # images we try to fit per row.  When max_cols is supplied the heuristic
  # is bypassed (useful for mixed-taxa mosaics where width alone suffices).
  if (!is.null(max_cols)) {
    tile_cols <- max_cols
  } else {
    aspect_ratios <- widths_raw / pmax(1, heights_raw)
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

  # In mixed-taxa mosaics we want the physically largest images to drive the
  # first rows. For the uniform-height path, width remains the relevant sort.
  ord <- if (isTRUE(allow_taller_rows)) {
    order(heights_raw, widths_raw, decreasing = TRUE)
  } else {
    order(widths_raw, decreasing = TRUE)
  }
  img_list <- img_list[ord]
  widths_raw <- widths_raw[ord]
  heights_raw <- heights_raw[ord]
  if (!is.null(labels)) labels <- labels[ord]

  rows <- list()
  width_rows <- list()
  height_rows <- list()
  label_rows <- list()
  gap_px <- 4
  row_gap_px <- 2
  target_row_width <- max_width_px

  if (isTRUE(allow_taller_rows)) {
    pack_hybrid <- function(scale) {
      scaled_w <- pmax(1L, as.integer(round(widths_raw * scale)))
      scaled_h <- pmax(1L, as.integer(round(heights_raw * scale)))

      # Reserve only the two largest images at the top. Using three creates a
      # top band that is too rigid and leaves large unused regions below.
      n_top <- min(2L, length(scaled_w))
      top_items <- list()
      top_x <- 0L
      top_row_height <- 0L

      if (n_top > 0L) {
        for (i in seq_len(n_top)) {
          top_items[[i]] <- list(
            idx = i,
            x = top_x,
            y = 0L,
            w = scaled_w[i],
            h = scaled_h[i]
          )
          top_row_height <- max(top_row_height, scaled_h[i])
          top_x <- top_x + scaled_w[i] + gap_px
        }
        top_row_width <- top_x - gap_px
      } else {
        top_row_width <- 0L
      }

      if (top_row_width > max_width_px) {
        return(list(fits = FALSE))
      }

      # seq.int(n_top + 1L, n) counts backwards when n <= n_top, producing
      # out-of-range indices (NA sizes) that segfault rectpacker downstream.
      remaining_idx <- if (length(scaled_w) > n_top) {
        seq.int(n_top + 1L, length(scaled_w))
      } else {
        integer(0)
      }
      if (length(remaining_idx) == 0L) {
        return(list(
          fits = TRUE,
          top_items = top_items,
          packed_items = list(),
          used_width = top_row_width,
          used_height = top_row_height
        ))
      }

      available_h <- max_height_px - top_row_height - row_gap_px
      if (available_h <= 0) {
        return(list(fits = FALSE))
      }

      rem_w <- scaled_w[remaining_idx]
      rem_h <- scaled_h[remaining_idx]
      rem_area <- rem_w * rem_h
      rem_aspect <- pmax(rem_w / pmax(1L, rem_h), rem_h / pmax(1L, rem_w))

      # Pack compact, high-area images first. Very elongated chain images tend
      # to create large unusable gaps if placed too early.
      rem_order <- order(rem_aspect > 3, -rem_area, -pmin(rem_w, rem_h))
      rem_idx_ordered <- remaining_idx[rem_order]

      packed <- rectpacker::pack_rects(
        box_width = as.integer(max_width_px),
        box_height = as.integer(available_h),
        rect_widths = as.integer(scaled_w[rem_idx_ordered]),
        rect_heights = as.integer(scaled_h[rem_idx_ordered])
      )

      if (nrow(packed) != length(remaining_idx) ||
          any(!isTRUE(packed$packed) & packed$packed == FALSE, na.rm = TRUE)) {
        return(list(fits = FALSE))
      }

      packed_items <- lapply(seq_along(rem_idx_ordered), function(k) {
        row <- packed[packed$idx == (k - 1L), , drop = FALSE]
        if (nrow(row) != 1L || !isTRUE(row$packed[[1]])) {
          return(NULL)
        }
        list(
          idx = rem_idx_ordered[k],
          x = as.integer(row$x[[1]]),
          y = as.integer(row$y[[1]]),
          w = as.integer(row$w[[1]]),
          h = as.integer(row$h[[1]])
        )
      })
      packed_items <- Filter(Negate(is.null), packed_items)

      if (length(packed_items) != length(remaining_idx)) {
        return(list(fits = FALSE))
      }

      packed_width <- max(vapply(packed_items, function(item) item$x + item$w, numeric(1)))
      packed_height <- max(vapply(packed_items, function(item) item$y + item$h, numeric(1)))

      list(
        fits = TRUE,
        top_items = top_items,
        packed_items = packed_items,
        used_width = max(top_row_width, packed_width),
        used_height = top_row_height + row_gap_px + packed_height
      )
    }

    # Binary search for the largest uniform scale factor such that all images
    # fit within the canvas bounds. 24 iterations give ~1/2^24 (~60 nm)
    # precision relative to the initial upper bound, which is more than enough
    # for pixel-level layout. `best` always holds the last successful layout.
    scale_upper <- min(
      max_width_px / max(widths_raw),
      max_height_px / max(heights_raw)
    )
    low <- 0
    high <- scale_upper
    best <- NULL

    for (iter in seq_len(24)) {
      mid <- (low + high) / 2
      packed <- pack_hybrid(mid)
      if (isTRUE(packed$fits)) {
        best <- packed
        low <- mid
      } else {
        high <- mid
      }
    }

    if (is.null(best)) {
      stop("Could not pack mosaic into the requested page size", call. = FALSE)
    }

    canvas <- magick::image_blank(
      max(1L, as.integer(best$used_width)),
      max(1L, as.integer(best$used_height)),
      color = median_col
    )

    all_item_heights <- c(
      vapply(best$top_items, function(item) item$h, numeric(1)),
      vapply(best$packed_items, function(item) item$h, numeric(1))
    )
    label_size <- NULL
    if (!is.null(labels)) {
      label_size <- max(14L, as.integer(stats::median(all_item_heights) * 0.22))
    }

    for (item in best$top_items) {
      img <- magick::image_resize(
        img_list[[item$idx]],
        paste0(item$w, "x", item$h, "!")
      )
      if (!is.null(labels)) {
        img <- magick::image_annotate(
          img, labels[item$idx], size = label_size, color = "black",
          location = "+3+1", weight = 700
        )
      }
      canvas <- magick::image_composite(
        canvas, img,
        offset = paste0("+", item$x, "+", item$y)
      )
    }

    top_band_height <- if (length(best$top_items) > 0) {
      max(vapply(best$top_items, function(item) item$h, numeric(1))) + row_gap_px
    } else {
      0L
    }
    lower_height <- max(0L, as.integer(best$used_height - top_band_height))

    for (item in best$packed_items) {
      img <- magick::image_resize(
        img_list[[item$idx]],
        paste0(item$w, "x", item$h, "!")
      )
      if (!is.null(labels)) {
        img <- magick::image_annotate(
          img, labels[item$idx], size = label_size, color = "black",
          location = "+3+1", weight = 700
        )
      }
      y_top <- top_band_height + (lower_height - item$y - item$h)
      canvas <- magick::image_composite(
        canvas, img,
        offset = paste0("+", item$x, "+", y_top)
      )
    }

    return(canvas)
  } else {
    # Read and resize to target height for the uniform-height taxon mosaics.
    rows <- NULL
    width_rows <- NULL
    img_list <- lapply(img_list, function(img) {
      magick::image_resize(img, paste0("x", target_height))
    })
    if (!is.null(labels)) {
      font_size <- max(16L, as.integer(target_height * 0.28))
      img_list <- mapply(function(img, lbl) {
        magick::image_annotate(img, lbl, size = font_size, color = "black",
                               location = "+3+1", weight = 700)
      }, img_list, labels, SIMPLIFY = FALSE)
    }
    widths <- vapply(img_list, function(img) {
      as.numeric(magick::image_info(img)$width)
    }, numeric(1))

    # Determine the minimum number of rows needed, accounting for 4px gaps.
    total_content_width <- sum(widths) + max(0, (length(widths) - 1)) * gap_px
    n_rows <- max(1L, ceiling(total_content_width / max_width_px))

    # Balance rows for uniform-height single-taxon mosaics.
    rows <- vector("list", n_rows)
    width_rows <- vector("list", n_rows)
    row_totals <- rep(0, n_rows)
    for (k in seq_len(n_rows)) {
      rows[[k]] <- list()
      width_rows[[k]] <- numeric(0)
    }

    for (i in seq_along(img_list)) {
      w <- widths[i]
      candidates <- which(
        vapply(rows, length, integer(1)) < tile_cols &
          row_totals + w + ifelse(vapply(rows, length, integer(1)) > 0, gap_px, 0) <=
            max_width_px
      )

      if (length(candidates) == 0) {
        rows <- c(rows, list(list(img_list[[i]])))
        width_rows <- c(width_rows, list(w))
        row_totals <- c(row_totals, w)
        n_rows <- n_rows + 1L
      } else {
        best <- candidates[which.min(row_totals[candidates])]
        gap <- if (length(rows[[best]]) > 0) gap_px else 0
        rows[[best]] <- c(rows[[best]], list(img_list[[i]]))
        width_rows[[best]] <- c(width_rows[[best]], w)
        row_totals[best] <- row_totals[best] + w + gap
      }
    }

    non_empty <- vapply(rows, function(r) length(r) > 0, logical(1))
    rows <- rows[non_empty]
    width_rows <- width_rows[non_empty]

    row_height_with_gap <- target_height + row_gap_px
    max_rows <- max(1L, floor(max_height_px / row_height_with_gap))
    if (length(rows) > max_rows) {
      rows <- rows[seq_len(max_rows)]
      width_rows <- width_rows[seq_len(max_rows)]
    }

    total_widths <- vapply(width_rows, function(w) {
      sum(w) + max(0, (length(w) - 1) * gap_px)
    }, numeric(1))
    target_row_width <- min(max(total_widths), max_width_px)
    row_heights <- rep(target_height, length(rows))
    rows <- mapply(function(imgs_row, h) {
      lapply(imgs_row, function(img) {
        magick::image_resize(img, paste0("x", as.integer(round(h))))
      })
    }, rows, row_heights, SIMPLIFY = FALSE)
    width_rows <- lapply(rows, function(imgs_row) {
      vapply(imgs_row, function(img) {
        as.numeric(magick::image_info(img)$width)
      }, numeric(1))
    })
  }

  # Justify each row
  row_imgs <- mapply(
    justify_row,
    rows, width_rows,
    MoreArgs = list(
      target_row_width = target_row_width,
      bg_color = median_col
    ),
    target_height = row_heights,
    SIMPLIFY = FALSE
  )

  # Stack rows vertically with a small gap
  gap_strip <- magick::image_blank(target_row_width, row_gap_px, color = median_col)
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
  sflag_col <- if ("sflag" %in% names(taxa_lookup)) taxa_lookup$sflag else ""
  sflag_col[is.na(sflag_col)] <- ""
  display_names <- trimws(paste(taxa_lookup$name, sflag_col))
  matching_classes <- taxa_lookup$clean_names[display_names == taxon_name]
  classifications[
    classifications$class_name %in% matching_classes &
      classifications$sample_name %in% sample_ids,
  ]
}

#' Extract IFCB PNGs with fallback if scale-bar rendering fails
#'
#' Retries extraction without a scale bar when the first attempt fails. This
#' avoids losing taxa from mosaics due to edge cases in ROI rendering.
#'
#' @param roi_file Path to a .roi file.
#' @param out_folder Output directory for extracted PNGs.
#' @param roi_numbers Integer ROI numbers to extract.
#' @param scale_bar_um Scale bar length in microns. Default 5.
#' @param scale_micron_factor Optional microns-per-pixel factor.
#' @return TRUE if an extraction attempt completed without error, FALSE
#'   otherwise.
#' @keywords internal
extract_pngs_with_fallback <- function(roi_file, out_folder, roi_numbers,
                                       scale_bar_um = 5,
                                       scale_micron_factor = NULL) {
  run_extract <- function(with_scale_bar) {
    warned <- FALSE
    ok <- tryCatch(
      withCallingHandlers({
        if (isTRUE(with_scale_bar)) {
          iRfcb::ifcb_extract_pngs(
            roi_file,
            out_folder,
            ROInumbers = roi_numbers,
            verbose = FALSE,
            scale_bar_um = scale_bar_um,
            scale_micron_factor = scale_micron_factor
          )
        } else {
          iRfcb::ifcb_extract_pngs(
            roi_file,
            out_folder,
            ROInumbers = roi_numbers,
            verbose = FALSE
          )
        }
        TRUE
      }, warning = function(w) {
        warned <<- TRUE
        invokeRestart("muffleWarning")
      }),
      error = function(e) FALSE
    )
    isTRUE(ok) && !warned
  }

  ok <- run_extract(with_scale_bar = TRUE)

  if (ok) {
    return(TRUE)
  }

  run_extract(with_scale_bar = FALSE)
}

#' Check whether an extracted PNG is readable
#'
#' @param path Path to a PNG file.
#' @return TRUE if the file exists and can be read by magick.
#' @keywords internal
is_valid_extracted_png <- function(path) {
  if (!file.exists(path)) {
    return(FALSE)
  }
  tryCatch({
    info <- magick::image_info(magick::image_read(path))
    nrow(info) > 0 && info$width[[1]] > 0 && info$height[[1]] > 0
  }, error = function(e) FALSE)
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
#' @param scale_micron_factor Optional numeric microns-per-pixel factor for
#'   drawing scale bars in extracted PNGs.
#' @param scale_bar_um Scale bar length in microns. Default 5.
#' @return A list with \code{path}, \code{taxon}, \code{sample_name},
#'   \code{roi_number}, and \code{n_available}, or NULL if no image found.
#' @export
extract_random_taxon_image <- function(taxon_name, classifications, taxa_lookup,
                                       sample_ids, raw_data_path, temp_dir,
                                       exclude_rois = NULL,
                                       scale_micron_factor = NULL,
                                       scale_bar_um = 5) {
  rois <- get_taxon_rois(classifications, taxa_lookup, taxon_name, sample_ids)
  if (nrow(rois) == 0) return(NULL)

  # Exclude previously seen ROIs if requested

  if (!is.null(exclude_rois) && nrow(exclude_rois) > 0) {
    key <- paste(rois$sample_name, rois$roi_number)
    excl_key <- paste(exclude_rois$sample_name, exclude_rois$roi_number)
    rois <- rois[!key %in% excl_key, ]
    if (nrow(rois) == 0) return(NULL)
  }

  rois <- rois[sample(seq_len(nrow(rois))), , drop = FALSE]
  out_folder <- file.path(temp_dir, "frontpage_extracted")

  for (i in seq_len(nrow(rois))) {
    samp <- rois$sample_name[i]
    roi_num <- rois$roi_number[i]

    roi_file <- list.files(raw_data_path, pattern = paste0(samp, "\\.roi$"),
                           recursive = TRUE, full.names = TRUE)
    if (length(roi_file) == 0) next

    ok <- extract_pngs_with_fallback(
      roi_file = roi_file[1],
      out_folder = out_folder,
      roi_numbers = roi_num,
      scale_bar_um = scale_bar_um,
      scale_micron_factor = scale_micron_factor
    )
    if (!isTRUE(ok)) next

    expected <- file.path(out_folder, samp,
                          paste0(samp, "_", sprintf("%05d", roi_num), ".png"))
    if (!is_valid_extracted_png(expected)) next

    return(list(
      path = expected,
      taxon = taxon_name,
      sample_name = samp,
      roi_number = roi_num,
      n_available = nrow(rois)
    ))
  }

  NULL
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
#' @param scale_micron_factor Optional numeric microns-per-pixel factor for
#'   drawing scale bars in extracted PNGs.
#' @param scale_bar_um Scale bar length in microns. Default 5.
#' @param temp_dir Temporary directory for extracted PNGs.
#' @return A named list of magick image objects (names = taxon names).
#' @export
create_region_mosaics <- function(wide_summary, classifications, sample_ids,
                                  raw_data_path, taxa_lookup,
                                  n_taxa = 5L, n_images = 32L,
                                  scale_micron_factor = NULL,
                                  scale_bar_um = 5,
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

  # Build display_name lookup once outside the loop
  sflag_col <- if ("sflag" %in% names(taxa_lookup)) taxa_lookup$sflag else ""
  sflag_col[is.na(sflag_col)] <- ""
  taxa_display_names <- trimws(paste(taxa_lookup$name, sflag_col))

  for (taxon in top_taxa) {
    # Find class names that map to this taxon (match on combined name+sflag)
    matching_classes <- taxa_lookup$clean_names[taxa_display_names == taxon]

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

      ok <- extract_pngs_with_fallback(
        roi_file = roi_file[1],
        out_folder = out_folder,
        roi_numbers = samp_rois,
        scale_bar_um = scale_bar_um,
        scale_micron_factor = scale_micron_factor
      )
      if (!isTRUE(ok)) return(character(0))

      # Only collect the specific PNGs for the requested ROIs
      expected_files <- file.path(
        out_folder, samp,
        paste0(samp, "_", sprintf("%05d", samp_rois), ".png")
      )
      expected_files[vapply(expected_files, is_valid_extracted_png, logical(1))]
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
#' @param wide_summary Optional wide-format summary from
#'   \code{create_wide_summary()} for ranking taxa by biovolume concentration.
#' @param scale_micron_factor Optional numeric microns-per-pixel factor for
#'   drawing scale bars in extracted PNGs.
#' @param scale_bar_um Scale bar length in microns. Default 5.
#' @param temp_dir Temporary directory for extracted PNGs.
#' @return A list with \code{mosaic} (magick image) and \code{taxa}
#'   (character vector of taxa in numbered order), or \code{NULL}.
#' @export
generate_frontpage_mosaic <- function(classifications, taxa_lookup, samples,
                                      raw_data_path, non_bio,
                                      n_images = 15L,
                                      wide_summary = NULL,
                                      scale_micron_factor = NULL,
                                      scale_bar_um = 5,
                                      temp_dir = tempdir()) {
  if (!is.null(wide_summary) && nrow(wide_summary) > 0 && ncol(wide_summary) > 1) {
    top_taxa <- get_top_taxa(wide_summary, n_taxa = n_images)
  } else {
    region_class <- classifications[classifications$sample_name %in% samples, ]
    region_class <- region_class[!region_class$class_name %in% non_bio, ]

    sflag_col <- if ("sflag" %in% names(taxa_lookup)) taxa_lookup$sflag else ""
    sflag_col[is.na(sflag_col)] <- ""
    display_name_vec <- trimws(paste(taxa_lookup$name, sflag_col))
    name_map <- stats::setNames(display_name_vec, taxa_lookup$clean_names)
    sci_names <- name_map[region_class$class_name]
    sci_names <- sci_names[!is.na(sci_names) & nzchar(sci_names)]
    if (length(sci_names) == 0) return(NULL)

    top_taxa <- utils::head(names(sort(table(sci_names), decreasing = TRUE)),
                            n_images)
  }

  fp_dir <- file.path(temp_dir, "algaware_frontpage_auto")
  dir.create(fp_dir, recursive = TRUE, showWarnings = FALSE)

  images <- list()
  for (taxon in top_taxa) {
    img_info <- extract_random_taxon_image(
      taxon, classifications, taxa_lookup, samples, raw_data_path, fp_dir,
      scale_micron_factor = scale_micron_factor,
      scale_bar_um = scale_bar_um
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
                  max_width_px = 1800L, target_height = 150L,
                  max_height_px = 1500L, max_cols = Inf,
                  labels = as.character(seq_along(paths)),
                  allow_taller_rows = TRUE),
    error = function(e) NULL
  )
  if (is.null(mosaic)) return(NULL)

  list(mosaic = mosaic, taxa = taxa_names)
}
