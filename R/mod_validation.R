#' Validation Module UI
#'
#' @param id Module namespace ID.
#' @return A UI element.
#' @export
mod_validation_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "d-grid gap-1",
      shiny::actionButton(ns("store_annotations"), "Store Annotations",
                          class = "btn-success btn-sm",
                          icon = shiny::icon("save")),
      shiny::actionButton(ns("relabel_selected"), "Relabel Selected",
                          class = "btn-outline-info btn-sm",
                          icon = shiny::icon("arrow-right-arrow-left")),
      shiny::actionButton(ns("invalidate_selected"), "Unclassify Selected",
                          class = "btn-outline-danger btn-sm",
                          icon = shiny::icon("eraser")),
      shiny::actionButton(ns("relabel_class"), "Relabel Class",
                          class = "btn-info btn-sm",
                          icon = shiny::icon("arrows-rotate")),
      shiny::actionButton(ns("invalidate_class"), "Unclassify Class",
                          class = "btn-warning btn-sm",
                          icon = shiny::icon("ban")),
      shiny::actionButton(ns("add_custom_class"), "Add Custom Class",
                          class = "btn-outline-secondary btn-sm",
                          icon = shiny::icon("plus"))
    ),
    shiny::hr(),
    shiny::fileInput(ns("import_corrections_file"),
      label = NULL,
      buttonLabel = shiny::tagList(shiny::icon("upload"), " Import corrections"),
      placeholder = "",
      accept = ".csv",
      width = "100%"
    ),
    shiny::uiOutput(ns("validation_status"))
  )
}

#' Parse image IDs into sample_name and roi_number
#'
#' Splits composite image IDs (e.g. \code{"D20221023T000155_IFCB134_00042"})
#' back into their sample_name and roi_number components.
#'
#' @param img_ids Character vector of image IDs.
#' @return A data.frame with \code{sample_name} and \code{roi_number} columns.
#' @keywords internal
parse_image_ids <- function(img_ids) {
  do.call(rbind, lapply(img_ids, function(img_id) {
    parts <- strsplit(img_id, "_")[[1]]
    roi_num <- as.integer(parts[length(parts)])
    samp_name <- paste(parts[-length(parts)], collapse = "_")
    data.frame(sample_name = samp_name, roi_number = roi_num,
               stringsAsFactors = FALSE)
  }))
}

#' Get the current region context for validation
#'
#' Resolves the current region samples, class list, index, and class name.
#' Shared by all validation actions to avoid duplication.
#'
#' @param rv Reactive values for app state.
#' @return A list with \code{region_samples}, \code{classes}, \code{idx},
#'   and \code{current_class} (NULL if no classes).
#' @keywords internal
get_region_context <- function(rv) {
  region_samples <- if (rv$current_region == "EAST") {
    rv$baltic_samples
  } else {
    rv$westcoast_samples
  }

  classes <- sort(unique(
    rv$classifications$class_name[
      rv$classifications$sample_name %in% region_samples
    ]
  ))

  idx <- min(rv$current_class_idx, length(classes))
  current_class <- if (length(classes) > 0) classes[idx] else NULL

  list(
    region_samples = region_samples,
    classes = classes,
    idx = idx,
    current_class = current_class
  )
}

#' Validation Module Server
#'
#' Provides five validation actions:
#' \enumerate{
#'   \item \strong{Store Annotations}: save selected images to the SQLite
#'     database (persistent, shared with ClassiPyR)
#'   \item \strong{Relabel Selected}: move selected images to a different class
#'     (session-only, logged in rv$corrections)
#'   \item \strong{Unclassify Selected}: move selected images to "unclassified"
#'     in one click (session-only); a fixed-target shortcut for Relabel Selected
#'   \item \strong{Relabel Class}: move ALL images of the current class in
#'     the current region to a different class (session-only)
#'   \item \strong{Unclassify Class}: mark an entire class as non-biological /
#'     unclassified (session-only)
#' }
#'
#' As crash protection, the corrections log is also auto-saved to
#' \code{<local_storage_path>/corrections/} (same CSV format as "Download
#' corrections") whenever the user navigates to another class or region and
#' on clean session end; see \code{autosave_corrections()}.
#'
#' @param id Module namespace ID.
#' @param rv Reactive values for app state.
#' @param config Reactive values with settings.
#' @return NULL (side effects only).
#' @export
mod_validation_server <- function(id, rv, config) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Keep rv$classifications_all (all samples, including excluded ones) in sync
    # with rv$classifications (active samples only). Validation actions operate
    # on the active slice; this helper propagates the same changes to the full
    # dataset so that excluded samples are also corrected when re-included.
    update_full_classifications <- function(sample_names, roi_numbers, new_class) {
      if (is.null(rv$classifications_all) || nrow(rv$classifications_all) == 0) {
        return(invisible(NULL))
      }
      keys_all <- paste0(rv$classifications_all$sample_name, "_",
                         rv$classifications_all$roi_number)
      keys_target <- paste0(sample_names, "_", roi_numbers)
      mask_all <- keys_all %in% keys_target
      if (any(mask_all)) {
        rv$classifications_all$class_name[mask_all] <- new_class
      }
      invisible(NULL)
    }

    # Apply a session-only relabel of the currently selected images to `target`.
    # Shared by "Relabel Selected" and "Unclassify Selected" (target =
    # "unclassified"): parse the selection, rewrite matching rows in both the
    # active and full classification tables, log the corrections, and clear the
    # selection. Returns the number of images changed (0 if none matched, in
    # which case the selection is still cleared). Callers handle their own
    # notifications since the wording differs.
    # Keep rv$current_class_idx within the region's (possibly shrunken)
    # class list after an action that can empty classes -- the same clamp
    # confirm_relabel applies inline. Without it, emptying the last class of
    # a region left the index past the end, showing "Class N of N" with dead
    # navigation arrows for the first click(s).
    clamp_class_index <- function() {
      ctx <- get_region_context(rv)
      rv$current_class_idx <- max(1L, min(rv$current_class_idx,
                                          length(ctx$classes)))
    }

    relabel_selected_images <- function(target) {
      parsed <- parse_image_ids(rv$selected_images)

      updated <- rv$classifications
      updated_keys <- paste0(updated$sample_name, "_", updated$roi_number)
      parsed_keys <- paste0(parsed$sample_name, "_", parsed$roi_number)
      mask <- updated_keys %in% parsed_keys

      n_changed <- sum(mask)
      if (n_changed == 0) {
        rv$selected_images <- character(0)
        return(0L)
      }

      # Record corrections
      new_corrections <- data.frame(
        sample_name = updated$sample_name[mask],
        roi_number = updated$roi_number[mask],
        original_class = updated$class_name[mask],
        new_class = target,
        stringsAsFactors = FALSE
      )
      rv$corrections <- rbind(rv$corrections, new_corrections)

      updated$class_name <- ifelse(mask, target, updated$class_name)
      rv$classifications <- updated
      update_full_classifications(updated$sample_name[mask],
                                  updated$roi_number[mask], target)
      rv$selected_images <- character(0)
      rv$summaries_stale <- TRUE
      clamp_class_index()

      n_changed
    }

    # ---- 1. Store Annotations (to database) ----
    shiny::observeEvent(input$store_annotations, {
      shiny::req(length(rv$selected_images) > 0)

      ctx <- get_region_context(rv)
      if (is.null(ctx$current_class)) return()

      # Parse selected image IDs back to sample_name + roi_number, and take
      # each image's class from the working classification table rather than
      # from the class currently displayed in the gallery. Stamping the
      # displayed class onto the whole selection could silently overwrite
      # correct database annotations with the wrong class whenever the
      # selection contained images from another class.
      parsed <- parse_image_ids(rv$selected_images)
      cls_keys <- paste0(rv$classifications$sample_name, "_",
                         rv$classifications$roi_number)
      idx <- match(paste0(parsed$sample_name, "_", parsed$roi_number),
                   cls_keys)
      parsed$class_name <- rv$classifications$class_name[idx]
      parsed <- parsed[!is.na(parsed$class_name), , drop = FALSE]
      if (nrow(parsed) == 0) {
        shiny::showNotification(
          "None of the selected images are in the loaded classifications.",
          type = "warning"
        )
        rv$selected_images <- character(0)
        return()
      }

      # Validate classes against class list before saving
      selected_classes <- unique(parsed$class_name)
      invalid_classes <- if (length(rv$class_list) > 0) {
        setdiff(selected_classes, rv$class_list)
      } else {
        character(0)
      }
      if (length(invalid_classes) > 0) {
        shiny::showNotification(
          paste0("'", paste(invalid_classes, collapse = "', '"),
                 "' is not in the database class ",
                 "list. Only database classes can be saved as annotations. ",
                 "This class may be from the taxa lookup or a custom class."),
          type = "error", duration = 8
        )
        return()
      }

      if (!nzchar(config$db_folder)) {
        shiny::showNotification(
          "No database folder configured. Set the database folder in Settings to save annotations.",
          type = "error", duration = 8
        )
        return()
      }

      db_path <- get_db_path(config$db_folder)
      success <- save_annotations_db(
        db_path, parsed,
        annotator = config$annotator,
        class_list = rv$class_list
      )

      if (success) {
        class_label <- if (length(selected_classes) == 1) {
          selected_classes
        } else {
          paste0(length(selected_classes), " classes")
        }
        shiny::showNotification(
          paste0("Saved ", nrow(parsed), " annotations for ", class_label),
          type = "message"
        )
        rv$selected_images <- character(0)
      } else {
        shiny::showNotification("Failed to save annotations", type = "error")
      }
    })

    # ---- 2. Relabel Selected Images (session-only) ----
    shiny::observeEvent(input$relabel_selected, {
      shiny::req(length(rv$selected_images) > 0)

      ctx <- get_region_context(rv)
      grouped <- rv$relabel_choices$grouped
      if (!is.null(ctx$current_class)) {
        grouped <- lapply(grouped, function(cls) {
          setdiff(cls, ctx$current_class)
        })
        grouped <- Filter(function(x) length(x) > 0, grouped)
      }

      shiny::showModal(shiny::modalDialog(
        title = paste0("Relabel ", length(rv$selected_images),
                       " selected image(s)"),
        shiny::p(paste0(
          "Move the selected images to a different class. ",
          "This only affects the current session (not stored in database)."
        )),
        # selected = "" keeps the dropdown empty so the placeholder shows;
        # without it Shiny preselects the first choice and a hasty "Relabel"
        # click moves the images to a class the user never picked.
        shiny::selectizeInput(ns("relabel_selected_target"), "Target class",
                              choices = grouped,
                              selected = "",
                              options = list(
                                placeholder = "Type to search...",
                                maxOptions = 500
                              )),
        footer = shiny::tagList(
          shiny::actionButton(ns("confirm_relabel_selected"), "Relabel",
                              class = "btn-info"),
          shiny::modalButton("Cancel")
        )
      ))
    })

    shiny::observeEvent(input$confirm_relabel_selected, {
      target <- input$relabel_selected_target
      shiny::req(nzchar(target))
      shiny::req(length(rv$selected_images) > 0)

      n_relabeled <- relabel_selected_images(target)

      shiny::removeModal()
      if (n_relabeled > 0) {
        shiny::showNotification(
          paste0("Relabeled ", n_relabeled, " selected image(s) to '",
                 target, "'"),
          type = "message"
        )
      }
    })

    # ---- 2b. Unclassify Selected (session-only) ----
    # One-click shortcut for the common case of relabelling the selected images
    # to "unclassified". Equivalent to "Relabel Selected" with the target fixed
    # to "unclassified", so no target picker is needed. Unlike "Unclassify
    # Class" this only touches the picked images and does not flag the whole
    # class as non-biological (rv$invalidated_classes is left untouched).
    shiny::observeEvent(input$invalidate_selected, {
      if (length(rv$selected_images) == 0) {
        shiny::showNotification("No images selected.", type = "warning")
        return()
      }

      n_invalidated <- relabel_selected_images("unclassified")

      if (n_invalidated > 0) {
        shiny::showNotification(
          paste0("Unclassified ", n_invalidated, " selected image(s)"),
          type = "message"
        )
      }
    })

    # ---- 3. Relabel Entire Class (session-only) ----
    shiny::observeEvent(input$relabel_class, {
      ctx <- get_region_context(rv)
      if (is.null(ctx$current_class)) return()

      # Build target choices from extended list, excluding current class
      grouped <- lapply(rv$relabel_choices$grouped, function(cls) {
        setdiff(cls, ctx$current_class)
      })
      grouped <- Filter(function(x) length(x) > 0, grouped)

      n_images <- sum(
        rv$classifications$class_name == ctx$current_class &
        rv$classifications$sample_name %in% ctx$region_samples
      )

      shiny::showModal(shiny::modalDialog(
        title = paste0("Relabel '", ctx$current_class, "'"),
        shiny::p(paste0(
          "Move all ", n_images, " images of '", ctx$current_class, "' in ",
          if (rv$current_region == "EAST") "Baltic Sea" else "West Coast",
          " to a different class."
        )),
        # selected = "" keeps the dropdown empty so the placeholder shows
        # (see relabel_selected_target above).
        shiny::selectizeInput(ns("relabel_target"), "Target class",
                              choices = grouped,
                              selected = "",
                              options = list(
                                placeholder = "Type to search...",
                                maxOptions = 500
                              )),
        footer = shiny::tagList(
          shiny::actionButton(ns("confirm_relabel"), "Relabel",
                              class = "btn-info"),
          shiny::modalButton("Cancel")
        )
      ))
    })

    shiny::observeEvent(input$confirm_relabel, {
      target <- input$relabel_target
      shiny::req(nzchar(target))

      ctx <- get_region_context(rv)
      if (is.null(ctx$current_class)) return()

      mask <- rv$classifications$class_name == ctx$current_class &
              rv$classifications$sample_name %in% ctx$region_samples
      n_relabeled <- sum(mask)

      # Record corrections
      new_corrections <- data.frame(
        sample_name = rv$classifications$sample_name[mask],
        roi_number = rv$classifications$roi_number[mask],
        original_class = ctx$current_class,
        new_class = target,
        stringsAsFactors = FALSE
      )
      rv$corrections <- rbind(rv$corrections, new_corrections)

      updated <- rv$classifications
      updated$class_name <- ifelse(mask, target, updated$class_name)
      rv$classifications <- updated
      update_full_classifications(updated$sample_name[mask],
                                  updated$roi_number[mask], target)
      rv$summaries_stale <- TRUE

      # Adjust class index if needed (the relabelled class may have vanished
      # from the region's class list). Use the same class-list definition as
      # the gallery (get_region_context / region_classes), and keep the index
      # within [1, length] so it can never point off the end or become 0.
      new_classes <- sort(unique(
        updated$class_name[updated$sample_name %in% ctx$region_samples]
      ))
      rv$current_class_idx <- max(
        1L, min(rv$current_class_idx, length(new_classes))
      )

      shiny::removeModal()
      shiny::showNotification(
        paste0("Relabeled ", n_relabeled, " images from '", ctx$current_class,
               "' to '", target, "'"),
        type = "message"
      )
    })

    # ---- 4. Unclassify Class (session-only) ----
    shiny::observeEvent(input$invalidate_class, {
      ctx <- get_region_context(rv)
      if (is.null(ctx$current_class)) return()

      shiny::showModal(shiny::modalDialog(
        title = "Confirm Unclassification",
        paste0("Unclassify all '", ctx$current_class, "' images in ",
               if (rv$current_region == "EAST") "Baltic Sea" else "West Coast",
               "? They will be treated as unclassified."),
        footer = shiny::tagList(
          shiny::actionButton(ns("confirm_invalidate"), "Unclassify",
                              class = "btn-warning"),
          shiny::modalButton("Cancel")
        )
      ))
    })

    shiny::observeEvent(input$confirm_invalidate, {
      ctx <- get_region_context(rv)
      if (is.null(ctx$current_class)) return()

      # Set all images of this class in the region to unclassified
      mask <- rv$classifications$class_name == ctx$current_class &
              rv$classifications$sample_name %in% ctx$region_samples

      # Record corrections
      new_corrections <- data.frame(
        sample_name = rv$classifications$sample_name[mask],
        roi_number = rv$classifications$roi_number[mask],
        original_class = ctx$current_class,
        new_class = "unclassified",
        stringsAsFactors = FALSE
      )
      rv$corrections <- rbind(rv$corrections, new_corrections)

      updated <- rv$classifications
      updated$class_name <- ifelse(mask, "unclassified", updated$class_name)
      rv$classifications <- updated
      update_full_classifications(updated$sample_name[mask],
                                  updated$roi_number[mask], "unclassified")
      rv$summaries_stale <- TRUE

      rv$invalidated_classes <- unique(c(rv$invalidated_classes,
                                         ctx$current_class))
      clamp_class_index()

      shiny::removeModal()
      shiny::showNotification(
        paste0("Unclassified '", ctx$current_class, "' (", sum(mask),
               " images)"),
        type = "message"
      )
    })

    # ---- 5. Add Custom Class (session-only) ----
    shiny::observeEvent(input$add_custom_class, {
      shiny::showModal(shiny::modalDialog(
        title = "Add Custom Class",
        shiny::p(
          "Add a class not in the database or taxa lookup. ",
          "Custom classes are session-only and cannot be saved to the database, ",
          "but will appear in corrections exports and reports."
        ),
        shiny::textInput(ns("custom_clean_name"), "Class name (identifier)",
                         placeholder = "e.g. Genus_species"),
        shiny::textInput(ns("custom_sci_name"), "Scientific name (display)",
                         placeholder = "e.g. Genus species"),
        shiny::textInput(ns("custom_sflag"), "Species flag (sflag)",
                         placeholder = "e.g. spp. or sp. or group"),
        shiny::numericInput(ns("custom_aphia_id"), "AphiaID (WoRMS)", value = NA,
                            min = 1),
        shiny::checkboxInput(ns("custom_hab"), "Potentially harmful (HAB)", FALSE),
        shiny::checkboxInput(ns("custom_italic"), "Italicize name", TRUE),
        shiny::checkboxInput(ns("custom_is_diatom"), "Is diatom", FALSE),
        footer = shiny::tagList(
          shiny::actionButton(ns("confirm_custom_class"), "Add",
                              class = "btn-primary"),
          shiny::modalButton("Cancel")
        )
      ))
    })

    shiny::observeEvent(input$confirm_custom_class, {
      clean_name <- trimws(input$custom_clean_name)
      sci_name <- trimws(input$custom_sci_name)
      aphia_id <- input$custom_aphia_id

      if (!nzchar(clean_name)) {
        shiny::showNotification("Class name is required.", type = "error")
        return()
      }

      # Check for duplicates across all sources
      all_existing <- c(
        rv$class_list,
        if (!is.null(rv$taxa_lookup)) rv$taxa_lookup$clean_names,
        rv$custom_classes$clean_names
      )
      if (clean_name %in% all_existing) {
        shiny::showNotification(
          paste0("'", clean_name, "' already exists in the class list."),
          type = "error"
        )
        return()
      }

      new_row <- data.frame(
        clean_names = clean_name,
        name = sci_name,
        sflag = trimws(input$custom_sflag %||% ""),
        AphiaID = if (is.na(aphia_id)) NA_integer_ else as.integer(aphia_id),
        HAB = isTRUE(input$custom_hab),
        italic = isTRUE(input$custom_italic),
        is_diatom = isTRUE(input$custom_is_diatom),
        stringsAsFactors = FALSE
      )
      rv$custom_classes <- rbind(rv$custom_classes, new_row)

      # Rebuild relabel choices
      rv$relabel_choices <- build_relabel_choices(
        rv$class_list, rv$taxa_lookup, rv$custom_classes
      )

      shiny::removeModal()
      shiny::showNotification(
        paste0("Added custom class '", clean_name, "'"),
        type = "message"
      )
    })

    # ---- 6. Import Corrections CSV ----
    shiny::observeEvent(input$import_corrections_file, {
      shiny::req(rv$data_loaded, input$import_corrections_file)

      path <- input$import_corrections_file$datapath
      df <- tryCatch(
        as_utf8_columns(
          utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8")
        ),
        error = function(e) {
          shiny::showNotification(
            paste0("Could not read file: ", e$message),
            type = "error", duration = 8
          )
          NULL
        }
      )
      if (is.null(df)) return()

      required <- c("sample_name", "roi_number", "original_class", "new_class")
      missing_cols <- setdiff(required, names(df))
      if (length(missing_cols) > 0) {
        shiny::showNotification(
          paste0("File is missing required columns: ",
                 paste(missing_cols, collapse = ", ")),
          type = "error", duration = 8
        )
        return()
      }

      df$roi_number <- as.integer(df$roi_number)
      n_import <- nrow(df)
      n_current <- nrow(rv$corrections)

      # Preview what will change
      relabels <- df[df$new_class != "unclassified", ]
      invalidated <- unique(df$original_class[df$new_class == "unclassified"])

      warning_text <- if (n_current > 0) {
        shiny::p(
          style = "color: #dc3545;",
          shiny::icon("triangle-exclamation"),
          paste0(" This will replace your ", n_current,
                 " existing correction(s).")
        )
      }

      shiny::showModal(shiny::modalDialog(
        title = "Import Corrections",
        shiny::p(paste0("Import ", n_import, " correction(s) from '",
                        input$import_corrections_file$name, "'?")),
        if (nrow(relabels) > 0) {
          agg <- stats::aggregate(roi_number ~ original_class + new_class,
                                  data = relabels, FUN = length)
          shiny::p(
            "Relabels: ",
            shiny::tags$ul(lapply(seq_len(nrow(agg)), function(i) {
              shiny::tags$li(paste0(agg$roi_number[i], "x ", agg$original_class[i],
                                   " \u2192 ", agg$new_class[i]))
            }))
          )
        },
        if (length(invalidated) > 0) {
          shiny::p(paste0("Unclassified: ", paste(invalidated, collapse = ", ")))
        },
        warning_text,
        footer = shiny::tagList(
          shiny::actionButton(ns("confirm_import_corrections"), "Apply",
                              class = "btn-primary"),
          shiny::modalButton("Cancel")
        ),
        # Store df for use in the confirm observer via a temp file to avoid
        # closure issues with reactive invalidation
        easyClose = FALSE
      ))

      # Cache parsed df for the confirm step
      import_cache(df)
    })

    import_cache <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$confirm_import_corrections, {
      df <- import_cache()
      shiny::req(df, rv$classifications_original)

      orig <- rv$classifications_original

      # Apply all corrections to a fresh copy of original
      keys_orig <- paste0(orig$sample_name, "_", orig$roi_number)
      keys_imp  <- paste0(df$sample_name,   "_", df$roi_number)
      match_idx <- match(keys_imp, keys_orig)
      valid     <- !is.na(match_idx)

      result <- orig
      result$class_name[match_idx[valid]] <- df$new_class[valid]

      rv$classifications_all <- result
      # `classifications_original` is the full load-time snapshot, so re-apply
      # the current sample exclusions when deriving the active slice.
      # Assigning the full set here used to resurrect excluded samples in
      # rv$classifications, inflating the report's image totals.
      active <- if (!is.null(rv$matched_metadata_all)) {
        setdiff(unique(rv$matched_metadata_all$pid), rv$excluded_samples)
      } else {
        unique(result$sample_name)
      }
      rv$classifications <- result[result$sample_name %in% active, ,
                                   drop = FALSE]

      # Rebuild corrections log (drop custom metadata columns)
      rv$corrections <- df[, c("sample_name", "roi_number",
                               "original_class", "new_class")]

      # Reconstruct invalidated_classes from corrections that set new_class to "unclassified"
      inv_rows <- df[df$new_class == "unclassified", ]
      rv$invalidated_classes <- unique(inv_rows$original_class)

      # Re-add any custom classes embedded in the export (including the
      # is_diatom flag; files from before that column existed import with
      # is_diatom = FALSE)
      all_known <- c(
        rv$class_list,
        if (!is.null(rv$taxa_lookup)) rv$taxa_lookup$clean_names,
        rv$custom_classes$clean_names
      )
      added <- custom_classes_from_corrections(df, all_known)
      if (nrow(added) > 0) {
        rv$custom_classes <- rbind(rv$custom_classes, added)
        rv$relabel_choices <- build_relabel_choices(
          rv$class_list, rv$taxa_lookup, rv$custom_classes
        )
      }

      rv$summaries_stale <- TRUE
      import_cache(NULL)
      shiny::removeModal()

      n_unmatched <- sum(!valid)
      msg <- paste0("Applied ", sum(valid), " correction(s).")
      if (n_unmatched > 0) {
        msg <- paste0(msg, " ", n_unmatched,
                      " row(s) did not match any image in this dataset.")
      }
      shiny::showNotification(msg, type = "message", duration = 6)
    })

    # ---- 7. Auto-save corrections (crash protection) ----
    # Every time the user navigates to another class or region, write the
    # corrections log to <local_storage_path>/corrections/ -- the same
    # enriched CSV as "Download corrections", so a session lost to a crash
    # (or closed without downloading) can be restored with "Import
    # corrections". Saves only when the log changed since the last write;
    # a failed write warns once per session and never blocks validation.
    autosave_last <- NULL
    autosave_warned <- FALSE

    do_autosave <- function(notify = TRUE) {
      if (!isTRUE(rv$data_loaded)) return(invisible(NULL))
      corrections <- rv$corrections
      if (is.null(corrections) || nrow(corrections) == 0 ||
          identical(corrections, autosave_last)) {
        return(invisible(NULL))
      }

      # First save of this session: an existing file on disk is from an
      # earlier session (possibly the crash being recovered from), so have
      # the helper set it aside as ..._prev.csv instead of clobbering it.
      result <- autosave_corrections(corrections, rv$custom_classes,
                                     config$local_storage_path,
                                     backup_existing = is.null(autosave_last))
      if (result$success) {
        autosave_last <<- corrections
      } else if (notify && !autosave_warned && !is.null(result$error)) {
        autosave_warned <<- TRUE
        shiny::showNotification(
          paste0("Auto-save of corrections failed: ", result$error,
                 ". Use 'Download corrections' to save your work manually."),
          type = "warning", duration = 10
        )
      }
      invisible(NULL)
    }

    shiny::observeEvent(list(rv$current_class_idx, rv$current_region), {
      do_autosave()
    }, ignoreInit = TRUE)

    # Flush on clean session end so work done in the last visited class is
    # not lost when the app is closed without downloading. Does not fire on
    # a crash -- that is what the per-class-navigation saves are for.
    session$onSessionEnded(function() {
      shiny::isolate(do_autosave(notify = FALSE))
    })

    # ---- Status display (sidebar summary of corrections) ----
    output$validation_status <- shiny::renderUI({
      n_selected <- length(rv$selected_images)
      n_invalidated <- length(rv$invalidated_classes)
      n_corrections <- nrow(rv$corrections)

      # Summarize relabels (exclude invalidations)
      relabels <- rv$corrections[rv$corrections$new_class != "unclassified", ]
      relabel_summary <- if (nrow(relabels) > 0) {
        agg <- stats::aggregate(roi_number ~ original_class + new_class,
                                data = relabels, FUN = length)
        paste0(agg$roi_number, "x ", agg$original_class, " -> ", agg$new_class)
      } else {
        character(0)
      }

      shiny::div(
        class = "validation-status",
        shiny::p(shiny::strong(n_selected), " images selected"),
        if (n_corrections > 0) {
          shiny::p(
            style = "color: #0d6efd;",
            shiny::strong(n_corrections), " corrections total"
          )
        },
        if (length(relabel_summary) > 0) {
          shiny::p(
            style = "color: #0dcaf0;",
            "Relabeled: ", paste(relabel_summary, collapse = "; ")
          )
        },
        if (n_invalidated > 0) {
          shiny::p(
            style = "color: #856404;",
            shiny::strong(n_invalidated), " classes unclassified: ",
            paste(rv$invalidated_classes, collapse = ", ")
          )
        }
      )
    })
  })
}
