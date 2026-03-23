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
      shiny::actionButton(ns("relabel_class"), "Relabel Class",
                          class = "btn-info btn-sm",
                          icon = shiny::icon("arrows-rotate")),
      shiny::actionButton(ns("invalidate_class"), "Invalidate Class",
                          class = "btn-warning btn-sm",
                          icon = shiny::icon("ban")),
      shiny::actionButton(ns("add_custom_class"), "Add Custom Class",
                          class = "btn-outline-secondary btn-sm",
                          icon = shiny::icon("plus"))
    ),
    shiny::hr(),
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
      rv$classifications$sample_name %in% region_samples &
      rv$classifications$class_name != "unclassified"
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
#' Provides four validation actions:
#' \enumerate{
#'   \item \strong{Store Annotations}: save selected images to the SQLite
#'     database (persistent, shared with ClassiPyR)
#'   \item \strong{Relabel Selected}: move selected images to a different class
#'     (session-only, logged in rv$corrections)
#'   \item \strong{Relabel Class}: move ALL images of the current class in
#'     the current region to a different class (session-only)
#'   \item \strong{Invalidate Class}: mark an entire class as non-biological /
#'     unclassified (session-only)
#' }
#'
#' @param id Module namespace ID.
#' @param rv Reactive values for app state.
#' @param config Reactive values with settings.
#' @return NULL (side effects only).
#' @export
mod_validation_server <- function(id, rv, config) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- 1. Store Annotations (to database) ----
    shiny::observeEvent(input$store_annotations, {
      shiny::req(length(rv$selected_images) > 0)

      ctx <- get_region_context(rv)
      if (is.null(ctx$current_class)) return()

      # Parse selected image IDs back to sample_name + roi_number
      parsed <- parse_image_ids(rv$selected_images)
      parsed$class_name <- ctx$current_class

      # Validate class against class list before saving
      if (length(rv$class_list) > 0 &&
          !ctx$current_class %in% rv$class_list) {
        shiny::showNotification(
          paste0("'", ctx$current_class, "' is not in the database class ",
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
        shiny::showNotification(
          paste0("Saved ", nrow(parsed), " annotations for ",
                 ctx$current_class),
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
        shiny::selectizeInput(ns("relabel_selected_target"), "Target class",
                              choices = grouped,
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

      # Parse selected image IDs to sample_name + roi_number
      parsed <- parse_image_ids(rv$selected_images)

      # Build mask for matching rows in classifications (vectorized)
      updated <- rv$classifications
      updated_keys <- paste0(updated$sample_name, "_", updated$roi_number)
      parsed_keys <- paste0(parsed$sample_name, "_", parsed$roi_number)
      mask <- updated_keys %in% parsed_keys

      n_relabeled <- sum(mask)
      if (n_relabeled == 0) {
        shiny::removeModal()
        return()
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
      rv$selected_images <- character(0)

      shiny::removeModal()
      shiny::showNotification(
        paste0("Relabeled ", n_relabeled, " selected image(s) to '",
               target, "'"),
        type = "message"
      )
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
        shiny::selectizeInput(ns("relabel_target"), "Target class",
                              choices = grouped,
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

      # Adjust class index if needed (class list changed)
      new_classes <- sort(unique(
        updated$class_name[
          updated$sample_name %in% ctx$region_samples &
          updated$class_name != "unclassified"
        ]
      ))
      rv$current_class_idx <- min(rv$current_class_idx, length(new_classes))

      shiny::removeModal()
      shiny::showNotification(
        paste0("Relabeled ", n_relabeled, " images from '", ctx$current_class,
               "' to '", target, "'"),
        type = "message"
      )
    })

    # ---- 4. Invalidate Class (session-only) ----
    shiny::observeEvent(input$invalidate_class, {
      ctx <- get_region_context(rv)
      if (is.null(ctx$current_class)) return()

      shiny::showModal(shiny::modalDialog(
        title = "Confirm Invalidation",
        paste0("Invalidate all '", ctx$current_class, "' images in ",
               if (rv$current_region == "EAST") "Baltic Sea" else "West Coast",
               "? They will be treated as unclassified."),
        footer = shiny::tagList(
          shiny::actionButton(ns("confirm_invalidate"), "Invalidate",
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

      rv$invalidated_classes <- unique(c(rv$invalidated_classes,
                                         ctx$current_class))

      shiny::removeModal()
      shiny::showNotification(
        paste0("Invalidated '", ctx$current_class, "' (", sum(mask),
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
            shiny::strong(n_invalidated), " classes invalidated: ",
            paste(rv$invalidated_classes, collapse = ", ")
          )
        }
      )
    })
  })
}
