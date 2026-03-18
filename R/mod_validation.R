#' Validation Module UI
#'
#' @param id Module namespace ID.
#' @return A UI element.
#' @keywords internal
mod_validation_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "d-flex gap-2 mb-2",
      shiny::actionButton(ns("select_page"), "Select Page",
                          class = "btn-outline-secondary btn-sm"),
      shiny::actionButton(ns("select_all"), "Select All",
                          class = "btn-outline-secondary btn-sm"),
      shiny::actionButton(ns("deselect"), "Deselect All",
                          class = "btn-outline-secondary btn-sm")
    ),
    shiny::div(
      class = "d-flex gap-2",
      shiny::actionButton(ns("store_annotations"), "Store Selected Annotations",
                          class = "btn-success btn-sm",
                          icon = shiny::icon("save")),
      shiny::actionButton(ns("invalidate_class"), "Invalidate Class",
                          class = "btn-warning btn-sm",
                          icon = shiny::icon("ban"))
    ),
    shiny::hr(),
    shiny::uiOutput(ns("validation_status"))
  )
}

#' Validation Module Server
#'
#' @param id Module namespace ID.
#' @param rv Reactive values for app state.
#' @param config Reactive values with settings.
#' @return NULL (side effects only).
#' @keywords internal
mod_validation_server <- function(id, rv, config) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Select page images
    shiny::observeEvent(input$select_page, {
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

      if (length(classes) == 0) return()
      idx <- min(rv$current_class_idx, length(classes))
      current_class <- classes[idx]

      page_imgs <- rv$classifications[
        rv$classifications$class_name == current_class &
        rv$classifications$sample_name %in% region_samples,
      ]

      img_ids <- paste0(page_imgs$sample_name, "_", page_imgs$roi_number)
      rv$selected_images <- union(rv$selected_images, img_ids)
    })

    # Select all images in current class
    shiny::observeEvent(input$select_all, {
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

      if (length(classes) == 0) return()
      idx <- min(rv$current_class_idx, length(classes))
      current_class <- classes[idx]

      all_imgs <- rv$classifications[
        rv$classifications$class_name == current_class &
        rv$classifications$sample_name %in% region_samples,
      ]

      img_ids <- paste0(all_imgs$sample_name, "_", all_imgs$roi_number)
      rv$selected_images <- img_ids
    })

    # Deselect all
    shiny::observeEvent(input$deselect, {
      rv$selected_images <- character(0)
    })

    # Store annotations
    shiny::observeEvent(input$store_annotations, {
      shiny::req(length(rv$selected_images) > 0)

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
      current_class <- classes[idx]

      # Parse selected image IDs back to sample_name + roi_number
      parsed <- do.call(rbind, lapply(rv$selected_images, function(img_id) {
        # Last part after final underscore is roi_number
        parts <- strsplit(img_id, "_")[[1]]
        roi_num <- as.integer(parts[length(parts)])
        samp_name <- paste(parts[-length(parts)], collapse = "_")
        data.frame(sample_name = samp_name, roi_number = roi_num,
                   class_name = current_class, stringsAsFactors = FALSE)
      }))

      # Validate class against class list before saving
      if (length(rv$class_list) > 0 &&
          !current_class %in% rv$class_list) {
        shiny::showNotification(
          paste0("'", current_class, "' is not in the class list. ",
                 "Annotations not saved."),
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
          paste0("Saved ", nrow(parsed), " annotations for ", current_class),
          type = "message"
        )
        rv$selected_images <- character(0)
      } else {
        shiny::showNotification("Failed to save annotations", type = "error")
      }
    })

    # Invalidate class
    shiny::observeEvent(input$invalidate_class, {
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

      if (length(classes) == 0) return()
      idx <- min(rv$current_class_idx, length(classes))
      current_class <- classes[idx]

      shiny::showModal(shiny::modalDialog(
        title = "Confirm Invalidation",
        paste0("Invalidate all '", current_class, "' images in ",
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
      current_class <- classes[idx]

      # Set all images of this class in the region to unclassified
      mask <- rv$classifications$class_name == current_class &
              rv$classifications$sample_name %in% region_samples
      updated <- rv$classifications
      updated$class_name[mask] <- "unclassified"
      rv$classifications <- updated

      rv$invalidated_classes <- unique(c(rv$invalidated_classes, current_class))

      shiny::removeModal()
      shiny::showNotification(
        paste0("Invalidated '", current_class, "' (", sum(mask), " images)"),
        type = "message"
      )
    })

    # Status display
    output$validation_status <- shiny::renderUI({
      n_selected <- length(rv$selected_images)
      n_invalidated <- length(rv$invalidated_classes)

      shiny::div(
        style = "font-size: 12px;",
        shiny::p(shiny::strong(n_selected), " images selected"),
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
