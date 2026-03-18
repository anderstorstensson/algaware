#' Gallery Module UI
#'
#' @param id Module namespace ID.
#' @return A UI element.
#' @keywords internal
mod_gallery_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # Toolbar
    shiny::div(
      class = "d-flex align-items-center gap-2 mb-3 p-2 bg-light rounded",

      # Region toggle
      shiny::radioButtons(ns("region_toggle"), NULL,
                          choices = c("Baltic Sea" = "EAST",
                                      "West Coast" = "WEST"),
                          selected = "EAST", inline = TRUE),

      shiny::div(class = "vr mx-2"),

      # Class navigation
      shiny::actionButton(ns("prev_class"), "",
                          icon = shiny::icon("arrow-left"),
                          class = "btn-outline-secondary btn-sm"),
      shiny::div(
        style = "min-width: 200px; text-align: center;",
        shiny::uiOutput(ns("class_header"))
      ),
      shiny::actionButton(ns("next_class"), "",
                          icon = shiny::icon("arrow-right"),
                          class = "btn-outline-secondary btn-sm"),

      shiny::div(class = "vr mx-2"),

      # Page size
      shiny::selectInput(ns("page_size"), NULL,
                         choices = c("50", "100", "200"),
                         selected = "100", width = "80px"),

      # Pagination
      shiny::actionButton(ns("prev_page"), "",
                          icon = shiny::icon("chevron-left"),
                          class = "btn-outline-secondary btn-sm"),
      shiny::uiOutput(ns("page_info"), inline = TRUE),
      shiny::actionButton(ns("next_page"), "",
                          icon = shiny::icon("chevron-right"),
                          class = "btn-outline-secondary btn-sm")
    ),

    # Gallery area
    shiny::div(
      id = ns("gallery_drag_area"),
      class = "gallery-drag-area",
      shiny::uiOutput(ns("image_gallery"))
    ),

    # Selection box overlay
    shiny::div(id = ns("selection_box"), class = "selection-box")
  )
}

#' Gallery Module Server
#'
#' @param id Module namespace ID.
#' @param rv Reactive values for app state.
#' @param config Reactive values with settings.
#' @return NULL (side effects only).
#' @keywords internal
mod_gallery_server <- function(id, rv, config) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    page <- shiny::reactiveVal(1L)

    # Current region's class list
    region_classes <- shiny::reactive({
      shiny::req(rv$data_loaded, rv$classifications)
      region <- input$region_toggle

      region_samples <- if (region == "EAST") rv$baltic_samples else rv$westcoast_samples
      region_data <- rv$classifications[
        rv$classifications$sample_name %in% region_samples &
        rv$classifications$class_name != "unclassified",
      ]
      sort(unique(region_data$class_name))
    })

    # Current class images
    current_images <- shiny::reactive({
      classes <- region_classes()
      shiny::req(length(classes) > 0, rv$current_class_idx)

      idx <- min(rv$current_class_idx, length(classes))
      current_class <- classes[idx]
      region_samples <- if (input$region_toggle == "EAST") {
        rv$baltic_samples
      } else {
        rv$westcoast_samples
      }

      rv$classifications[
        rv$classifications$class_name == current_class &
        rv$classifications$sample_name %in% region_samples,
      ]
    })

    # Paginated images
    paginated <- shiny::reactive({
      imgs <- current_images()
      shiny::req(nrow(imgs) > 0)
      ps <- as.integer(input$page_size)
      p <- page()
      total_pages <- ceiling(nrow(imgs) / ps)
      p <- min(p, total_pages)
      start <- (p - 1) * ps + 1
      end <- min(p * ps, nrow(imgs))
      imgs[start:end, ]
    })

    # Class header
    output$class_header <- shiny::renderUI({
      classes <- region_classes()
      if (length(classes) == 0) return(shiny::p("No classes"))

      idx <- min(rv$current_class_idx, length(classes))
      current_class <- classes[idx]
      total_imgs <- nrow(current_images())

      # Check HAB status
      hab_species <- get_hab_species(rv$taxa_lookup)
      # Map class_name to scientific name for HAB check
      taxa <- rv$taxa_lookup
      sci_name <- current_class
      if (!is.null(taxa)) {
        match_idx <- match(current_class, taxa$clean_names)
        if (!is.na(match_idx)) sci_name <- taxa$name[match_idx]
      }
      is_hab <- sci_name %in% hab_species

      shiny::div(
        shiny::strong(current_class),
        if (is_hab) {
          shiny::span(
            style = paste0("background: #dc3545; color: white; ",
                           "padding: 1px 6px; border-radius: 3px; ",
                           "font-size: 10px; margin-left: 6px; ",
                           "vertical-align: middle;"),
            "HAB"
          )
        },
        shiny::br(),
        shiny::span(
          style = "font-size: 11px; color: #666;",
          paste0("Class ", idx, " of ", length(classes),
                 " (", total_imgs, " images)")
        )
      )
    })

    # Page info
    output$page_info <- shiny::renderUI({
      imgs <- current_images()
      ps <- as.integer(input$page_size)
      total_pages <- max(1, ceiling(nrow(imgs) / ps))
      shiny::span(
        style = "font-size: 12px; min-width: 60px; text-align: center;",
        paste0(page(), "/", total_pages)
      )
    })

    # Navigation
    shiny::observeEvent(input$prev_class, {
      if (rv$current_class_idx > 1) {
        rv$current_class_idx <- rv$current_class_idx - 1L
        page(1L)
      }
    })

    shiny::observeEvent(input$next_class, {
      classes <- region_classes()
      if (rv$current_class_idx < length(classes)) {
        rv$current_class_idx <- rv$current_class_idx + 1L
        page(1L)
      }
    })

    shiny::observeEvent(input$region_toggle, {
      rv$current_class_idx <- 1L
      rv$current_region <- input$region_toggle
      page(1L)
    })

    shiny::observeEvent(input$prev_page, {
      if (page() > 1) page(page() - 1L)
    })

    shiny::observeEvent(input$next_page, {
      imgs <- current_images()
      ps <- as.integer(input$page_size)
      total_pages <- ceiling(nrow(imgs) / ps)
      if (page() < total_pages) page(page() + 1L)
    })

    # Toggle image selection
    shiny::observeEvent(input$toggle_image, {
      img_id <- input$toggle_image$img
      if (img_id %in% rv$selected_images) {
        rv$selected_images <- setdiff(rv$selected_images, img_id)
      } else {
        rv$selected_images <- c(rv$selected_images, img_id)
      }
    })

    # Drag select
    shiny::observeEvent(input$drag_select, {
      new_imgs <- input$drag_select$images
      rv$selected_images <- union(rv$selected_images, new_imgs)
    })

    # Render gallery
    output$image_gallery <- shiny::renderUI({
      imgs <- tryCatch(paginated(), error = function(e) NULL)
      if (is.null(imgs) || nrow(imgs) == 0) {
        return(shiny::div(
          class = "text-center text-muted p-5",
          shiny::h4("No images to display"),
          shiny::p("Load data or select a different class/region.")
        ))
      }

      # Extract PNGs for current page
      storage <- config$local_storage_path
      raw_dir <- file.path(storage, "raw")
      png_dir <- file.path(tempdir(), "algaware_gallery",
                           session$token)

      for (samp in unique(imgs$sample_name)) {
        samp_dir <- file.path(png_dir, samp)
        if (dir.exists(samp_dir)) next

        roi_file <- list.files(raw_dir,
                               pattern = paste0(samp, "\\.roi$"),
                               recursive = TRUE, full.names = TRUE)
        if (length(roi_file) > 0) {
          samp_rois <- imgs$roi_number[imgs$sample_name == samp]
          tryCatch(
            iRfcb::ifcb_extract_pngs(roi_file[1], png_dir,
                                     ROInumbers = samp_rois,
                                     verbose = FALSE),
            error = function(e) NULL
          )
        }
      }

      # Register resource path
      resource_name <- paste0("gallery_", session$token)
      shiny::addResourcePath(resource_name, png_dir)

      # Build image cards
      cards <- lapply(seq_len(nrow(imgs)), function(i) {
        row <- imgs[i, ]
        file_name <- paste0(row$sample_name, "_",
                            sprintf("%05d", row$roi_number), ".png")
        img_id <- paste0(row$sample_name, "_", row$roi_number)
        is_selected <- img_id %in% rv$selected_images
        src <- paste0(resource_name, "/", row$sample_name, "/", file_name)

        border_style <- if (is_selected) {
          "border: 3px solid #007bff; background-color: #e7f1ff;"
        } else {
          "border: 1px solid #ddd;"
        }

        shiny::div(
          class = paste("image-card", if (is_selected) "selected"),
          `data-img` = img_id,
          style = paste0(
            "display: inline-block; margin: 4px; padding: 4px; ",
            "border-radius: 5px; cursor: pointer; ", border_style
          ),
          shiny::tags$img(
            src = src,
            style = "display: block; max-height: 120px;",
            onerror = paste0(
              "this.style.display='none';",
              "this.nextSibling.style.display='block';"
            )
          ),
          shiny::div(
            style = paste0(
              "width: 80px; height: 60px; background: #f0f0f0; ",
              "display: none; line-height: 60px; text-align: center; ",
              "font-size: 10px;"
            ),
            "Not found"
          ),
          shiny::div(
            style = "font-size: 9px; text-align: center; margin-top: 2px;",
            paste0("ROI ", row$roi_number),
            if (!is.null(row$score)) {
              shiny::span(
                style = "color: #999; margin-left: 4px;",
                paste0(round(row$score * 100, 1), "%")
              )
            }
          )
        )
      })

      shiny::div(
        style = "display: flex; flex-wrap: wrap; align-items: flex-start;",
        cards
      )
    })
  })
}
