#' Match a selectize change event against the pending programmatic-update queue
#'
#' The gallery keeps the class selectize in sync with the current class index by
#' calling \code{updateSelectizeInput()}. Each such call echoes back as an
#' \code{input$class_select} change event. To stop those echoes from feeding
#' back into the index (which could ping-pong between two classes), every pushed
#' value is recorded in a queue and the matching echo is swallowed here.
#'
#' The queue is drained through the \emph{last} matching entry, not just the
#' head: rapid navigation can make Shiny coalesce several programmatic updates
#' and only echo the final value, so intermediate entries never return.
#'
#' @param queue Character vector of values pushed to the widget, oldest first.
#' @param sel The incoming \code{input$class_select} value (length-1 character,
#'   possibly \code{NA}).
#' @return A list with \code{is_echo} (logical) and \code{queue} (the queue with
#'   consumed entries removed when \code{is_echo} is \code{TRUE}).
#' @keywords internal
match_pending_echo <- function(queue, sel) {
  if (length(sel) == 0 || is.null(sel)) sel <- NA_character_
  hit <- if (is.na(sel)) {
    which(is.na(queue))
  } else {
    which(!is.na(queue) & queue == sel)
  }
  if (length(hit) > 0) {
    list(is_echo = TRUE, queue = queue[-seq_len(max(hit))])
  } else {
    list(is_echo = FALSE, queue = queue)
  }
}

#' Gallery Module UI
#'
#' @param id Module namespace ID.
#' @return A UI element.
#' @export
mod_gallery_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # Toolbar
    shiny::div(
      class = "d-flex align-items-start gap-2 mb-3 p-2 bg-light rounded toolbar-row",

      # Region toggle
      shiny::radioButtons(ns("region_toggle"), NULL,
                          choices = c("Baltic Sea" = "EAST",
                                      "West Coast" = "WEST"),
                          selected = "EAST", inline = FALSE),

      shiny::div(class = "vr mx-1", style = "align-self: stretch;"),

      # Class navigation
      shiny::actionButton(ns("prev_class"), "",
                          icon = shiny::icon("arrow-left"),
                          class = "btn-outline-secondary btn-sm",
                          style = "position: relative; z-index: 10;"),
      shiny::div(
        id = "toolbar-class-container",
        class = "toolbar-select toolbar-class-select",
        style = "width: 390px; min-width: 390px; max-width: 390px; text-align: center;",
        shiny::selectizeInput(ns("class_select"), NULL, choices = NULL,
                              width = "100%",
                              options = list(
                                placeholder = "Select class...",
                                maxOptions = 500,
                                dropdownParent = "body"
                              )),
        shiny::uiOutput(ns("class_header"))
      ),
      shiny::actionButton(ns("next_class"), "",
                          icon = shiny::icon("arrow-right"),
                          class = "btn-outline-secondary btn-sm",
                          style = "position: relative; z-index: 10;"),

      shiny::div(class = "vr mx-1", style = "align-self: stretch;"),

      # Selection buttons
      shiny::actionButton(ns("select_page"), "Select Page",
                          class = "btn-outline-secondary btn-sm"),
      shiny::actionButton(ns("deselect"), "Deselect",
                          class = "btn-outline-secondary btn-sm"),

      # Measure tool
      shiny::actionButton(ns("measure_toggle"), label = shiny::icon("ruler"),
                          class = "btn-outline-secondary btn-sm",
                          title = "Measure: click and drag on images"),

      shiny::div(class = "vr mx-1", style = "align-self: stretch;"),

      # Page size
      shiny::div(
        class = "toolbar-select",
        shiny::selectInput(ns("page_size"), NULL,
                           choices = c("50", "100", "200"),
                           selected = "100", width = "80px")
      ),

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

#' Extract missing PNGs for gallery display
#'
#' Checks which ROIs are already extracted and only extracts missing ones.
#'
#' @param imgs Data.frame with sample_name and roi_number columns.
#' @param raw_dir Path to raw data directory.
#' @param png_dir Path to PNG output directory.
#' @return Invisible NULL.
#' @keywords internal
extract_gallery_pngs <- function(imgs, raw_dir, png_dir) {
  for (samp in unique(imgs$sample_name)) {
    samp_dir <- file.path(png_dir, samp)
    samp_rois <- imgs$roi_number[imgs$sample_name == samp]

    expected_pngs <- paste0(samp, "_",
                            sprintf("%05d", samp_rois), ".png")
    existing_pngs <- if (dir.exists(samp_dir)) {
      list.files(samp_dir)
    } else {
      character(0)
    }
    missing_rois <- samp_rois[!expected_pngs %in% existing_pngs]

    if (length(missing_rois) == 0) next

    roi_file <- list.files(raw_dir,
                           pattern = paste0(samp, "\\.roi$"),
                           recursive = TRUE, full.names = TRUE)
    if (length(roi_file) > 0) {
      tryCatch(
        iRfcb::ifcb_extract_pngs(roi_file[1], png_dir,
                                   ROInumbers = missing_rois,
                                   verbose = FALSE),
        error = function(e) NULL
      )
    }
  }
  invisible(NULL)
}

#' Paginate a data frame of images
#'
#' @param imgs Data frame of images.
#' @param page Integer page number (1-based).
#' @param page_size Number of images per page.
#' @return Row-subset of \code{imgs} for the requested page.
#' @keywords internal
paginate_images <- function(imgs, page, page_size) {
  total_pages <- ceiling(nrow(imgs) / page_size)
  p <- min(page, max(1L, total_pages))
  start <- (p - 1L) * page_size + 1L
  end <- min(p * page_size, nrow(imgs))
  imgs[start:end, , drop = FALSE]
}

#' Gallery Module Server
#'
#' @param id Module namespace ID.
#' @param rv Reactive values for app state.
#' @param config Reactive values with settings.
#' @return NULL (side effects only).
#' @export
mod_gallery_server <- function(id, rv, config) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    page <- shiny::reactiveVal(1L)

    # Outstanding values pushed to the class selectize by the sync observer
    # below. Every programmatic updateSelectizeInput() echoes back as an
    # input$class_select change event; we record each pushed value here and
    # swallow the matching echo so it never feeds back into current_class_idx.
    # A queue (not a single value or boolean) is required because rapid
    # navigation can leave several programmatic echoes in flight at once —
    # the previous single-guard logic let stale echoes through, which made the
    # selectize and the index correct each other forever, ping-ponging between
    # two classes after the class list changed (e.g. relabelling a whole class).
    pending_selections <- shiny::reactiveVal(character(0))

    # Temp directory for extracted PNG images. Each browser session gets its
    # own folder (keyed by session$token) so concurrent users don't collide.
    # Cleaned up automatically when the session ends.
    gallery_dir <- file.path(tempdir(), "algaware_gallery", session$token)
    resource_registered <- FALSE

    session$onSessionEnded(function() {
      unlink(gallery_dir, recursive = TRUE)
    })

    # Current region's class list
    region_classes <- shiny::reactive({
      shiny::req(rv$data_loaded, rv$classifications)
      region <- input$region_toggle

      region_samples <- if (region == "EAST") rv$baltic_samples else rv$westcoast_samples
      region_data <- rv$classifications[
        rv$classifications$sample_name %in% region_samples,
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
      paginate_images(imgs, page(), as.integer(input$page_size))
    })

    # ---- PNG extraction ----
    # Separated into its own reactive so it only re-runs when the *page data*
    # changes, not when the user clicks images (selection is handled in JS).
    gallery_data <- shiny::reactive({
      imgs <- tryCatch(paginated(), error = function(e) NULL)
      if (is.null(imgs) || nrow(imgs) == 0) return(NULL)

      storage <- config$local_storage_path
      raw_dir <- file.path(storage, "raw")
      extract_gallery_pngs(imgs, raw_dir, gallery_dir)

      # Register resource path once per session
      if (!resource_registered) {
        resource_name <- paste0("gallery_", session$token)
        shiny::addResourcePath(resource_name, gallery_dir)
        resource_registered <<- TRUE
      }

      imgs
    })

    # ---- Class dropdown synchronization ----

    # Currently displayed class name, derived from the index (single source of
    # truth). Clamped to a valid position so a shrinking class list can never
    # produce an out-of-range or zero index.
    displayed_class <- shiny::reactive({
      classes <- region_classes()
      if (length(classes) == 0) return(NULL)
      idx <- max(1L, min(rv$current_class_idx, length(classes)))
      classes[idx]
    })

    # Keep the selectize dropdown in sync with the current class (state ->
    # widget, one-way). Only push when the selection actually changes, so each
    # programmatic update yields exactly one echo to swallow below.
    shiny::observe({
      classes <- region_classes()
      selected <- displayed_class()

      current_value <- shiny::isolate(input$class_select)
      if (!identical(selected, current_value)) {
        pending_selections(c(shiny::isolate(pending_selections()),
                             selected %||% NA_character_))
        shiny::updateSelectizeInput(session, "class_select",
                                    choices = classes,
                                    selected = selected)
      }

      # Tell the browser to adjust toolbar height (JS handler in gallery.js).
      # sendCustomMessage() calls a JavaScript function registered via
      # Shiny.addCustomMessageHandler() in inst/app/www/gallery.js.
      has_data <- if (length(classes) > 0) "add" else "remove"
      session$sendCustomMessage("toggle-toolbar-height", has_data)
    })

    # Jump to class when the user picks from the selectize (widget -> state).
    # Echoes from the programmatic updates above are swallowed by matching them
    # against the pending queue, so they never write back to current_class_idx.
    # We drop the queue through the *last* matching entry rather than just the
    # head: when navigation is rapid, Shiny may coalesce several programmatic
    # updates and only echo the final value, so intermediate entries never come
    # back. Clearing through the last match keeps the queue from getting stuck.
    shiny::observeEvent(input$class_select, {
      sel <- input$class_select %||% NA_character_
      echo <- match_pending_echo(pending_selections(), sel)
      if (echo$is_echo) {
        pending_selections(echo$queue)  # our own update echoing back: ignore
        return()
      }

      classes <- region_classes()
      if (length(classes) == 0) return()
      new_idx <- match(sel, classes)
      if (!is.na(new_idx) && new_idx != rv$current_class_idx) {
        rv$current_class_idx <- new_idx
        page(1L)
      }
    })

    # Class header (detail info below selectize)
    output$class_header <- shiny::renderUI({
      classes <- region_classes()
      if (length(classes) == 0) return(shiny::p("No classes"))

      idx <- min(rv$current_class_idx, length(classes))
      current_class <- classes[idx]
      total_imgs <- nrow(current_images())

      # Check if non-biological
      non_bio <- parse_non_bio_classes(config$non_biological_classes)
      is_non_bio <- current_class %in% non_bio

      # Map class_name to scientific display name (name + sflag)
      taxa <- rv$taxa_lookup
      sci_name <- current_class
      if (!is.null(taxa)) {
        match_idx <- match(current_class, taxa$clean_names)
        if (!is.na(match_idx)) {
          sflag_val <- if ("sflag" %in% names(taxa)) taxa$sflag[match_idx] else ""
          if (is.na(sflag_val)) sflag_val <- ""
          sci_name <- trimws(paste(taxa$name[match_idx], sflag_val))
        }
      }

      # Check HAB status (hab_species returns display names)
      hab_species <- get_hab_species(rv$taxa_lookup)
      is_hab <- !is_non_bio && sci_name %in% hab_species

      # Determine display name (scientific name for biological classes)
      name_display <- NULL
      if (!is_non_bio) {
        show_sci <- !is.null(taxa) && !is.na(match(current_class, taxa$clean_names))
        if (show_sci) {
          name_display <- shiny::span(class = "sci-name", sci_name)
        }
      }

      # Check if class is in the database class list
      not_in_db <- length(rv$class_list) > 0 &&
        !current_class %in% rv$class_list

      shiny::div(
        class = "class-header",
        if (is_hab) shiny::span(class = "badge-hab", "HAB"),
        if (is_non_bio) shiny::span(class = "badge-nonbio", "Non-biological"),
        if (not_in_db) shiny::span(
          class = "badge bg-secondary",
          style = "font-size: 0.75em; margin-right: 4px;",
          "Not in DB"
        ),
        if (!is.null(name_display)) name_display,
        shiny::br(),
        shiny::span(
          class = "class-meta",
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
      shiny::span(class = "page-info", paste0(page(), "/", total_pages))
    })

    # ---- Class and page navigation ----
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

    # ---- Image selection ----
    # Selection works via a JS/R split: clicking an image in the browser sends
    # a message to R (input$toggle_image), and R updates rv$selected_images.
    # The visual highlight (CSS class .selected) is toggled client-side in JS
    # for instant feedback without a server round-trip.
    #
    # rv$selected_images is the single source of truth. Every change to it --
    # including clears done by mod_validation after relabel/store-annotations --
    # is mirrored to the browser below, and gallery.js re-applies the mirrored
    # state after each gallery re-render (renderUI replaces the DOM, which
    # would otherwise wipe the .selected highlights while the images remained
    # selected server-side).
    shiny::observeEvent(rv$selected_images, {
      session$sendCustomMessage("syncSelection",
                                list(selected = as.list(rv$selected_images)))
    }, ignoreNULL = FALSE)

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

    # Select all images on current page. The browser highlight follows via
    # the rv$selected_images -> syncSelection mirror above.
    shiny::observeEvent(input$select_page, {
      imgs <- tryCatch(paginated(), error = function(e) NULL)
      if (is.null(imgs) || nrow(imgs) == 0) return()
      img_ids <- paste0(imgs$sample_name, "_", imgs$roi_number)
      rv$selected_images <- union(rv$selected_images, img_ids)
    })

    # Clear all selections
    shiny::observeEvent(input$deselect, {
      rv$selected_images <- character(0)
    })

    # ---- Measure tool (ruler overlay, handled in gallery.js) ----
    measure_active <- shiny::reactiveVal(FALSE)

    shiny::observeEvent(input$measure_toggle, {
      active <- !measure_active()
      measure_active(active)
      session$sendCustomMessage("measureMode", active)
      session$sendCustomMessage("toggleMeasureBtn",
        list(id = ns("measure_toggle"), active = active))
    })

    # Send pixels_per_micron on session start
    shiny::observe({
      session$sendCustomMessage("updatePixelsPerMicron",
                                config$pixels_per_micron)
    })

    # ---- Gallery rendering ----
    # This only depends on gallery_data() (paginated images), NOT on
    # rv$selected_images. Selection highlighting is managed entirely in JS
    # so selecting/deselecting images doesn't cause a full re-render.
    output$image_gallery <- shiny::renderUI({
      imgs <- gallery_data()
      if (is.null(imgs) || nrow(imgs) == 0) {
        return(shiny::div(
          class = "text-center text-muted p-5",
          shiny::h4("No images to display"),
          shiny::p("Load data or select a different class/region.")
        ))
      }

      resource_name <- paste0("gallery_", session$token)

      # Build station name lookup from matched metadata
      station_lookup <- stats::setNames(
        rv$matched_metadata$STATION_NAME,
        rv$matched_metadata$pid
      )

      # Build image cards (no selection dependency -- JS handles .selected)
      cards <- lapply(seq_len(nrow(imgs)), function(i) {
        row <- imgs[i, ]
        file_name <- paste0(row$sample_name, "_",
                            sprintf("%05d", row$roi_number), ".png")
        img_id <- paste0(row$sample_name, "_", row$roi_number)
        src <- paste0(resource_name, "/", row$sample_name, "/", file_name)
        station <- unname(station_lookup[row$sample_name])
        if (is.na(station)) station <- ""

        shiny::div(
          class = "image-card",
          `data-img` = img_id,
          shiny::tags$img(
            src = src,
            alt = paste("ROI", row$roi_number, "from", row$sample_name)
          ),
          shiny::div(
            class = "image-placeholder",
            "Not found"
          ),
          shiny::div(
            class = "image-label",
            station,
            if (!is.null(row$score)) {
              shiny::span(
                class = "confidence",
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
