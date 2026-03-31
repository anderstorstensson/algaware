#' Front Page Mosaic Module UI
#'
#' Interactive designer for the report front page mosaics. Users can select
#' taxa, preview individual images, re-roll bad images, and compose mosaics
#' for Baltic Sea and West Coast regions.
#'
#' @param id Module namespace ID.
#' @return A UI element.
#' @export
mod_frontpage_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 300,
        shiny::radioButtons(ns("region"), "Region",
          choices = c("Baltic Sea" = "EAST", "West Coast" = "WEST"),
          selected = "EAST"
        ),
        shiny::numericInput(ns("n_images"), "Number of images",
          value = 10L, min = 1L, max = 20L, step = 1L
        ),
        shiny::uiOutput(ns("taxa_selector")),
        shiny::actionButton(ns("generate"), "Generate Mosaic",
          class = "btn-primary w-100",
          icon = shiny::icon("shuffle")
        ),
        shiny::hr(),
        shiny::p(
          style = "font-size: 11px; color: #666;",
          "Select taxa and click Generate to create a mosaic. ",
          "Use the re-roll button next to any image to replace it."
        )
      ),
      shiny::div(
        shiny::uiOutput(ns("image_grid")),
        shiny::hr(),
        shiny::h5("Mosaic Preview"),
        shiny::uiOutput(ns("mosaic_status")),
        shiny::imageOutput(ns("mosaic_preview"), inline = TRUE)
      )
    )
  )
}

#' Front Page Mosaic Module Server
#'
#' @param id Module namespace ID.
#' @param rv Reactive values for app state.
#' @param config Reactive values with settings.
#' @return NULL (side effects only).
#' @export
mod_frontpage_server <- function(id, rv, config) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Session temp directory for extracted images
    fp_dir <- file.path(tempdir(), "algaware_frontpage")
    dir.create(fp_dir, recursive = TRUE, showWarnings = FALSE)
    shiny::addResourcePath("fp_imgs", fp_dir)
    session$onSessionEnded(function() {
      shiny::removeResourcePath("fp_imgs")
    })

    # Per-region state: list of image info objects keyed by taxon
    state <- shiny::reactiveValues(
      baltic_images = list(),
      westcoast_images = list(),
      # Track re-roll history to avoid repeats
      baltic_history = list(),
      westcoast_history = list()
    )

    # Current region's images as a reactive
    current_images <- shiny::reactive({
      if (input$region == "EAST") state$baltic_images else state$westcoast_images
    })

    # Compute top taxa from the current (possibly reclassified) classifications.
    # Uses image count as ranking metric - this is fast and always reflects
    # corrections made in the Validate tab.
    current_top_taxa <- shiny::reactive({
      shiny::req(rv$data_loaded, rv$classifications)
      region <- input$region
      samples <- if (region == "EAST") rv$baltic_samples else rv$westcoast_samples
      taxa_lookup <- merge_custom_taxa(rv$taxa_lookup, rv$custom_classes)

      region_class <- rv$classifications[
        rv$classifications$sample_name %in% samples,
      ]
      non_bio <- parse_non_bio_classes(config$non_biological_classes)
      region_class <- region_class[!region_class$class_name %in% non_bio, ]

      # Map class names to scientific names
      name_map <- stats::setNames(taxa_lookup$name, taxa_lookup$clean_names)
      sci_names <- name_map[region_class$class_name]
      sci_names <- sci_names[!is.na(sci_names)]

      if (length(sci_names) == 0) return(character(0))
      names(sort(table(sci_names), decreasing = TRUE))
    })

    # Update taxa selector when data, region, or classifications change
    output$taxa_selector <- shiny::renderUI({
      all_taxa <- current_top_taxa()
      shiny::req(length(all_taxa) > 0)

      default_n <- min(input$n_images, length(all_taxa))

      shiny::selectizeInput(ns("taxa"), "Taxa to include",
        choices = all_taxa,
        selected = utils::head(all_taxa, default_n),
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      )
    })

    # Generate mosaic images
    shiny::observeEvent(input$generate, {
      shiny::req(rv$data_loaded, input$taxa)

      region <- input$region
      taxa <- input$taxa
      samples <- if (region == "EAST") rv$baltic_samples else rv$westcoast_samples
      storage <- config$local_storage_path
      taxa_lookup <- merge_custom_taxa(rv$taxa_lookup, rv$custom_classes)

      shiny::withProgress(message = "Extracting images...", value = 0, {
        images <- list()
        for (i in seq_along(taxa)) {
          shiny::incProgress(1 / length(taxa), detail = taxa[i])
          img_info <- extract_random_taxon_image(
            taxa[i], rv$classifications, taxa_lookup, samples,
            file.path(storage, "raw"), fp_dir
          )
          if (!is.null(img_info)) {
            images[[taxa[i]]] <- img_info
          }
        }
      })

      if (region == "EAST") {
        state$baltic_images <- images
        state$baltic_history <- lapply(images, function(img) {
          data.frame(
            sample_name = img$sample_name,
            roi_number = img$roi_number,
            stringsAsFactors = FALSE
          )
        })
      } else {
        state$westcoast_images <- images
        state$westcoast_history <- lapply(images, function(img) {
          data.frame(
            sample_name = img$sample_name,
            roi_number = img$roi_number,
            stringsAsFactors = FALSE
          )
        })
      }

      compose_mosaic(region)
    })

    # Re-roll handler: triggered via Shiny.setInputValue from onclick
    shiny::observeEvent(input$reroll_taxon, {
      taxon <- input$reroll_taxon
      region <- input$region
      samples <- if (region == "EAST") rv$baltic_samples else rv$westcoast_samples
      storage <- config$local_storage_path
      taxa_lookup <- merge_custom_taxa(rv$taxa_lookup, rv$custom_classes)

      # Get history for this taxon to exclude already-seen ROIs
      history <- if (region == "EAST") {
        state$baltic_history[[taxon]]
      } else {
        state$westcoast_history[[taxon]]
      }

      img_info <- extract_random_taxon_image(
        taxon, rv$classifications, taxa_lookup, samples,
        file.path(storage, "raw"), fp_dir,
        exclude_rois = history
      )

      if (is.null(img_info)) {
        # If we exhausted all images, reset history and try again
        if (region == "EAST") {
          state$baltic_history[[taxon]] <- data.frame(
            sample_name = character(0), roi_number = integer(0),
            stringsAsFactors = FALSE
          )
        } else {
          state$westcoast_history[[taxon]] <- data.frame(
            sample_name = character(0), roi_number = integer(0),
            stringsAsFactors = FALSE
          )
        }
        img_info <- extract_random_taxon_image(
          taxon, rv$classifications, taxa_lookup, samples,
          file.path(storage, "raw"), fp_dir
        )
      }

      if (!is.null(img_info)) {
        if (region == "EAST") {
          imgs <- state$baltic_images
          imgs[[taxon]] <- img_info
          state$baltic_images <- imgs
          hist_df <- state$baltic_history[[taxon]]
          state$baltic_history[[taxon]] <- rbind(hist_df, data.frame(
            sample_name = img_info$sample_name,
            roi_number = img_info$roi_number,
            stringsAsFactors = FALSE
          ))
        } else {
          imgs <- state$westcoast_images
          imgs[[taxon]] <- img_info
          state$westcoast_images <- imgs
          hist_df <- state$westcoast_history[[taxon]]
          state$westcoast_history[[taxon]] <- rbind(hist_df, data.frame(
            sample_name = img_info$sample_name,
            roi_number = img_info$roi_number,
            stringsAsFactors = FALSE
          ))
        }
        compose_mosaic(region)
      } else {
        shiny::showNotification(
          paste0("No more images available for ", taxon),
          type = "warning"
        )
      }
    })

    # Compose mosaic from current images and store in rv
    compose_mosaic <- function(region) {
      images <- if (region == "EAST") {
        state$baltic_images
      } else {
        state$westcoast_images
      }
      if (length(images) == 0) return(NULL)

      paths <- vapply(images, function(img) img$path, character(1))
      paths <- paths[file.exists(paths)]
      if (length(paths) == 0) return(NULL)

      mosaic <- tryCatch(
        create_mosaic(paths, n_images = length(paths),
                      max_width_px = 1800L, target_height = 120L,
                      max_height_px = 1100L),
        error = function(e) NULL
      )

      if (region == "EAST") {
        rv$frontpage_baltic_mosaic <- mosaic
      } else {
        rv$frontpage_westcoast_mosaic <- mosaic
      }
    }

    # Render the image grid with re-roll buttons
    output$image_grid <- shiny::renderUI({
      images <- current_images()
      if (length(images) == 0) {
        return(shiny::div(
          class = "empty-state",
          style = "padding: 40px;",
          shiny::icon("image"),
          shiny::h4("No images generated yet"),
          shiny::p("Select taxa and click Generate to create a mosaic.")
        ))
      }

      taxa_names <- names(images)
      cards <- lapply(taxa_names, function(taxon) {
        img_info <- images[[taxon]]
        # Build a relative URL for the served resource
        rel_path <- sub(
          paste0("^", gsub("([\\[\\]{}()+*?^$.|\\\\])", "\\\\\\1", fp_dir), "/?"),
          "",
          img_info$path
        )
        img_url <- paste0("fp_imgs/", rel_path)

        shiny::div(
          style = paste0(
            "display: inline-block; vertical-align: top; margin: 6px; ",
            "border: 1px solid #ddd; border-radius: 6px; padding: 8px; ",
            "background: #fafafa; max-width: 220px; text-align: center;"
          ),
          shiny::div(
            style = "min-height: 80px; display: flex; align-items: center; justify-content: center;",
            shiny::tags$img(
              src = img_url,
              style = "max-width: 200px; max-height: 200px;",
              alt = taxon
            )
          ),
          shiny::div(
            style = "margin-top: 4px; font-size: 12px;",
            shiny::tags$em(taxon),
            shiny::tags$br(),
            shiny::tags$span(
              style = "color: #999; font-size: 10px;",
              paste0("(", img_info$n_available, " available)")
            )
          ),
          shiny::tags$button(
            type = "button",
            class = "btn btn-outline-secondary btn-sm mt-1",
            onclick = paste0(
              "Shiny.setInputValue('", ns("reroll_taxon"),
              "', '", gsub("'", "\\\\'", taxon),
              "', {priority: 'event'});"
            ),
            shiny::icon("arrows-rotate"),
            " Re-roll"
          )
        )
      })

      shiny::div(
        style = "display: flex; flex-wrap: wrap; gap: 4px;",
        cards
      )
    })

    # Mosaic status text
    output$mosaic_status <- shiny::renderUI({
      region_label <- if (input$region == "EAST") "Baltic Sea" else "West Coast"
      mosaic <- if (input$region == "EAST") {
        rv$frontpage_baltic_mosaic
      } else {
        rv$frontpage_westcoast_mosaic
      }

      baltic_ready <- !is.null(rv$frontpage_baltic_mosaic)
      west_ready <- !is.null(rv$frontpage_westcoast_mosaic)

      shiny::div(
        style = "font-size: 12px; margin-bottom: 8px;",
        if (baltic_ready) {
          shiny::span(
            style = "color: green; margin-right: 12px;",
            shiny::icon("check"), " Baltic Sea ready"
          )
        } else {
          shiny::span(
            style = "color: #999; margin-right: 12px;",
            shiny::icon("circle"), " Baltic Sea not set"
          )
        },
        if (west_ready) {
          shiny::span(
            style = "color: green;",
            shiny::icon("check"), " West Coast ready"
          )
        } else {
          shiny::span(
            style = "color: #999;",
            shiny::icon("circle"), " West Coast not set"
          )
        }
      )
    })

    # Render mosaic preview
    output$mosaic_preview <- shiny::renderImage({
      mosaic <- if (input$region == "EAST") {
        rv$frontpage_baltic_mosaic
      } else {
        rv$frontpage_westcoast_mosaic
      }
      shiny::req(mosaic)

      f <- tempfile(fileext = ".png")
      magick::image_write(mosaic, f)

      info <- magick::image_info(mosaic)
      # Scale for display: max 700px wide
      display_width <- min(700, info$width)
      display_height <- as.integer(display_width * info$height / info$width)

      list(
        src = f,
        width = display_width,
        height = display_height,
        alt = "Mosaic preview",
        deleteFile = TRUE
      )
    }, deleteFile = TRUE)
  })
}
