#' Report Module UI
#'
#' @param id Module namespace ID.
#' @return A UI element.
#' @export
mod_report_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "report-section",
    shiny::hr(),
    shiny::h5("Report Generation"),
    shiny::uiOutput(ns("llm_status")),
    shiny::actionButton(ns("make_report"), "Make Report",
                        class = "btn-primary mb-1",
                        icon = shiny::icon("file-word")),
    shiny::downloadButton(ns("download_report"), "Download Report",
                          class = "btn-outline-primary mt-1 mb-1"),
    shiny::downloadButton(ns("download_corrections"), "Download Corrections",
                          class = "btn-outline-secondary mt-1"),
    shiny::uiOutput(ns("report_status"))
  )
}

#' Report Module Server
#'
#' @param id Module namespace ID.
#' @param rv Reactive values for app state.
#' @param config Reactive values with settings.
#' @return NULL (side effects only).
#' @export
mod_report_server <- function(id, rv, config) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    report_path <- shiny::reactiveVal(NULL)
    corrections_path <- shiny::reactiveVal(NULL)

    output$llm_status <- shiny::renderUI({
      providers <- llm_providers()
      if (length(providers) > 0) {
        provider_labels <- c(
          openai = paste0("OpenAI (", llm_model_name("openai"), ")"),
          gemini = paste0("Gemini (", llm_model_name("gemini"), ")")
        )
        choices <- stats::setNames(providers, provider_labels[providers])
        provider_ui <- if (length(providers) > 1) {
          shiny::selectInput(ns("llm_provider"), "LLM Provider",
                             choices = choices, selected = providers[1],
                             width = "220px")
        } else {
          shiny::div(
            class = "llm-status available",
            shiny::icon("robot"),
            paste0(" ", provider_labels[providers], " API key detected")
          )
        }
        shiny::tagList(
          shiny::checkboxInput(ns("use_llm"), "AI text generation",
                               value = TRUE),
          provider_ui
        )
      } else {
        shiny::div(
          class = "llm-status unavailable",
          shiny::icon("pencil"),
          " Manual text mode (set OPENAI_API_KEY or GEMINI_API_KEY for AI text)"
        )
      }
    })

    shiny::observeEvent(input$make_report, {
      shiny::req(rv$data_loaded, rv$station_summary)

      shiny::withProgress(message = "Generating report...", value = 0, {
        tryCatch({
          # Recompute summaries using corrected classifications
          shiny::incProgress(0.1, detail = "Reprocessing with corrections...")

          non_bio <- parse_non_bio_classes(config$non_biological_classes)
          taxa_lookup <- merge_custom_taxa(rv$taxa_lookup, rv$custom_classes)
          storage <- config$local_storage_path

          biovolume_data <- summarize_biovolumes(
            file.path(storage, "features"),
            file.path(storage, "raw"),
            rv$classifications, taxa_lookup, non_bio,
            pixels_per_micron = config$pixels_per_micron,
            custom_classes = rv$custom_classes
          )

          station_summary <- aggregate_station_data(
            biovolume_data, rv$matched_metadata
          )

          # Re-attach ferrybox chlorophyll data
          if (!is.null(rv$ferrybox_chl)) {
            station_summary <- merge(station_summary, rv$ferrybox_chl,
                                     by = "STATION_NAME", all.x = TRUE)
          }

          baltic_wide <- create_wide_summary(station_summary, "EAST")
          westcoast_wide <- create_wide_summary(station_summary, "WEST")

          # Create mosaics
          shiny::incProgress(0.3, detail = "Creating mosaics...")

          baltic_mosaics <- create_region_mosaics(
            baltic_wide, rv$classifications, rv$baltic_samples,
            file.path(storage, "raw"), taxa_lookup,
            n_taxa = config$n_mosaic_taxa,
            n_images = config$n_mosaic_images
          )

          westcoast_mosaics <- create_region_mosaics(
            westcoast_wide, rv$classifications, rv$westcoast_samples,
            file.path(storage, "raw"), taxa_lookup,
            n_taxa = config$n_mosaic_taxa,
            n_images = config$n_mosaic_images
          )

          # Generate report
          shiny::incProgress(0.3, detail = "Building Word document...")

          out_file <- file.path(tempdir(),
                                paste0("algaware_report_",
                                       format(Sys.time(), "%Y%m%d_%H%M%S"),
                                       ".docx"))

          use_llm <- llm_available() && isTRUE(input$use_llm)
          selected_provider <- if (use_llm) {
            input$llm_provider %||% llm_provider()
          } else {
            NULL
          }

          if (!use_llm) {
            shiny::incProgress(0, detail = "Building Word document...")
          }

          total_bio <- sum(!rv$classifications$class_name %in% non_bio)
          n_stn_samples <- length(unique(rv$matched_metadata$pid))

          llm_progress <- if (use_llm) {
            function(step, total, detail) {
              shiny::incProgress(
                0,
                detail = sprintf("AI text %d/%d: %s", step, total, detail)
              )
            }
          } else {
            NULL
          }

          generate_report(
            out_file, station_summary,
            baltic_wide, westcoast_wide,
            baltic_mosaics, westcoast_mosaics,
            taxa_lookup = taxa_lookup,
            cruise_info = rv$cruise_info,
            classifier_name = rv$classifier_name,
            use_llm = use_llm,
            annotator = config$annotator,
            image_counts = rv$image_counts,
            total_bio_images = total_bio,
            llm_model = if (use_llm) llm_model_name(selected_provider) else NULL,
            n_station_samples = n_stn_samples,
            llm_provider = selected_provider,
            on_llm_progress = llm_progress
          )

          report_path(out_file)

          # Export corrections log if any corrections were made
          if (nrow(rv$corrections) > 0) {
            csv_file <- sub("\\.docx$", "_corrections.csv", out_file)
            utils::write.csv(rv$corrections, csv_file, row.names = FALSE)
            corrections_path(csv_file)
          } else {
            corrections_path(NULL)
          }

          shiny::incProgress(0.3, detail = "Done!")

          shiny::showNotification("Report generated successfully!",
                                  type = "message")
        }, error = function(e) {
          shiny::showNotification(
            paste0("Report error: ", e$message),
            type = "error", duration = 10
          )
        })
      })
    })

    output$download_report <- shiny::downloadHandler(
      filename = function() {
        paste0("algaware_report_", format(Sys.Date(), "%Y%m%d"), ".docx")
      },
      content = function(file) {
        rp <- report_path()
        shiny::req(rp)
        file.copy(rp, file)
      }
    )

    output$download_corrections <- shiny::downloadHandler(
      filename = function() {
        paste0("algaware_corrections_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        cp <- corrections_path()
        shiny::req(cp)
        file.copy(cp, file)
      }
    )

    output$report_status <- shiny::renderUI({
      rp <- report_path()
      if (is.null(rp)) return(NULL)
      cp <- corrections_path()
      shiny::div(
        style = "font-size: 12px; color: green; margin-top: 8px;",
        shiny::p(shiny::icon("check"), " Report ready for download."),
        if (!is.null(cp)) {
          shiny::p(
            shiny::icon("check"),
            paste0(" Corrections log (", nrow(rv$corrections), " changes)")
          )
        }
      )
    })
  })
}
