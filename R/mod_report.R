#' Report Module UI
#'
#' @param id Module namespace ID.
#' @return A UI element.
#' @export
mod_report_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "report-section",
    shiny::uiOutput(ns("report_readiness")),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::textInput(ns("report_number"), "Report No",
                         placeholder = "e.g. 1")
      ),
      shiny::column(
        width = 6,
        shiny::textInput(ns("report_dnr"), "Dnr",
                         placeholder = "e.g. 2026/718/2.1.3")
      )
    ),
    shiny::uiOutput(ns("llm_status")),
    bslib::tooltip(
      bslib::input_task_button(
        ns("make_report"), "Make Report",
        icon = shiny::icon("file-word"),
        label_busy = "Generating...",
        class = "btn-primary mb-1 w-100"
      ),
      "Generates a Word report. CTD data and mosaics are included only if loaded/created first."
    ),
    shiny::downloadButton(ns("download_report"), "Download Report",
                          class = "btn-outline-primary mt-1 mb-1"),
    shiny::downloadButton(ns("download_corrections"), "Download Corrections",
                          class = "btn-outline-secondary mt-1"),
    shiny::uiOutput(ns("report_status"))
  )
}

#' Build the report readiness item list
#'
#' @param data_loaded Logical, whether IFCB data has been loaded.
#' @param ctd_loaded Logical, whether CTD data has been loaded.
#' @param baltic_mosaic Front-page Baltic mosaic or \code{NULL}.
#' @param westcoast_mosaic Front-page West Coast mosaic or \code{NULL}.
#' @return List of items with \code{ok} (logical) and \code{label} (character).
#' @keywords internal
build_readiness_items <- function(data_loaded, ctd_loaded,
                                  baltic_mosaic, westcoast_mosaic) {
  list(
    list(ok = isTRUE(data_loaded),       label = "IFCB data loaded"),
    list(ok = isTRUE(ctd_loaded),        label = "CTD data (load in CTD tab)"),
    list(ok = !is.null(baltic_mosaic),   label = "Baltic mosaic (Images tab)"),
    list(ok = !is.null(westcoast_mosaic),label = "West Coast mosaic (Images tab)")
  )
}

#' Report Module Server
#'
#' @param id Module namespace ID.
#' @param rv Reactive values for app state.
#' @param config Reactive values with settings.
#' @param phyto_groups_reactive Optional reactive returning a data frame of
#'   phytoplankton group assignments (\code{name}, \code{AphiaID},
#'   \code{phyto_group}). When supplied, the cached value is forwarded to
#'   \code{generate_report()} so the WoRMS lookup is not repeated.
#' @return NULL (side effects only).
#' @export
mod_report_server <- function(id, rv, config, phyto_groups_reactive = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    report_path <- shiny::reactiveVal(NULL)
    corrections_path <- shiny::reactiveVal(NULL)

    output$report_readiness <- shiny::renderUI({
      items <- build_readiness_items(
        rv$data_loaded, rv$ctd_loaded,
        rv$frontpage_baltic_mosaic, rv$frontpage_westcoast_mosaic
      )
      shiny::tagList(
        shiny::tags$p(class = "small fw-semibold mb-1", "Report contents:"),
        lapply(items, function(x) {
          shiny::div(
            class = "d-flex align-items-center gap-1",
            style = paste0("font-size: 12px; color: ",
                           if (x$ok) "green" else "#aaa", ";"),
            shiny::icon(if (x$ok) "circle-check" else "circle"),
            x$label
          )
        }),
        shiny::hr(style = "margin: 8px 0;")
      )
    })

    # Initialize Dnr field from persisted settings.
    shiny::observeEvent(config$report_dnr, {
      shiny::updateTextInput(session, "report_dnr",
                             value = config$report_dnr %||% "")
    }, ignoreInit = FALSE)

    # Persist Dnr changes between sessions.
    shiny::observeEvent(input$report_dnr, {
      new_dnr <- trimws(input$report_dnr %||% "")
      old_dnr <- trimws(config$report_dnr %||% "")
      if (!identical(new_dnr, old_dnr)) {
        config$report_dnr <- new_dnr
        save_settings(shiny::reactiveValuesToList(config))
      }
    }, ignoreInit = TRUE)

    output$llm_status <- shiny::renderUI({
      providers <- llm_providers()
      if (length(providers) > 0) {
        use_llm_checked <- isTRUE(input$use_llm %||% TRUE)
        provider_labels <- c(
          openai = paste0("OpenAI (", llm_model_name("openai"), ")"),
          gemini = paste0("Gemini (", llm_model_name("gemini"), ")")
        )
        choices <- stats::setNames(providers, provider_labels[providers])
        provider_ui <- if (length(providers) > 1) {
          selected_provider <- input$llm_provider %||% providers[1]
          control <- shiny::selectInput(ns("llm_provider"), "LLM Provider",
                             choices = choices, selected = selected_provider,
                             width = "220px")
          if (!use_llm_checked) {
            control <- shiny::tags$div(
              style = "opacity: 0.55; pointer-events: none;",
              control
            )
          }
          control
        } else {
          selected_provider <- providers[1]
          provider_text <- paste0(" ", provider_labels[selected_provider],
                                  " API key detected")
          if (!use_llm_checked) {
            provider_text <- paste0(provider_text, " (disabled)")
          }
          shiny::div(
            class = "llm-status available",
            style = if (!use_llm_checked) "opacity: 0.55;" else NULL,
            shiny::icon("robot"),
            provider_text
          )
        }
        shiny::tagList(
          shiny::checkboxInput(ns("use_llm"), "AI text generation",
                               value = use_llm_checked),
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

      # Invalidate any previously generated report up front: if this run
      # fails, the status text and download button must not keep serving the
      # stale document as "ready" (a user could otherwise download a report
      # that lacks their latest corrections).
      report_path(NULL)
      corrections_path(NULL)

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

          # Create per-class mosaics (optional, off by default)
          baltic_mosaics <- list()
          westcoast_mosaics <- list()
          if (isTRUE(config$include_class_mosaics)) {
            shiny::incProgress(0.3, detail = "Creating mosaics...")

            baltic_mosaics <- suppressWarnings(
              create_region_mosaics(
                baltic_wide, rv$classifications, rv$baltic_samples,
                file.path(storage, "raw"), taxa_lookup,
                n_taxa = config$n_mosaic_taxa,
                n_images = config$n_mosaic_images,
                scale_micron_factor = 1 / config$pixels_per_micron
              )
            )

            westcoast_mosaics <- suppressWarnings(
              create_region_mosaics(
                westcoast_wide, rv$classifications, rv$westcoast_samples,
                file.path(storage, "raw"), taxa_lookup,
                n_taxa = config$n_mosaic_taxa,
                n_images = config$n_mosaic_images,
                scale_micron_factor = 1 / config$pixels_per_micron
              )
            )
          }

          # Auto-generate frontpage mosaics if not manually created
          shiny::incProgress(0.1, detail = "Preparing front page...")

          fp_baltic_mosaic <- rv$frontpage_baltic_mosaic
          fp_baltic_taxa <- rv$frontpage_baltic_taxa
          fp_westcoast_mosaic <- rv$frontpage_westcoast_mosaic
          fp_westcoast_taxa <- rv$frontpage_westcoast_taxa

          if (is.null(fp_baltic_mosaic) && length(rv$baltic_samples) > 0) {
            result <- suppressWarnings(
              generate_frontpage_mosaic(
                rv$classifications, taxa_lookup, rv$baltic_samples,
                file.path(storage, "raw"), non_bio,
                wide_summary = baltic_wide,
                scale_micron_factor = 1 / config$pixels_per_micron
              )
            )
            if (!is.null(result)) {
              fp_baltic_mosaic <- result$mosaic
              fp_baltic_taxa <- result$taxa
            }
          }

          if (is.null(fp_westcoast_mosaic) && length(rv$westcoast_samples) > 0) {
            result <- suppressWarnings(
              generate_frontpage_mosaic(
                rv$classifications, taxa_lookup, rv$westcoast_samples,
                file.path(storage, "raw"), non_bio,
                wide_summary = westcoast_wide,
                scale_micron_factor = 1 / config$pixels_per_micron
              )
            )
            if (!is.null(result)) {
              fp_westcoast_mosaic <- result$mosaic
              fp_westcoast_taxa <- result$taxa
            }
          }

          # Generate report
          shiny::incProgress(0.2, detail = "Building Word document...")

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

          # Compute unclassified fractions for LLM context
          unclassified_fractions <- compute_unclassified_fractions(
            rv$classifications, rv$matched_metadata
          )

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
            on_llm_progress = llm_progress,
            frontpage_baltic_mosaic = fp_baltic_mosaic,
            frontpage_westcoast_mosaic = fp_westcoast_mosaic,
            unclassified_fractions = unclassified_fractions,
            frontpage_baltic_taxa = fp_baltic_taxa,
            frontpage_westcoast_taxa = fp_westcoast_taxa,
            report_number = input$report_number,
            report_dnr = input$report_dnr,
            ctd_data = rv$ctd_data,
            ctd_data_full = rv$ctd_data_full,
            lims_data = rv$lims_data,
            lims_data_full = rv$lims_data_full,
            chl_stats = rv$chl_stats,
            chl_map_source = rv$chl_map_source %||% "ferrybox",
            phyto_groups = if (!is.null(phyto_groups_reactive)) {
              tryCatch(phyto_groups_reactive(), error = function(e) NULL)
            } else {
              NULL
            }
          )

          report_path(out_file)

          # Export corrections log if any corrections were made
          if (nrow(rv$corrections) > 0) {
            csv_file <- sub("\\.docx$", "_corrections.csv", out_file)
            corrections_export <- enrich_corrections_for_export(
              rv$corrections, rv$custom_classes
            )
            utils::write.csv(corrections_export, csv_file, row.names = FALSE,
                             fileEncoding = "UTF-8")
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
        report_no <- trimws(input$report_number %||% "")
        suffix <- if (nzchar(report_no)) report_no else "X"
        paste0("Algaware_", suffix, ".docx")
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
        # Build the corrections CSV on demand from the session log so it can be
        # downloaded any time after corrections are made, without first
        # generating the Word report.
        shiny::req(nrow(rv$corrections) > 0)
        corrections_export <- enrich_corrections_for_export(
          rv$corrections, rv$custom_classes
        )
        utils::write.csv(corrections_export, file, row.names = FALSE,
                         fileEncoding = "UTF-8")
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
