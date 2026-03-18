#' Report Module UI
#'
#' @param id Module namespace ID.
#' @return A UI element.
#' @keywords internal
mod_report_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::hr(),
    shiny::h5("Report Generation"),
    shiny::actionButton(ns("make_report"), "Make Report",
                        class = "btn-primary",
                        icon = shiny::icon("file-word")),
    shiny::downloadButton(ns("download_report"), "Download Report",
                          class = "btn-outline-primary mt-2"),
    shiny::uiOutput(ns("report_status"))
  )
}

#' Report Module Server
#'
#' @param id Module namespace ID.
#' @param rv Reactive values for app state.
#' @param config Reactive values with settings.
#' @return NULL (side effects only).
#' @keywords internal
mod_report_server <- function(id, rv, config) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    report_path <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$make_report, {
      shiny::req(rv$data_loaded, rv$station_summary)

      shiny::withProgress(message = "Generating report...", value = 0, {
        tryCatch({
          # Recompute summaries with invalidated classes applied
          shiny::incProgress(0.1, detail = "Reprocessing with validations...")

          non_bio <- parse_non_bio_classes(config$non_biological_classes)
          all_excluded <- unique(c(non_bio, rv$invalidated_classes))
          clean_class <- apply_invalidation(rv$classifications_raw,
                                            all_excluded)

          taxa_lookup <- rv$taxa_lookup
          storage <- config$local_storage_path

          biovolume_data <- summarize_biovolumes(
            file.path(storage, "features"),
            file.path(storage, "raw"),
            clean_class, taxa_lookup, all_excluded
          )

          station_summary <- aggregate_station_data(
            biovolume_data, rv$matched_metadata
          )

          baltic_wide <- create_wide_summary(station_summary, "EAST")
          westcoast_wide <- create_wide_summary(station_summary, "WEST")

          # Create mosaics
          shiny::incProgress(0.3, detail = "Creating mosaics...")

          baltic_mosaics <- create_region_mosaics(
            baltic_wide, clean_class, rv$baltic_samples,
            file.path(storage, "raw"), taxa_lookup,
            n_taxa = config$n_mosaic_taxa,
            n_images = config$n_mosaic_images
          )

          westcoast_mosaics <- create_region_mosaics(
            westcoast_wide, clean_class, rv$westcoast_samples,
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

          generate_report(
            out_file, station_summary,
            baltic_wide, westcoast_wide,
            baltic_mosaics, westcoast_mosaics,
            taxa_lookup = taxa_lookup,
            cruise_info = rv$cruise_info
          )

          report_path(out_file)
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

    output$report_status <- shiny::renderUI({
      rp <- report_path()
      if (is.null(rp)) return(NULL)
      shiny::div(
        style = "font-size: 12px; color: green; margin-top: 8px;",
        shiny::icon("check"), " Report ready for download."
      )
    })
  })
}
