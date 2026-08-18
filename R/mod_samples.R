#' Sample Exclusion Module UI
#'
#' @param id Module namespace ID.
#' @return A UI element.
#' @export
mod_samples_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "d-flex gap-2 mb-3",
      shiny::actionButton(ns("exclude_selected"), "Exclude Selected",
                          class = "btn-warning btn-sm",
                          icon = shiny::icon("minus-circle")),
      shiny::actionButton(ns("include_selected"), "Include Selected",
                          class = "btn-outline-primary btn-sm",
                          icon = shiny::icon("plus-circle")),
      shiny::actionButton(ns("include_all"), "Include All",
                          class = "btn-outline-success btn-sm",
                          icon = shiny::icon("rotate-left"))
    ),
    shiny::uiOutput(ns("sample_status")),
    DT::DTOutput(ns("samples_table"))
  )
}

#' Build the sample summary table
#'
#' Transforms raw matched metadata into the display table used by the samples
#' module. Pure function — no Shiny dependency.
#'
#' @param matched_metadata_all Data frame with at least columns \code{pid},
#'   \code{STATION_NAME}, \code{STATION_NAME_SHORT}, \code{COAST}, and
#'   \code{sample_time}.
#' @param excluded_samples Character vector of PIDs currently excluded.
#' @param raw_dir Path to the raw data directory containing \code{.roi} files,
#'   or \code{NULL} to skip file-size lookup.
#' @return A data frame with columns \code{Status}, \code{pid},
#'   \code{STATION_NAME_SHORT}, \code{STATION_NAME}, \code{Region},
#'   \code{Time}, and \code{ROI_MB}.
#' @keywords internal
build_sample_table <- function(matched_metadata_all, excluded_samples,
                               raw_dir = NULL) {
  samples <- unique(matched_metadata_all[, c(
    "pid", "STATION_NAME", "STATION_NAME_SHORT", "COAST", "sample_time"
  )])
  samples <- samples[order(samples$sample_time, samples$STATION_NAME), ]
  samples$Status <- ifelse(samples$pid %in% excluded_samples,
                           "Excluded", "Included")
  samples$Region <- ifelse(samples$COAST == "EAST", "Baltic Sea",
                           ifelse(samples$COAST == "WEST", "West Coast",
                                  samples$COAST))
  samples$Time <- format(samples$sample_time, "%Y-%m-%d %H:%M:%S")

  roi_sizes_mb <- rep(NA_real_, nrow(samples))
  if (!is.null(raw_dir) && dir.exists(raw_dir)) {
    roi_files <- list.files(raw_dir, pattern = "\\.roi$",
                            recursive = TRUE, full.names = TRUE)
    if (length(roi_files) > 0) {
      roi_basenames <- tools::file_path_sans_ext(basename(roi_files))
      roi_sizes <- file.size(roi_files) / (1024^2)
      size_map <- stats::setNames(as.numeric(roi_sizes), roi_basenames)
      roi_sizes_mb <- unname(size_map[samples$pid])
    }
  }
  samples$ROI_MB <- roi_sizes_mb

  samples[, c("Status", "pid", "STATION_NAME_SHORT", "STATION_NAME",
              "Region", "Time", "ROI_MB")]
}

#' Sample Exclusion Module Server
#'
#' Provides a table of loaded samples and lets the user exclude selected
#' samples from the active dataset. Excluded samples remain available in memory
#' and can be re-included later.
#'
#' @param id Module namespace ID.
#' @param rv Reactive values for app state.
#' @param config Reactive values with settings.
#' @return NULL (side effects only).
#' @export
mod_samples_server <- function(id, rv, config) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    sample_table <- shiny::reactive({
      shiny::req(rv$data_loaded, rv$matched_metadata_all)
      raw_dir <- file.path(config$local_storage_path, "raw")
      build_sample_table(rv$matched_metadata_all, rv$excluded_samples, raw_dir)
    })

    output$sample_status <- shiny::renderUI({
      tbl <- tryCatch(sample_table(), error = function(e) NULL)
      if (is.null(tbl) || nrow(tbl) == 0) return(NULL)

      n_excluded <- sum(tbl$Status == "Excluded")
      n_included <- nrow(tbl) - n_excluded

      shiny::div(
        class = "mb-2",
        shiny::p(
          shiny::strong(n_included), " included, ",
          shiny::strong(n_excluded), " excluded."
        ),
        shiny::p(
          style = "font-size: 12px; color: #666;",
          "Excluded samples are removed from validation, summaries, maps, mosaics, and report generation."
        )
      )
    })

    output$samples_table <- DT::renderDT({
      tbl <- sample_table()
      DT::datatable(
        tbl,
        selection = "multiple",
        rownames = FALSE,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          # 0-indexed with rownames = FALSE: column 5 is Time. Column 6 was
          # ROI_MB, which re-sorted the table by file size and scrambled the
          # deliberate chronological (cruise-track) order.
          order = list(list(5, "asc"))
        )
      ) |>
        DT::formatRound("ROI_MB", 2)
    })

    proxy <- DT::dataTableProxy(ns("samples_table"))

    get_selected_pids <- function() {
      rows <- input$samples_table_rows_selected
      if (length(rows) == 0) return(character(0))
      sample_table()$pid[rows]
    }

    shiny::observeEvent(input$exclude_selected, {
      shiny::req(rv$data_loaded)
      selected_pids <- get_selected_pids()
      shiny::req(length(selected_pids) > 0)

      rv$excluded_samples <- unique(c(rv$excluded_samples, selected_pids))
      DT::selectRows(proxy, NULL)
    })

    shiny::observeEvent(input$include_selected, {
      shiny::req(rv$data_loaded)
      selected_pids <- get_selected_pids()
      shiny::req(length(selected_pids) > 0)

      rv$excluded_samples <- setdiff(rv$excluded_samples, selected_pids)
      DT::selectRows(proxy, NULL)
    })

    shiny::observeEvent(input$include_all, {
      shiny::req(rv$data_loaded)
      rv$excluded_samples <- character(0)
      DT::selectRows(proxy, NULL)
    })
  })
}
