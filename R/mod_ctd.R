#' CTD Module UI
#'
#' @param id Module namespace ID.
#' @return A UI element.
#' @export
mod_ctd_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "alert alert-info py-1 px-2 mb-3",
      style = "font-size: 12px;",
      shiny::icon("circle-info"),
      " CTD and LIMS data loaded here will be included in the report and improve the chlorophyll map."
    ),
    shiny::div(
      class = "ctd-inputs",
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::textInput(ns("cnv_folder"), "CNV Folder Path",
                           placeholder = "/path/to/Cnv/")
        ),
        shiny::column(
          width = 6,
          shiny::fileInput(ns("lims_file"), "LIMS data.txt (optional)",
                           accept = c(".txt", "text/plain"),
                           buttonLabel = "Browse\u2026",
                           placeholder = "No file selected")
        )
      ),
      shiny::actionButton(ns("load_ctd"), "Load CTD Data",
                          class = "btn-primary mb-2",
                          icon = shiny::icon("water")),
      shiny::uiOutput(ns("ctd_status"))
    ),
    shiny::conditionalPanel(
      condition = paste0("output['", ns("ctd_loaded"), "']"),
      shiny::uiOutput(ns("region_plots_ui"))
    ),
    shiny::conditionalPanel(
      condition = paste0("!output['", ns("ctd_loaded"), "']"),
      shiny::div(
        class = "empty-state",
        shiny::icon("water"),
        shiny::h4("No CTD data loaded"),
        shiny::p("Enter a CNV folder path and click Load CTD Data.")
      )
    )
  )
}

#' Filter a data frame to a date range
#'
#' Keeps rows where \code{sample_date} is non-NA and falls within
#' \code{date_min} to \code{date_max} (inclusive). Used to restrict LIMS data
#' to the cruise date window.
#'
#' @param df Data frame with a \code{sample_date} column.
#' @param date_min Minimum date (inclusive).
#' @param date_max Maximum date (inclusive).
#' @return Row-filtered \code{df}, or \code{df} unchanged if empty.
#' @keywords internal
filter_lims_by_date_range <- function(df, date_min, date_max) {
  if (is.null(df) || nrow(df) == 0) return(df)
  df[!is.na(df$sample_date) &
     df$sample_date >= date_min &
     df$sample_date <= date_max, ]
}

#' Extract the current year from CTD data
#'
#' Returns the year of the most recent sample date, or the current year if no
#' dates are available.
#'
#' @param ctd_data_full Data frame with a \code{sample_date} column.
#' @return Integer year.
#' @keywords internal
extract_current_year_from_ctd <- function(ctd_data_full) {
  dates <- ctd_data_full$sample_date[!is.na(ctd_data_full$sample_date)]
  if (length(dates) > 0) {
    as.integer(format(max(dates), "%Y"))
  } else {
    as.integer(format(Sys.Date(), "%Y"))
  }
}

#' Compute plot height for a CTD region panel
#'
#' Height scales with the number of unique stations in the region so that
#' dense regions get more vertical space.
#'
#' @param ctd_data_full Data frame with \code{canonical_name} and
#'   \code{region} columns.
#' @param region Character region name.
#' @return Character CSS height string (e.g. \code{"780px"}).
#' @keywords internal
compute_region_plot_height <- function(ctd_data_full, region) {
  n_unique <- length(unique(
    ctd_data_full$canonical_name[ctd_data_full$region == region]
  ))
  paste0(max(300L, n_unique * 260L), "px")
}

#' CTD Module Server
#'
#' @param id Module namespace ID.
#' @param config Reactive values with settings.
#' @param rv Reactive values for shared app state.
#' @return NULL (side effects only).
#' @export
mod_ctd_server <- function(id, config, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # CTD paths are not persisted across sessions (intentionally blank on start)

    # Expose ctd_loaded for conditionalPanel
    output$ctd_loaded <- shiny::reactive(isTRUE(rv$ctd_loaded))
    shiny::outputOptions(output, "ctd_loaded", suspendWhenHidden = FALSE)

    # Inner function so both the button and the LIMS file upload can trigger it
    do_load_ctd <- function() {
      cnv_folder <- trimws(input$cnv_folder)
      lims_path  <- input$lims_file$datapath  # NULL if no file uploaded

      if (!nzchar(cnv_folder)) {
        shiny::showNotification("Please enter a CNV folder path.",
                                type = "warning")
        return()
      }
      if (!dir.exists(cnv_folder)) {
        shiny::showNotification("CNV folder does not exist.", type = "error")
        return()
      }

      shiny::withProgress(message = "Loading CTD data...", value = 0, {
        tryCatch({
          # Invalidate the previous load first: if this load fails or finds
          # no matching stations, the CTD tab and the report must not keep
          # using the previous cruise's profiles as if they belonged to the
          # newly selected folder.
          rv$ctd_loaded     <- FALSE
          rv$ctd_data_full  <- NULL
          rv$lims_data_full <- NULL
          rv$ctd_data       <- NULL
          rv$lims_data      <- NULL

          # Load reference tables
          station_mapper    <- load_station_mapper()
          standard_stations <- load_standard_stations()
          algaware_stations <- load_algaware_stations(config$extra_stations)

          # --- All standard stations (for CTD tab plots) ---
          shiny::incProgress(0.2, detail = "Parsing CNV files...")
          ctd_data_full <- read_cnv_folder_all(cnv_folder,
                                               station_mapper,
                                               standard_stations)

          if (is.null(ctd_data_full) || nrow(ctd_data_full) == 0) {
            shiny::showNotification(
              "No matching stations found in CNV files.", type = "warning")
            return()
          }

          # --- AlgAware subset (for chl map) ---
          ctd_data <- read_cnv_folder(cnv_folder, algaware_stations)

          # --- LIMS (from uploaded file) ---
          lims_data_full <- NULL
          lims_data      <- NULL
          if (!is.null(lims_path) && file.exists(lims_path)) {
            shiny::incProgress(0.2, detail = "Reading LIMS data...")
            lims_data_full <- read_lims_data_all(lims_path,
                                                 station_mapper,
                                                 standard_stations)
            lims_data <- read_lims_data(lims_path, algaware_stations)

            # Filter lims_data (AlgAware chl map) to the cruise date range
            # derived from the CTD data.  A 1-day buffer on each side covers
            # the typical offset between CTD casts and bottle sampling, and
            # handles cruises that span a month boundary (e.g. 27 Feb – 6 Mar).
            # lims_data_full (used for the time-series panels) is intentionally
            # NOT filtered here so all cruises within the year are shown as
            # green points in the time series.
            ctd_dates <- ctd_data_full$sample_date[
              !is.na(ctd_data_full$sample_date)]
            if (length(ctd_dates) > 0) {
              date_min <- min(ctd_dates) - 1L
              date_max <- max(ctd_dates) + 1L
              lims_data <- filter_lims_by_date_range(lims_data,
                                                     date_min, date_max)
            }
          }

          shiny::incProgress(0.2, detail = "Loading statistics...")
          chl_stats <- load_chl_statistics()

          rv$ctd_data_full  <- ctd_data_full
          rv$lims_data_full <- lims_data_full
          rv$ctd_data       <- ctd_data
          rv$lims_data      <- lims_data
          rv$chl_stats      <- chl_stats
          rv$ctd_loaded     <- TRUE

          # Paths are not persisted (session-only)

          n_stations <- length(unique(ctd_data_full$canonical_name))
          n_profiles <- length(unique(ctd_data_full$file_path))
          msg <- paste0("Loaded ", n_profiles, " profiles from ",
                        n_stations, " stations.")
          if (!is.null(lims_data_full)) {
            msg <- paste0(msg, " LIMS: ",
                          length(unique(lims_data_full$canonical_name)),
                          " stations.")
          }

          shiny::incProgress(0.4, detail = "Done!")
          shiny::showNotification(msg, type = "message")
        }, error = function(e) {
          shiny::showNotification(
            paste0("CTD loading error: ", e$message),
            type = "error", duration = 10)
        })
      })
    }

    # Load CTD data on button click
    shiny::observeEvent(input$load_ctd, {
      do_load_ctd()
    })

    # Auto-reload when a new LIMS file is uploaded (only if CNV folder is set)
    shiny::observeEvent(input$lims_file, {
      shiny::req(nzchar(trimws(input$cnv_folder)))
      do_load_ctd()
    })

    output$ctd_status <- shiny::renderUI({
      if (!isTRUE(rv$ctd_loaded)) return(NULL)
      n_stn    <- length(unique(rv$ctd_data_full$canonical_name))
      has_lims <- !is.null(rv$lims_data_full) && nrow(rv$lims_data_full) > 0
      shiny::div(
        style = "font-size: 12px; color: green; margin-top: 4px;",
        shiny::icon("check"),
        paste0(n_stn, " CTD stations loaded",
               if (has_lims) ", LIMS data available" else "")
      )
    })

    # Current year from CTD data
    current_year <- shiny::reactive({
      shiny::req(rv$ctd_data_full)
      extract_current_year_from_ctd(rv$ctd_data_full)
    })

    # Regions present in loaded data (in YAML order)
    active_regions <- shiny::reactive({
      shiny::req(rv$ctd_data_full)
      std <- load_standard_stations()
      present <- unique(rv$ctd_data_full$region)
      # Preserve YAML region order (first appearance of each region in station_list)
      yaml_order <- unique(std$region[std$region %in% present])
      c(yaml_order, setdiff(present, yaml_order))
    })

    # Dynamically build one plotOutput per region
    output$region_plots_ui <- shiny::renderUI({
      shiny::req(isTRUE(rv$ctd_loaded))
      regions <- active_regions()
      plot_cards <- lapply(seq_along(regions), function(i) {
        reg <- regions[[i]]
        n_stn <- if (!is.null(rv$ctd_data_full)) {
          sum(rv$ctd_data_full$region == reg)
        } else { 0L }
        # Height scales with station count in this region
        ht <- compute_region_plot_height(rv$ctd_data_full, reg)
        shiny::div(
          class = "plot-card",
          shiny::plotOutput(ns(paste0("region_plot_", i)), height = ht)
        )
      })
      do.call(shiny::tagList, plot_cards)
    })

    # Render each region plot
    shiny::observe({
      shiny::req(isTRUE(rv$ctd_loaded), rv$ctd_data_full, rv$chl_stats)
      regions <- active_regions()
      std <- load_standard_stations()
      yr  <- current_year()

      for (i in seq_along(regions)) {
        local({
          reg <- regions[[i]]
          idx <- i
          output[[paste0("region_plot_", idx)]] <- shiny::renderPlot({
            shiny::req(rv$ctd_data_full)
            create_ctd_region_figure(
              ctd_data_full    = rv$ctd_data_full,
              lims_data_full   = rv$lims_data_full,
              chl_stats        = rv$chl_stats,
              standard_stations = std,
              region           = reg,
              current_year     = yr
            )
          })
        })
      }
    })
  })
}
