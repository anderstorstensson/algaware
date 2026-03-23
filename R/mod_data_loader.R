#' Data Loader Module UI
#'
#' Sidebar controls for selecting a cruise or date range and loading data.
#'
#' @param id Module namespace ID. Shiny modules use namespaced IDs so that
#'   multiple instances of the same module don't conflict. \code{NS(id)}
#'   creates a function that prefixes all input/output IDs with this namespace.
#' @return A UI element.
#' @export
mod_data_loader_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h5("Data Selection"),
    shiny::radioButtons(ns("selection_mode"), NULL,
                        choices = c("Cruise" = "cruise",
                                    "Date Range" = "date"),
                        selected = "cruise", inline = TRUE),

    shiny::conditionalPanel(
      condition = paste0("input['", ns("selection_mode"), "'] == 'cruise'"),
      shiny::selectInput(ns("cruise_select"), "Cruise Number",
                         choices = NULL)
    ),

    shiny::conditionalPanel(
      condition = paste0("input['", ns("selection_mode"), "'] == 'date'"),
      shiny::dateRangeInput(ns("date_range"), "Date Range",
                            start = Sys.Date() - 7,
                            end = Sys.Date())
    ),

    shiny::actionButton(ns("fetch_metadata"), "Fetch Metadata",
                        class = "btn-outline-primary btn-sm mb-2",
                        icon = shiny::icon("download")),
    shiny::actionButton(ns("load_data"), "Load Data",
                        class = "btn-primary mb-2",
                        icon = shiny::icon("database")),
    shiny::hr(),
    shiny::uiOutput(ns("status_text"))
  )
}

#' Filter and match metadata to stations
#' @keywords internal
filter_and_match <- function(dashboard_metadata, selection_mode, cruise,
                             date_range, extra_stations) {
  if (selection_mode == "cruise") {
    filtered <- filter_metadata(dashboard_metadata, cruise = cruise)
    cruise_info <- cruise
  } else {
    filtered <- filter_metadata(dashboard_metadata,
                                date_from = date_range[1],
                                date_to = date_range[2])
    cruise_info <- paste(date_range[1], "to", date_range[2])
  }

  if (nrow(filtered) == 0) {
    return(list(matched = data.frame(), cruise_info = cruise_info))
  }

  algaware_stations <- load_algaware_stations(extra_stations)
  matched <- match_bins_to_stations(filtered, algaware_stations)

  list(matched = matched, cruise_info = cruise_info)
}

#' Download raw data, features, and classification files
#' @keywords internal
download_all_data <- function(config, sample_ids, storage) {
  raw_dir <- file.path(storage, "raw")
  feat_dir <- file.path(storage, "features")
  class_dir <- file.path(storage, "classified")

  download_raw_data(config$dashboard_url, sample_ids, raw_dir)

  feat_url <- config$dashboard_url
  if (nzchar(config$dashboard_dataset)) {
    feat_url <- paste0(sub("/+$", "", feat_url), "/",
                       config$dashboard_dataset, "/")
  }
  download_features(feat_url, sample_ids, feat_dir)

  if (nzchar(config$classification_path)) {
    copy_classification_files(config$classification_path,
                              sample_ids, class_dir)
  }

  list(raw_dir = raw_dir, feat_dir = feat_dir, class_dir = class_dir)
}

#' Process classifications and compute station summaries
#' @keywords internal
process_classifications <- function(config, dirs, sample_ids, matched) {
  classifications <- read_h5_classifications(dirs$class_dir, sample_ids)

  if (nrow(classifications) == 0) return(NULL)

  non_bio <- parse_non_bio_classes(config$non_biological_classes)

  taxa_lookup <- load_taxa_lookup()

  # Non-biological classes are excluded from biovolume calculations
  # by summarize_biovolumes() but kept in classifications for gallery display
  biovolume_data <- summarize_biovolumes(
    dirs$feat_dir, dirs$raw_dir, classifications,
    taxa_lookup, non_bio,
    pixels_per_micron = config$pixels_per_micron
  )

  station_summary <- aggregate_station_data(biovolume_data, matched)

  classifier_name <- read_classifier_name(dirs$class_dir)

  list(
    classifications_raw = classifications,
    classifications = classifications,
    non_bio_classes = non_bio,
    taxa_lookup = taxa_lookup,
    station_summary = station_summary,
    classifier_name = classifier_name
  )
}

#' Collect and merge ferrybox chlorophyll data
#' @keywords internal
merge_ferrybox_data <- function(config, matched, station_summary) {
  chl_summary <- NULL

  if (nzchar(config$ferrybox_path)) {
    fb_data <- collect_ferrybox_data(
      matched$sample_time, config$ferrybox_path
    )
    if (nrow(fb_data) > 0 && "chl" %in% names(fb_data)) {
      fb_chl <- fb_data[, c("timestamp", "chl")]
      matched_fb <- merge(
        matched[, c("pid", "STATION_NAME", "sample_time")],
        fb_chl,
        by.x = "sample_time", by.y = "timestamp",
        all.x = TRUE
      )
      chl_summary <- stats::aggregate(
        chl ~ STATION_NAME,
        data = matched_fb,
        FUN = function(x) {
          vals <- x[!is.na(x)]
          if (length(vals) == 0) NA_real_ else mean(vals)
        },
        na.action = stats::na.pass
      )
      names(chl_summary)[names(chl_summary) == "chl"] <- "chl_mean"
      station_summary <- merge(station_summary, chl_summary,
                               by = "STATION_NAME", all.x = TRUE)
    }
  }

  list(station_summary = station_summary, chl_summary = chl_summary)
}

#' Resolve the class list from database or auto-generate
#' @keywords internal
resolve_classes <- function(config, taxa_lookup, classifications) {
  resolved_classes <- NULL
  if (nzchar(config$db_folder)) {
    db_path <- get_db_path(config$db_folder)
    resolved_classes <- resolve_class_list(db_path)
  }

  auto_generated <- FALSE
  if (is.null(resolved_classes)) {
    all_class_names <- sort(unique(c(
      taxa_lookup$clean_names,
      classifications$class_name
    )))
    resolved_classes <- unique(c(all_class_names, "unclassified"))
    auto_generated <- TRUE
  }

  list(class_list = resolved_classes, auto_generated = auto_generated)
}

#' Build a descriptive cruise info string from sample timestamps
#'
#' Determines the most common month among samples and formats as
#' "RV Svea <Month> cruise, <start date> to <end date>".
#'
#' @param sample_times POSIXct vector of sample timestamps.
#' @return Character string.
#' @keywords internal
build_cruise_info <- function(sample_times) {
  dates <- as.Date(sample_times)
  old_locale <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", old_locale), add = TRUE)
  Sys.setlocale("LC_TIME", "C")
  months <- format(dates, "%B")
  month_tab <- table(months)
  dominant_month <- names(month_tab)[which.max(month_tab)]
  date_range <- paste(min(dates), "to", max(dates))
  paste0("RV Svea ", dominant_month, " cruise, ", date_range)
}

#' Sanitize error message for user display
#' @keywords internal
sanitize_error_msg <- function(msg) {
  sub("^.*: ", "", msg)
}

#' Data Loader Module Server
#'
#' Handles the full data loading pipeline: fetch metadata from the IFCB
#' Dashboard, match bins to monitoring stations, download raw files and
#' classifications, compute biovolumes, and populate the shared reactive
#' state (\code{rv}).
#'
#' @param id Module namespace ID.
#' @param config Reactive values with settings.
#' @param rv Reactive values for app state (see server.R for field docs).
#' @return NULL (side effects only).
#' @export
mod_data_loader_server <- function(id, config, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    status <- shiny::reactiveVal("Ready. Click 'Fetch Metadata' to start.")

    output$status_text <- shiny::renderUI({
      shiny::div(
        style = "font-size: 12px; color: #666; white-space: pre-wrap;",
        status()
      )
    })

    # Fetch metadata from dashboard
    shiny::observeEvent(input$fetch_metadata, {
      if (!nzchar(config$dashboard_url %||% "")) {
        shiny::showNotification(
          "Please enter a Dashboard URL in Settings first.",
          type = "warning"
        )
        return()
      }

      status("Fetching metadata from dashboard...")
      progress_id <- shiny::showNotification(
        "Fetching metadata from dashboard...",
        type = "message", duration = NULL, closeButton = FALSE
      )
      on.exit(shiny::removeNotification(progress_id), add = TRUE)

      tryCatch({
        result <- fetch_dashboard_metadata(
          config$dashboard_url,
          dataset_name = config$dashboard_dataset
        )
        rv$dashboard_metadata <- result$metadata
        rv$cruise_numbers <- result$cruise_numbers

        if (length(result$cruise_numbers) > 0) {
          shiny::updateSelectInput(
            session, "cruise_select",
            choices = rev(result$cruise_numbers),
            selected = utils::tail(result$cruise_numbers, 1)
          )
          status(paste0("Found ", nrow(result$metadata), " bins, ",
                        length(result$cruise_numbers), " cruises."))
        } else {
          status(paste0("Found ", nrow(result$metadata),
                        " bins (no cruise numbers available)."))
        }
      }, error = function(e) {
        status(paste0("Error: ", sanitize_error_msg(e$message)))
        shiny::showNotification(sanitize_error_msg(e$message), type = "error")
      })
    })

    # Load and process data
    shiny::observeEvent(input$load_data, {
      if (is.null(rv$dashboard_metadata)) {
        shiny::showNotification(
          "Please fetch metadata first.",
          type = "warning"
        )
        return()
      }

      shiny::withProgress(message = "Loading data...", value = 0, {
        tryCatch({
          # Step 1: Filter and match
          shiny::incProgress(0.05, detail = "Filtering metadata...")
          match_result <- filter_and_match(
            rv$dashboard_metadata, input$selection_mode,
            input$cruise_select, input$date_range, config$extra_stations
          )
          matched <- match_result$matched

          if (nrow(matched) == 0) {
            status("No bins matched to AlgAware stations.")
            shiny::showNotification("No bins matched", type = "warning")
            return()
          }

          rv$matched_metadata <- matched
          status(paste0("Matched ", nrow(matched), " bins to ",
                        length(unique(matched$STATION_NAME)), " stations."))

          sample_ids <- matched$pid
          storage <- config$local_storage_path

          # Step 2: Download data
          shiny::incProgress(0.35, detail = "Downloading data...")
          dirs <- download_all_data(config, sample_ids, storage)

          # Step 3: Process classifications
          shiny::incProgress(0.25, detail = "Processing classifications...")
          proc <- process_classifications(config, dirs, sample_ids, matched)

          if (is.null(proc)) {
            status("No classifications found. Check classification path.")
            shiny::showNotification("No classifications found",
                                    type = "warning")
            return()
          }

          rv$classifications_raw <- proc$classifications_raw
          rv$classifications <- proc$classifications
          rv$invalidated_classes <- proc$non_bio_classes
          rv$taxa_lookup <- proc$taxa_lookup
          rv$classifier_name <- proc$classifier_name

          # Build descriptive cruise info from sample dates
          rv$cruise_info <- build_cruise_info(matched$sample_time)

          # Step 4a: Fetch cruise-wide image counts
          shiny::incProgress(0.05, detail = "Fetching image counts...")
          sample_dates <- as.Date(matched$sample_time)
          rv$image_counts <- fetch_image_counts(
            config$dashboard_url, config$dashboard_dataset,
            min(sample_dates), max(sample_dates)
          )

          # Step 4b: Ferrybox data
          shiny::incProgress(0.05, detail = "Collecting ferrybox data...")
          fb_result <- merge_ferrybox_data(config, matched,
                                           proc$station_summary)
          rv$station_summary <- fb_result$station_summary
          rv$ferrybox_chl <- fb_result$chl_summary

          # Step 5: Create summaries
          shiny::incProgress(0.1, detail = "Creating summaries...")
          rv$baltic_wide <- create_wide_summary(rv$station_summary, "EAST")
          rv$westcoast_wide <- create_wide_summary(rv$station_summary, "WEST")

          rv$baltic_samples <- matched$pid[matched$COAST == "EAST"]
          rv$westcoast_samples <- matched$pid[matched$COAST == "WEST"]

          # Step 6: Resolve class list
          shiny::incProgress(0.05, detail = "Resolving class list...")
          class_result <- resolve_classes(config, rv$taxa_lookup,
                                          rv$classifications)
          rv$class_list <- class_result$class_list

          if (class_result$auto_generated) {
            shiny::showNotification(
              paste0("Class list auto-generated from taxa lookup (",
                     length(class_result$class_list), " classes). ",
                     "Set a database folder in Settings to use a ",
                     "curated class list."),
              type = "message", duration = 8
            )
          }

          # Build extended relabel choices (DB + taxa lookup + custom)
          rv$relabel_choices <- build_relabel_choices(
            rv$class_list, rv$taxa_lookup, rv$custom_classes
          )

          # Warn about unmatched classes
          observed_classes <- unique(rv$classifications$class_name)
          unmatched <- setdiff(observed_classes, rv$class_list)
          if (length(unmatched) > 0) {
            shiny::showNotification(
              paste0(length(unmatched), " class(es) in classifications not ",
                     "in class list: ",
                     paste(utils::head(unmatched, 5), collapse = ", "),
                     if (length(unmatched) > 5) "..."),
              type = "warning", duration = 10
            )
          }

          # Gallery classes
          gallery_classes <- sort(unique(rv$classifications$class_name))
          gallery_classes <- gallery_classes[gallery_classes != "unclassified"]
          rv$current_class_idx <- 1L
          rv$current_region <- "EAST"
          rv$data_loaded <- TRUE

          status(paste0("Data loaded successfully!\n",
                        nrow(matched), " bins, ",
                        length(unique(matched$STATION_NAME)), " stations, ",
                        length(gallery_classes), " classes, ",
                        length(rv$class_list), " in class list.\n",
                        "Proceed to 'Validate' tab."))

        }, error = function(e) {
          status(paste0("Error: ", sanitize_error_msg(e$message)))
          shiny::showNotification(sanitize_error_msg(e$message),
                                  type = "error", duration = 10)
        })
      })
    })
  })
}
