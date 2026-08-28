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
#'
#' Filters the full dashboard metadata by cruise number or date range and
#' spatially matches the resulting bins to AlgAware monitoring stations.
#'
#' @param dashboard_metadata Data frame from \code{fetch_dashboard_metadata()}.
#' @param selection_mode Character; \code{"cruise"} or \code{"date"}.
#' @param cruise Cruise number string (used when \code{selection_mode = "cruise"}).
#' @param date_range Length-2 Date vector (used when \code{selection_mode = "date"}).
#' @param extra_stations List of extra station definitions (from settings).
#' @return A list with \code{matched} (data frame) and \code{cruise_info} (string).
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
#'
#' Downloads .roi/.adc/.hdr raw files and feature CSVs from the IFCB Dashboard,
#' then copies AI classification H5 files from the configured source path.
#' All three destinations are subdirectories of \code{storage}:
#' \code{raw/}, \code{features/}, and \code{classified/}.
#'
#' @param config Reactive values with settings (\code{dashboard_url},
#'   \code{dashboard_dataset}, \code{classification_path}).
#' @param sample_ids Character vector of sample PIDs to retrieve.
#' @param storage Local base directory for downloaded files.
#' @return A list with \code{raw_dir}, \code{feat_dir}, \code{class_dir} paths.
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
#'
#' Reads H5 classification files, computes biovolume data for each classified
#' image, aggregates results by station visit, and extracts the classifier name
#' from the first H5 file. Non-biological classes are excluded from biovolume
#' but kept in the classification data frame for gallery display.
#'
#' @param config Reactive values with settings (\code{non_biological_classes},
#'   \code{pixels_per_micron}).
#' @param dirs List with \code{raw_dir}, \code{feat_dir}, \code{class_dir} paths.
#' @param sample_ids Character vector of sample PIDs to process.
#' @param matched Data frame of station-matched metadata.
#' @param cached_diatom_status Optional data.frame from a previous
#'   \code{resolve_diatom_status()} call (e.g. an earlier load in the same
#'   session), so already-resolved classes skip the WoRMS lookup.
#' @return A named list, or NULL if no H5 classifications were found.
#' @keywords internal
process_classifications <- function(config, dirs, sample_ids, matched,
                                    cached_diatom_status = NULL) {
  classifications <- read_h5_classifications(dirs$class_dir, sample_ids)

  if (nrow(classifications) == 0) return(NULL)

  non_bio <- parse_non_bio_classes(config$non_biological_classes)

  taxa_lookup <- load_taxa_lookup()

  # Read the immutable per-ROI biovolumes and per-sample volumes once and
  # keep them for the rest of the session: later summary recomputations
  # (corrections, sample exclusions, report generation) then run in memory
  # instead of re-reading every feature CSV and .hdr file.
  biovolume_cache <- tryCatch(
    build_biovolume_cache(dirs$feat_dir, dirs$raw_dir, sample_ids),
    error = function(e) {
      warning("Failed to build biovolume cache (falling back to ",
              "file-based summaries): ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )

  # Non-biological classes are excluded from biovolume calculations
  # but kept in classifications for gallery display
  if (!is.null(biovolume_cache) && nrow(biovolume_cache$roi_biovolumes) > 0) {
    diatom_status <- resolve_diatom_status(unique(classifications$class_name),
                                           cached_status = cached_diatom_status)
    biovolume_data <- summarize_biovolumes_cached(
      biovolume_cache, classifications, taxa_lookup, non_bio,
      pixels_per_micron = config$pixels_per_micron,
      diatom_status = diatom_status
    )
  } else {
    diatom_status <- NULL
    biovolume_data <- summarize_biovolumes(
      dirs$feat_dir, dirs$raw_dir, classifications,
      taxa_lookup, non_bio,
      pixels_per_micron = config$pixels_per_micron
    )
  }

  station_summary <- aggregate_station_data(biovolume_data, matched)

  classifier_name <- read_classifier_name(dirs$class_dir)

  list(
    classifications_raw = classifications,
    classifications = classifications,
    non_bio_classes = non_bio,
    taxa_lookup = taxa_lookup,
    station_summary = station_summary,
    classifier_name = classifier_name,
    biovolume_cache = biovolume_cache,
    diatom_status = diatom_status
  )
}

#' Collect and merge ferrybox chlorophyll data
#'
#' Fetches ferrybox data for the cruise sample timestamps, extracts
#' chlorophyll fluorescence (parameter 8063, QC-approved values only),
#' and computes a per-station mean that is merged into \code{station_summary}.
#' Returns an empty data frame (no error) when the ferrybox path is not
#' configured or no matching data is found.
#'
#' @param config Reactive values with settings (\code{ferrybox_path}).
#' @param matched Station-matched metadata with \code{sample_time} column.
#' @param station_summary Aggregated station data to receive \code{chl_mean}.
#' @return A list with \code{station_summary}, \code{chl_summary}, and
#'   \code{ferrybox_data} fields.
#' @keywords internal
merge_ferrybox_data <- function(config, matched, station_summary) {
  chl_summary <- NULL
  fb_data <- data.frame(
    timestamp = matched$sample_time[0],
    chl = numeric(0),
    stringsAsFactors = FALSE
  )

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

  list(
    station_summary = station_summary,
    chl_summary = chl_summary,
    ferrybox_data = fb_data
  )
}

#' Resolve the class list from database or auto-generate
#'
#' Tries to load the global class list from the configured SQLite database
#' (shared with ClassiPyR). If no database is configured or it has no class
#' list, auto-generates one from the union of all taxa lookup names and
#' observed classification class names. The caller receives a flag indicating
#' which path was taken so it can show a notification.
#'
#' @param config Reactive values with settings (\code{db_folder}).
#' @param taxa_lookup Data frame with \code{clean_names} column.
#' @param classifications Data frame with \code{class_name} column.
#' @return A list with \code{class_list} (character vector) and
#'   \code{auto_generated} (logical).
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
#' @export
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
#'
#' Strips the leading "Error in <call>: " prefix that R prepends to condition
#' messages so that only the human-readable part is shown in the sidebar.
#' Only that exact prefix is removed: the old greedy pattern (`"^.*: "`)
#' stripped everything up to the *last* colon, reducing e.g.
#' `cannot open URL 'https://...': HTTP status was '404 Not Found'` to just
#' `'404 Not Found'` -- and this is the only error surface the app has.
#'
#' @param msg Character string (typically \code{e$message}).
#' @return Simplified character string.
#' @keywords internal
sanitize_error_msg <- function(msg) {
  sub("^Error in [^:]*: ", "", msg)
}

#' Detect near-empty bins (possible cleaning-cycle samples)
#'
#' At the end of a cruise the IFCB runs a cleaning cycle in which it pulls
#' distilled water. When that coincides with a station on the AlgAware list,
#' the resulting bins contain almost no images and should usually be excluded
#' from the report. A bin is flagged when its image count is below the
#' absolute threshold \code{min_images}. The threshold is deliberately
#' strict: legitimate bins can be small (e.g. few cells on the west coast
#' while the Baltic blooms), so a relative/median-based criterion would
#' produce false positives. Only truly near-empty distilled-water bins
#' should be caught.
#'
#' @param matched Data frame of station-matched metadata. Requires \code{pid}
#'   and \code{n_images} columns; \code{STATION_NAME} is used when present.
#' @param min_images Absolute threshold: bins with fewer images are flagged.
#' @return Data frame with \code{pid}, \code{STATION_NAME}, and
#'   \code{n_images} for flagged bins, sorted by image count (ascending).
#'   Empty when nothing is flagged or when no usable image counts exist.
#' @keywords internal
detect_near_empty_bins <- function(matched, min_images = 20) {
  empty <- data.frame(pid = character(0), STATION_NAME = character(0),
                      n_images = numeric(0), stringsAsFactors = FALSE)
  if (is.null(matched) || nrow(matched) == 0 ||
      !all(c("pid", "n_images") %in% names(matched))) {
    return(empty)
  }

  n_images <- suppressWarnings(as.numeric(matched$n_images))
  flagged <- !is.na(n_images) & n_images < min_images
  if (!any(flagged)) return(empty)

  station <- if ("STATION_NAME" %in% names(matched)) {
    as.character(matched$STATION_NAME)
  } else {
    rep(NA_character_, nrow(matched))
  }

  out <- data.frame(
    pid = as.character(matched$pid)[flagged],
    STATION_NAME = station[flagged],
    n_images = n_images[flagged],
    stringsAsFactors = FALSE
  )
  out[order(out$n_images), , drop = FALSE]
}

#' Build the persistent near-empty-bin warning UI
#'
#' @param near_empty Data frame from \code{detect_near_empty_bins()}.
#' @param max_listed Maximum number of bins to list individually; the rest
#'   are summarized as "+ n more".
#' @return A \code{shiny::tagList} for use in \code{showNotification()}.
#' @keywords internal
build_near_empty_warning <- function(near_empty, max_listed = 10) {
  n <- nrow(near_empty)
  listed <- utils::head(near_empty, max_listed)
  lines <- paste0(
    listed$pid,
    ifelse(is.na(listed$STATION_NAME) | !nzchar(listed$STATION_NAME), "",
           paste0(" (", listed$STATION_NAME, ")")),
    ": ", listed$n_images, " images"
  )
  n_more <- n - nrow(listed)

  shiny::tagList(
    shiny::strong(paste0(
      n, " near-empty bin", if (n > 1) "s", " detected"
    )),
    shiny::p(
      style = "margin: 4px 0;",
      "These may be IFCB cleaning-cycle samples (distilled water) ",
      "and should usually not be included in the report."
    ),
    shiny::div(
      style = "font-size: 12px; font-family: monospace;",
      lapply(lines, shiny::div),
      if (n_more > 0) shiny::div(paste0("+ ", n_more, " more"))
    ),
    shiny::p(
      style = "margin: 4px 0 0 0;",
      "Review and exclude them in the 'Samples' tab if needed."
    )
  )
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

    # Fetch metadata from dashboard. Incremental: only bins newer than the
    # local cache are downloaded. A full re-download happens automatically
    # when there is no cache -- "Clear Metadata Cache" in Settings forces it.
    run_metadata_fetch <- function() {
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
          dataset_name = config$dashboard_dataset,
          cache_dir = config$local_storage_path
        )
        rv$dashboard_metadata <- result$metadata
        rv$cruise_numbers <- result$cruise_numbers

        fetch_info <- if (isTRUE(result$incremental)) {
          paste0(" (cached; ", result$n_new, " bin",
                 if (result$n_new != 1) "s",
                 " refreshed from dashboard)")
        } else {
          ""
        }
        if (length(result$cruise_numbers) > 0) {
          shiny::updateSelectInput(
            session, "cruise_select",
            choices = rev(result$cruise_numbers),
            selected = utils::tail(result$cruise_numbers, 1)
          )
          status(paste0("Found ", nrow(result$metadata), " bins, ",
                        length(result$cruise_numbers), " cruises.",
                        fetch_info))
        } else {
          status(paste0("Found ", nrow(result$metadata),
                        " bins (no cruise numbers available).", fetch_info))
        }
      }, error = function(e) {
        status(paste0("Error: ", sanitize_error_msg(e$message)))
        shiny::showNotification(sanitize_error_msg(e$message), type = "error")
      })
    }

    shiny::observeEvent(input$fetch_metadata, run_metadata_fetch())

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

          # Do NOT write `matched` into rv yet: everything below reads the
          # local variable, and committing to rv before download/processing
          # can fail would leave the app half-loaded (new cruise's metadata
          # with the previous cruise's classifications). All rv writes happen
          # together after processing succeeds.
          status(paste0("Matched ", nrow(matched), " bins to ",
                        length(unique(matched$STATION_NAME)), " stations."))

          sample_ids <- matched$pid
          storage <- config$local_storage_path

          # Step 2: Download data
          shiny::incProgress(0.35, detail = "Downloading data...")
          dirs <- download_all_data(config, sample_ids, storage)

          # Step 3: Process classifications
          shiny::incProgress(0.25, detail = "Processing classifications...")
          proc <- process_classifications(config, dirs, sample_ids, matched,
                                          cached_diatom_status = rv$diatom_status)

          if (is.null(proc)) {
            resolved_class_path <- resolve_classification_path(
              config$classification_path
            )
            local_n <- length(list.files(
              dirs$class_dir, pattern = "_class.*\\.h5$",
              recursive = TRUE
            ))
            local_files <- list.files(
              dirs$class_dir, pattern = "_class.*\\.h5$",
              recursive = TRUE, full.names = FALSE
            )
            local_samples <- unique(sub("_class.*\\.h5$", "", basename(local_files)))
            requested_samples <- unique(sample_ids)
            matched_local <- intersect(requested_samples, local_samples)
            missing_local <- setdiff(requested_samples, local_samples)

            source_year_dirs <- if (nzchar(resolved_class_path) &&
                                    dir.exists(resolved_class_path)) {
              roots <- list.dirs(resolved_class_path,
                                 recursive = FALSE,
                                 full.names = FALSE)
              sum(grepl("^class\\d{4}(_|$)", roots, ignore.case = TRUE))
            } else {
              NA_integer_
            }

            sample_example <- if (length(requested_samples) > 0) {
              requested_samples[[1]]
            } else {
              ""
            }
            missing_example <- if (length(missing_local) > 0) {
              missing_local[[1]]
            } else {
              ""
            }
            example_source_candidate <- if (nzchar(missing_example)) {
              year <- substr(missing_example, 2, 5)
              file.path(
                resolved_class_path,
                paste0("class", year, "_v3"),
                paste0(missing_example, "_class.h5")
              )
            } else {
              ""
            }
            msg <- paste0(
              "No classifications found.\n",
              "Source path: ", config$classification_path, "\n",
              "Resolved source path: ", resolved_class_path, "\n",
              "Source year folders (classYYYY*): ",
              if (is.na(source_year_dirs)) "path missing/unreadable" else source_year_dirs, "\n",
              "Local copied *_class*.h5 files: ", local_n, "\n",
              "Requested sample IDs: ", length(requested_samples), "\n",
              "Requested IDs found locally: ", length(matched_local), "\n",
              if (nzchar(sample_example)) {
                paste0("Example expected sample ID: ", sample_example, "\n")
              } else {
                ""
              },
              if (nzchar(missing_example)) {
                paste0("Example requested ID missing locally: ", missing_example, "\n")
              } else {
                ""
              },
              if (nzchar(example_source_candidate)) {
                paste0("Example expected source file: ", example_source_candidate, "\n")
              } else {
                ""
              },
              "Check that file basenames match sample IDs from metadata ",
              "(e.g. DYYYYMMDDTHHMMSS_IFCB###_class.h5)."
            )
            status(msg)
            shiny::showNotification(
              "No classifications found (see sidebar status for details)",
              type = "warning", duration = 10
            )
            return()
          }

          rv$matched_metadata_all <- matched
          rv$matched_metadata <- matched
          rv$classifications_raw_all <- proc$classifications_raw
          rv$classifications_raw <- proc$classifications_raw
          rv$classifications_all      <- proc$classifications
          rv$classifications          <- proc$classifications
          rv$classifications_original <- proc$classifications
          rv$invalidated_classes      <- proc$non_bio_classes
          rv$taxa_lookup <- proc$taxa_lookup
          rv$classifier_name <- proc$classifier_name
          rv$biovolume_cache <- proc$biovolume_cache
          rv$diatom_status <- proc$diatom_status
          rv$excluded_samples <- character(0)
          reset_corrections_state(rv)
          rv$frontpage_baltic_mosaic <- NULL
          rv$frontpage_westcoast_mosaic <- NULL

          # Build descriptive cruise info from sample dates
          rv$cruise_info <- build_cruise_info(matched$sample_time)

          # Step 4a: Fetch cruise-wide image counts
          shiny::incProgress(0.05, detail = "Fetching image counts...")
          sample_dates <- as.Date(matched$sample_time)
          rv$image_counts_all <- fetch_image_counts(
            config$dashboard_url, config$dashboard_dataset,
            min(sample_dates), max(sample_dates)
          )
          rv$image_counts <- rv$image_counts_all

          # Step 4b: Ferrybox data
          shiny::incProgress(0.05, detail = "Collecting ferrybox data...")
          fb_result <- merge_ferrybox_data(config, matched,
                                           proc$station_summary)
          rv$station_summary <- fb_result$station_summary
          rv$ferrybox_data <- fb_result$ferrybox_data
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

          gallery_classes <- sort(unique(rv$classifications$class_name))
          rv$current_class_idx <- 1L
          rv$current_region <- "EAST"
          rv$data_loaded <- TRUE

          # Warn about near-empty bins (likely end-of-cruise cleaning-cycle
          # samples pulling distilled water). duration = NULL keeps the
          # notification on screen until the user closes it, so it isn't
          # missed while doing other things after loading. A fixed id makes
          # a reload replace the previous warning instead of stacking; when
          # a reload finds nothing, any stale warning is removed.
          near_empty <- detect_near_empty_bins(matched)
          if (nrow(near_empty) > 0) {
            shiny::showNotification(
              build_near_empty_warning(near_empty),
              type = "warning", duration = NULL, closeButton = TRUE,
              id = ns("near_empty_bins")
            )
          } else {
            shiny::removeNotification(ns("near_empty_bins"))
          }

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


#' Reset per-cruise validation state
#'
#' Clears the corrections log, user-added custom classes and the gallery
#' selection when new data is loaded, so corrections made on one cruise are
#' never carried into -- and exported or auto-saved together with -- the next
#' cruise loaded in the same session. Column structure is preserved.
#'
#' @param rv \code{shiny::reactiveValues} (or a list-like object) holding
#'   \code{corrections}, \code{custom_classes} and \code{selected_images}.
#' @return \code{rv}, invisibly, after modification.
#' @keywords internal
reset_corrections_state <- function(rv) {
  if (!is.null(rv$corrections)) {
    rv$corrections <- rv$corrections[0, , drop = FALSE]
  }
  if (!is.null(rv$custom_classes)) {
    rv$custom_classes <- rv$custom_classes[0, , drop = FALSE]
  }
  rv$selected_images <- character(0)
  invisible(rv)
}
