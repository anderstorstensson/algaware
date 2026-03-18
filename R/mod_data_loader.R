#' Data Loader Module UI
#'
#' @param id Module namespace ID.
#' @return A UI element.
#' @keywords internal
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

#' Data Loader Module Server
#'
#' @param id Module namespace ID.
#' @param config Reactive values with settings.
#' @param rv Reactive values for app state.
#' @return NULL (side effects only).
#' @keywords internal
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
      shiny::req(config$dashboard_url)

      status("Fetching metadata from dashboard...")

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
        status(paste0("Error: ", e$message))
        shiny::showNotification(e$message, type = "error")
      })
    })

    # Load and process data
    shiny::observeEvent(input$load_data, {
      shiny::req(rv$dashboard_metadata)

      shiny::withProgress(message = "Loading data...", value = 0, {
        tryCatch({
          # Step 1: Filter metadata
          shiny::incProgress(0.05, detail = "Filtering metadata...")

          if (input$selection_mode == "cruise") {
            filtered <- filter_metadata(rv$dashboard_metadata,
                                        cruise = input$cruise_select)
            rv$cruise_info <- input$cruise_select
          } else {
            filtered <- filter_metadata(rv$dashboard_metadata,
                                        date_from = input$date_range[1],
                                        date_to = input$date_range[2])
            rv$cruise_info <- paste(input$date_range[1], "to",
                                    input$date_range[2])
          }

          if (nrow(filtered) == 0) {
            status("No bins found for selection.")
            shiny::showNotification("No bins found", type = "warning")
            return()
          }

          # Step 2: Match bins to stations
          shiny::incProgress(0.1, detail = "Matching bins to stations...")
          algaware_stations <- load_algaware_stations()
          matched <- match_bins_to_stations(filtered, algaware_stations)

          if (nrow(matched) == 0) {
            status("No bins matched to AlgAware stations.")
            shiny::showNotification("No bins near AlgAware stations",
                                    type = "warning")
            return()
          }

          rv$matched_metadata <- matched
          status(paste0("Matched ", nrow(matched), " bins to ",
                        length(unique(matched$STATION_NAME)), " stations."))

          sample_ids <- matched$pid
          storage <- config$local_storage_path
          raw_dir <- file.path(storage, "raw")
          feat_dir <- file.path(storage, "features")
          class_dir <- file.path(storage, "classified")

          # Step 3: Download raw data
          shiny::incProgress(0.15, detail = "Downloading raw data...")
          download_raw_data(config$dashboard_url, sample_ids, raw_dir)

          # Step 4: Download features
          shiny::incProgress(0.1, detail = "Downloading features...")
          feat_url <- config$dashboard_url
          if (nzchar(config$dashboard_dataset)) {
            feat_url <- paste0(sub("/+$", "", feat_url), "/",
                               config$dashboard_dataset, "/")
          }
          download_features(feat_url, sample_ids, feat_dir)

          # Step 5: Copy classification files
          shiny::incProgress(0.1, detail = "Copying classification files...")
          if (nzchar(config$classification_path)) {
            copy_classification_files(config$classification_path,
                                      sample_ids, class_dir)
          }

          # Step 6: Read classifications
          shiny::incProgress(0.1, detail = "Reading classifications...")
          classifications <- read_h5_classifications(class_dir, sample_ids)

          if (nrow(classifications) == 0) {
            status("No classifications found. Check classification path.")
            shiny::showNotification("No classifications found",
                                    type = "warning")
            return()
          }

          # Remove non-biological classes
          non_bio <- parse_non_bio_classes(config$non_biological_classes)
          rv$classifications_raw <- classifications
          rv$classifications <- apply_invalidation(classifications, non_bio)
          rv$invalidated_classes <- non_bio

          # Step 7: Summarize biovolumes
          shiny::incProgress(0.15, detail = "Summarizing biovolumes...")
          taxa_lookup <- load_taxa_lookup()
          rv$taxa_lookup <- taxa_lookup

          biovolume_data <- summarize_biovolumes(
            feat_dir, raw_dir, rv$classifications,
            taxa_lookup, non_bio
          )

          # Step 8: Aggregate per station
          shiny::incProgress(0.1, detail = "Aggregating station data...")
          station_summary <- aggregate_station_data(biovolume_data, matched)

          # Step 9: Ferrybox data
          shiny::incProgress(0.05, detail = "Collecting ferrybox data...")
          if (nzchar(config$ferrybox_path)) {
            fb_data <- collect_ferrybox_data(
              matched$sample_time, config$ferrybox_path
            )
            if (nrow(fb_data) > 0 && "chl" %in% names(fb_data)) {
              chl_summary <- stats::aggregate(
                chl ~ STATION_NAME + visit_date,
                data = merge(station_summary, fb_data,
                             by.x = "median_time",
                             by.y = "timestamp",
                             all.x = TRUE),
                FUN = mean, na.rm = TRUE
              )
              names(chl_summary)[3] <- "chl_mean"
              station_summary <- merge(station_summary, chl_summary,
                                       by = c("STATION_NAME", "visit_date"),
                                       all.x = TRUE)
            }
          }

          rv$station_summary <- station_summary

          # Step 10: Create wide summaries
          shiny::incProgress(0.05, detail = "Creating summaries...")
          rv$baltic_wide <- create_wide_summary(station_summary, "EAST")
          rv$westcoast_wide <- create_wide_summary(station_summary, "WEST")

          # Identify samples per region
          rv$baltic_samples <- matched$pid[matched$COAST == "EAST"]
          rv$westcoast_samples <- matched$pid[matched$COAST == "WEST"]

          # Resolve class list (DB > file > auto-generate from classifications)
          db_path <- get_db_path(config$db_folder)
          resolved_classes <- resolve_class_list(
            db_path, config$class_list_path
          )

          if (is.null(resolved_classes)) {
            # Auto-generate from taxa lookup + current classifications
            all_class_names <- sort(unique(c(
              taxa_lookup$clean_names,
              rv$classifications$class_name
            )))
            # Ensure "unclassified" is included
            all_class_names <- unique(c(all_class_names, "unclassified"))
            resolved_classes <- all_class_names

            # Persist to database
            save_global_class_list_db(db_path, resolved_classes)

            shiny::showNotification(
              paste0("Class list auto-generated from taxa lookup (",
                     length(resolved_classes), " classes). ",
                     "You can also set a class list file in Settings."),
              type = "message", duration = 8
            )
          }

          rv$class_list <- resolved_classes

          # Warn about unmatched classes
          observed_classes <- unique(rv$classifications$class_name)
          unmatched <- setdiff(observed_classes, resolved_classes)
          if (length(unmatched) > 0) {
            shiny::showNotification(
              paste0(length(unmatched), " class(es) in classifications not in ",
                     "class list: ", paste(utils::head(unmatched, 5),
                                           collapse = ", "),
                     if (length(unmatched) > 5) "..."),
              type = "warning", duration = 10
            )
          }

          # Gallery classes (non-biological excluded, unclassified excluded)
          gallery_classes <- sort(unique(rv$classifications$class_name))
          gallery_classes <- gallery_classes[gallery_classes != "unclassified"]
          rv$current_class_idx <- 1L
          rv$current_region <- "EAST"
          rv$data_loaded <- TRUE

          status(paste0("Data loaded successfully!\n",
                        nrow(matched), " bins, ",
                        length(unique(matched$STATION_NAME)), " stations, ",
                        length(gallery_classes), " classes, ",
                        length(resolved_classes), " in class list.\n",
                        "Proceed to 'Validate' tab."))

        }, error = function(e) {
          status(paste0("Error: ", e$message))
          shiny::showNotification(e$message, type = "error", duration = 10)
        })
      })
    })
  })
}
