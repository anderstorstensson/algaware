server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # Shared reactive state (rv)
  #
  # This is the central data store that all Shiny modules read from and write
  # to. In Shiny, `reactiveValues` is like a named list where any change to a
  # value automatically triggers re-execution of code that depends on it.
  #
  # Data flow overview:
  #   1. mod_data_loader  -> sets most rv fields after fetching & processing
  #   2. mod_gallery      -> reads classifications, writes selected_images
  #   3. mod_validation   -> reads/writes classifications & corrections
  #   4. mod_report       -> reads all fields to generate the Word report
  # ---------------------------------------------------------------------------
  rv <- reactiveValues(
    # -- Data loading stage (set by mod_data_loader) --
    dashboard_metadata  = NULL,          # Raw metadata from IFCB Dashboard API
    cruise_numbers      = character(0),  # Available cruise IDs for dropdown
    matched_metadata_all = NULL,         # Full matched metadata before exclusions
    matched_metadata    = NULL,          # Metadata filtered & matched to stations
    classifications_raw_all = NULL,      # Full original AI predictions
    classifications_raw = NULL,          # Original AI predictions (immutable)
    classifications_all      = NULL,     # Full working copy across all samples
    classifications          = NULL,     # Working copy (mutated by validation)
    classifications_original = NULL,     # Immutable snapshot at load time (for import reset)
    invalidated_classes = character(0),  # Classes marked as non-biological
    taxa_lookup         = NULL,          # Mapping: class_name -> scientific name
    station_summary     = NULL,          # Aggregated biovolume per station/taxon
    baltic_wide         = NULL,          # Wide-format summary for Baltic region
    westcoast_wide      = NULL,          # Wide-format summary for West Coast
    baltic_samples      = character(0),  # Sample IDs belonging to Baltic
    westcoast_samples   = character(0),  # Sample IDs belonging to West Coast
    class_list          = character(0),  # Approved class names for annotation
    ferrybox_data       = NULL,          # Raw ferrybox rows for active cruise samples
    ferrybox_chl        = NULL,          # Chlorophyll data from ferrybox sensors
    image_counts_all    = NULL,          # Cruise image counts before exclusions
    image_counts        = NULL,          # Per-sample image counts (cruise-wide)
    cruise_info         = "",            # Human-readable cruise description
    classifier_name     = NULL,          # Name of the AI classifier model

    # -- Extended class list for relabeling --
    relabel_choices     = list(),         # Grouped choices for relabel dropdowns
    custom_classes = data.frame(          # User-added custom classes (session-only)
      clean_names = character(0),
      name        = character(0),
      sflag       = character(0),
      AphiaID     = integer(0),
      HAB         = logical(0),
      italic      = logical(0),
      is_diatom   = logical(0),
      stringsAsFactors = FALSE
    ),

    # -- Gallery & validation state (shared between modules) --
    current_class_idx   = 1L,            # Index into the current region's class list
    current_region      = "EAST",        # "EAST" (Baltic) or "WEST" (West Coast)
    selected_images     = character(0),  # Image IDs selected in gallery for action
    corrections = data.frame(            # Log of all user corrections this session
      sample_name    = character(0),
      roi_number     = integer(0),
      original_class = character(0),
      new_class      = character(0),
      stringsAsFactors = FALSE
    ),

    # -- Front page mosaics (set by mod_frontpage) --
    frontpage_baltic_mosaic   = NULL,    # Magick image for front page Baltic mosaic
    frontpage_westcoast_mosaic = NULL,   # Magick image for front page West Coast mosaic

    # -- CTD / LIMS state (set by mod_ctd) --
    ctd_data            = NULL,          # AlgAware-matched CTD profiles (for chl map)
    ctd_data_full       = NULL,          # All standard-station CTD profiles (for CTD tab)
    lims_data           = NULL,          # AlgAware-matched LIMS Chl-a (for chl map)
    lims_data_full      = NULL,          # All standard-station LIMS data (for CTD tab)
    chl_stats           = NULL,          # Historical Chl-a statistics (bundled)
    ctd_loaded          = FALSE,         # TRUE once CTD data loaded
    chl_map_source      = "ferrybox",   # Active chl map source for report

    # -- App state flags --
    excluded_samples = character(0),     # Loaded samples excluded from analysis/report
    data_loaded = FALSE,                 # TRUE once data loading completes
    summaries_stale = FALSE              # TRUE when classifications changed but summaries not yet recomputed
  )

  # ---------------------------------------------------------------------------
  # Config (persistent settings loaded from JSON on disk)
  # ---------------------------------------------------------------------------
  settings <- load_settings()
  config <- do.call(reactiveValues, settings)

  # ---------------------------------------------------------------------------
  # Conditional UI visibility
  #
  # Shiny's conditionalPanel() evaluates a JavaScript expression to show/hide
  # UI elements. Here we expose `data_loaded` as an output so the JS condition
  # "output.data_loaded" works. `suspendWhenHidden = FALSE` ensures it stays
  # up-to-date even when no one is looking at it.
  # ---------------------------------------------------------------------------
  output$data_loaded <- reactive({ isTRUE(rv$data_loaded) })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)

  # ---------------------------------------------------------------------------
  # Initialize Shiny modules
  #
  # Each mod_*_server() function runs in its own namespace (the string ID
  # matches the corresponding mod_*_ui() call in ui.R). Modules communicate
  # via the shared `rv` and `config` reactive values passed as arguments.
  # ---------------------------------------------------------------------------
  mod_settings_server("settings", config)
  mod_data_loader_server("data_loader", config, rv)
  mod_gallery_server("gallery", rv, config)
  mod_validation_server("validation", rv, config)
  mod_samples_server("samples", rv, config)
  mod_frontpage_server("frontpage", rv, config)
  # `phyto_group_assignments` is defined further down; wrap in a closure so the
  # lookup happens at call time, not when the module is initialised.
  mod_report_server("report", rv, config,
                    phyto_groups_reactive = function() phyto_group_assignments())
  mod_ctd_server("ctd", config, rv)

  # CTD tab title: show "loaded" badge once CTD data is available
  output$ctd_tab_title <- shiny::renderUI({
    if (isTRUE(rv$ctd_loaded)) {
      shiny::tagList("CTD ", shiny::span(class = "badge bg-success ms-1", "loaded"))
    } else {
      "CTD"
    }
  })

  # Auto-open the Validate accordion panel after data loads so buttons are
  # immediately visible without the user having to manually expand the panel.
  shiny::observeEvent(rv$data_loaded, {
    shiny::req(isTRUE(rv$data_loaded))
    bslib::accordion_panel_open("sidebar_accordion", values = "validate",
                                session = session)
  }, ignoreInit = TRUE)

  compute_ferrybox_summary <- function(matched_metadata, ferrybox_data) {
    if (is.null(matched_metadata) || nrow(matched_metadata) == 0 ||
        is.null(ferrybox_data) || nrow(ferrybox_data) == 0 ||
        !"chl" %in% names(ferrybox_data)) {
      return(NULL)
    }

    matched_fb <- merge(
      matched_metadata[, c("pid", "STATION_NAME", "sample_time")],
      ferrybox_data[, c("timestamp", "chl")],
      by.x = "sample_time",
      by.y = "timestamp",
      all.x = TRUE
    )

    if (nrow(matched_fb) == 0) {
      return(NULL)
    }

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
    chl_summary
  }

  recompute_summaries <- function() {
    id <- showNotification("Updating summaries...", type = "message",
                           duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)

    if (is.null(rv$matched_metadata) || nrow(rv$matched_metadata) == 0 ||
        is.null(rv$classifications) || nrow(rv$classifications) == 0) {
      rv$ferrybox_chl <- NULL
      rv$station_summary <- NULL
      rv$baltic_wide <- data.frame(scientific_name = character(0))
      rv$westcoast_wide <- data.frame(scientific_name = character(0))
      rv$summaries_stale <- FALSE
      return(invisible(NULL))
    }

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

    rv$ferrybox_chl <- compute_ferrybox_summary(rv$matched_metadata,
                                                rv$ferrybox_data)

    # Re-attach ferrybox chlorophyll data
    if (!is.null(rv$ferrybox_chl)) {
      station_summary <- merge(station_summary, rv$ferrybox_chl,
                               by = "STATION_NAME", all.x = TRUE)
    }

    rv$station_summary <- station_summary
    rv$baltic_wide <- create_wide_summary(station_summary, "EAST")
    rv$westcoast_wide <- create_wide_summary(station_summary, "WEST")
    rv$summaries_stale <- FALSE
  }

  refresh_active_dataset <- function() {
    if (!isTRUE(rv$data_loaded) || is.null(rv$matched_metadata_all)) {
      return(invisible(NULL))
    }

    active_ids <- setdiff(unique(rv$matched_metadata_all$pid), rv$excluded_samples)

    rv$matched_metadata <- rv$matched_metadata_all[
      rv$matched_metadata_all$pid %in% active_ids, ,
      drop = FALSE
    ]
    rv$classifications_raw <- rv$classifications_raw_all[
      rv$classifications_raw_all$sample_name %in% active_ids, ,
      drop = FALSE
    ]
    rv$classifications <- rv$classifications_all[
      rv$classifications_all$sample_name %in% active_ids, ,
      drop = FALSE
    ]
    rv$baltic_samples <- rv$matched_metadata$pid[rv$matched_metadata$COAST == "EAST"]
    rv$westcoast_samples <- rv$matched_metadata$pid[rv$matched_metadata$COAST == "WEST"]

    if (!is.null(rv$image_counts_all) && nrow(rv$image_counts_all) > 0) {
      rv$image_counts <- rv$image_counts_all[
        !rv$image_counts_all$pid %in% rv$excluded_samples, ,
        drop = FALSE
      ]
    } else {
      rv$image_counts <- rv$image_counts_all
    }

    rv$cruise_info <- if (!is.null(rv$matched_metadata) &&
                          nrow(rv$matched_metadata) > 0) {
      build_cruise_info(rv$matched_metadata$sample_time)
    } else {
      ""
    }

    rv$frontpage_baltic_mosaic <- NULL
    rv$frontpage_westcoast_mosaic <- NULL

    recompute_summaries()
    invisible(NULL)
  }

  observeEvent(rv$excluded_samples, {
    req(rv$data_loaded, rv$matched_metadata_all)
    refresh_active_dataset()
  }, ignoreInit = TRUE)

  observeEvent(input$main_tabs, {
    req(isTRUE(rv$summaries_stale), rv$data_loaded, rv$matched_metadata)
    if (input$main_tabs == "Validate") return()
    recompute_summaries()
  })

  # Cached biomass maps (invalidates when station_summary changes)
  biomass_maps <- reactive({
    req(rv$station_summary)
    create_biomass_maps(rv$station_summary)
  })

  # Phytoplankton group assignments (queries WoRMS via SHARK4R).
  # bindCache() keys on the sorted unique set of name+AphiaID pairs, so the
  # API call only reruns when the actual species list changes — not when
  # biovolume values change (e.g. after sample exclusions or corrections).
  # The cache is shared across sessions in the same R process.
  phyto_group_assignments <- reactive({
    req(rv$station_summary)
    taxa <- unique(rv$station_summary[, c("name", "AphiaID")])
    taxa <- taxa[!is.na(taxa$name), ]
    
    groups <- tryCatch(
      algaware::assign_phyto_groups(
        scientific_names = taxa$name,
        aphia_ids        = taxa$AphiaID
      ),
      error = function(e) rep("Other", nrow(taxa))
    )
    data.frame(
      name        = taxa$name,
      AphiaID     = taxa$AphiaID,
      phyto_group = groups,
      stringsAsFactors = FALSE
    )
  }) |> bindCache({
    req(rv$station_summary)
    taxa <- rv$station_summary[!is.na(rv$station_summary$name),
                               c("name", "AphiaID")]
    sort(unique(paste0(taxa$name, "_", taxa$AphiaID)))
  })

  # Maps
  output$image_count_map <- renderPlot({
    req(rv$image_counts, nrow(rv$image_counts) > 0)
    create_image_count_map(rv$image_counts)
  })

  # Update chl map source choices when CTD/LIMS data becomes available.
  # Priority: Hose > Bottle (0-20m) > CTD > FerryBox — auto-select best source.
  observe({
    choices <- c("FerryBox" = "ferrybox")
    if (isTRUE(rv$ctd_loaded) && !is.null(rv$ctd_data)) {
      choices <- c(choices, "CTD (0-20 m)" = "ctd")
    }
    if (!is.null(rv$lims_data) && nrow(rv$lims_data) > 0) {
      choices <- c(choices, "LIMS bottle (0-20 m)" = "lims")
      if ("SMPNO" %in% names(rv$lims_data) &&
          any(grepl("-SLA_", rv$lims_data$SMPNO, fixed = TRUE), na.rm = TRUE)) {
        choices <- c(choices, "LIMS hose (0-10 m)" = "lims_hose")
      }
    }
    # Auto-select best available source
    best <- if ("lims_hose" %in% choices) "lims_hose"
            else if ("lims" %in% choices) "lims"
            else if ("ctd" %in% choices) "ctd"
            else "ferrybox"
    updateSelectInput(session, "chl_map_source",
                      choices = choices, selected = best)
  })

  # Sync the selector to rv for report access
  observe({
    rv$chl_map_source <- input$chl_map_source %||% "ferrybox"
  })

  output$chl_map <- renderPlot({
    source <- input$chl_map_source %||% "ferrybox"
    if (source == "ctd" && !is.null(rv$ctd_data)) {
      create_chl_map(compute_ctd_chl_avg(rv$ctd_data),
                     title = "CTD chlorophyll fluorescence (0-20 m avg)")
    } else if (source == "lims" && !is.null(rv$lims_data)) {
      create_chl_map(compute_lims_chl_avg(rv$lims_data),
                     title = "Bottle Chl-a (0-20 m avg)")
    } else if (source == "lims_hose" && !is.null(rv$lims_data)) {
      create_chl_map(compute_lims_hose_avg(rv$lims_data),
                     title = "Hose Chl-a (0-10 m integrated)")
    } else {
      biomass_maps()$chl_map
    }
  })

  output$group_map <- renderPlot({
    req(rv$station_summary)
    create_group_map(rv$station_summary, phyto_group_assignments())
  })

  # Heatmaps
  output$baltic_heatmap <- renderPlot({
    req(rv$baltic_wide, ncol(rv$baltic_wide) > 1)
    create_heatmap(rv$baltic_wide, taxa_lookup = rv$taxa_lookup,
                   title = "Baltic Sea")
  })

  output$westcoast_heatmap <- renderPlot({
    req(rv$westcoast_wide, ncol(rv$westcoast_wide) > 1)
    create_heatmap(rv$westcoast_wide, taxa_lookup = rv$taxa_lookup,
                   title = "West Coast")
  })

  # Stacked bar charts
  output$baltic_stacked_bar <- renderPlot({
    req(rv$baltic_wide, ncol(rv$baltic_wide) > 1)
    create_stacked_bar(rv$baltic_wide, taxa_lookup = rv$taxa_lookup,
                       title = "Baltic Sea")
  })

  output$westcoast_stacked_bar <- renderPlot({
    req(rv$westcoast_wide, ncol(rv$westcoast_wide) > 1)
    create_stacked_bar(rv$westcoast_wide, taxa_lookup = rv$taxa_lookup,
                       title = "West Coast")
  })

  # Summary table
  output$summary_table <- DT::renderDT({
    req(rv$station_summary)
    display <- rv$station_summary[, c(
      "STATION_NAME_SHORT", "COAST", "visit_date", "name",
      "counts_per_liter", "biovolume_mm3_per_liter", "carbon_ug_per_liter"
    )]
    names(display) <- c("Station", "Region", "Date", "Taxon",
                         "Counts/L", "Biovolume mm3/L", "Carbon ug/L")
    DT::datatable(display,
                  options = list(pageLength = 25, scrollX = TRUE),
                  rownames = FALSE) |>
      DT::formatRound(c("Counts/L", "Biovolume mm3/L", "Carbon ug/L"), 3)
  })
}
