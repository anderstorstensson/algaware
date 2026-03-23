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
    matched_metadata    = NULL,          # Metadata filtered & matched to stations
    classifications_raw = NULL,          # Original AI predictions (immutable)
    classifications     = NULL,          # Working copy (mutated by validation)
    invalidated_classes = character(0),  # Classes marked as non-biological
    taxa_lookup         = NULL,          # Mapping: class_name -> scientific name
    station_summary     = NULL,          # Aggregated biovolume per station/taxon
    baltic_wide         = NULL,          # Wide-format summary for Baltic region
    westcoast_wide      = NULL,          # Wide-format summary for West Coast
    baltic_samples      = character(0),  # Sample IDs belonging to Baltic
    westcoast_samples   = character(0),  # Sample IDs belonging to West Coast
    class_list          = character(0),  # Approved class names for annotation
    ferrybox_chl        = NULL,          # Chlorophyll data from ferrybox sensors
    image_counts        = NULL,          # Per-sample image counts (cruise-wide)
    cruise_info         = "",            # Human-readable cruise description
    classifier_name     = NULL,          # Name of the AI classifier model

    # -- Extended class list for relabeling --
    relabel_choices     = list(),         # Grouped choices for relabel dropdowns
    custom_classes = data.frame(          # User-added custom classes (session-only)
      clean_names = character(0),
      name        = character(0),
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

    # -- App state flag --
    data_loaded = FALSE                  # TRUE once data loading completes
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
  mod_report_server("report", rv, config)

  # Cached biomass maps (invalidates when station_summary changes)
  biomass_maps <- reactive({
    req(rv$station_summary)
    create_biomass_maps(rv$station_summary)
  })

  # Maps
  output$image_count_map <- renderPlot({
    req(rv$image_counts, nrow(rv$image_counts) > 0)
    create_image_count_map(rv$image_counts)
  })

  output$biomass_map <- renderPlot({
    id <- showNotification("Generating maps...", type = "message",
                           duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    biomass_maps()$biomass_map
  })

  output$chl_map <- renderPlot({
    biomass_maps()$chl_map
  })

  # Heatmaps
  output$baltic_heatmap <- renderPlot({
    req(rv$baltic_wide, ncol(rv$baltic_wide) > 1)
    id <- showNotification("Generating plots...", type = "message",
                           duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
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
