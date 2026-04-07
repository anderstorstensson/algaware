# Shiny UI definition for AlgAware-IFCB
#
# Layout overview (bslib page_sidebar):
#   Sidebar (width = 350):
#     Settings accordion panel  -- mod_settings_ui
#     Data accordion panel      -- mod_data_loader_ui
#     Validate accordion panel  -- mod_validation_ui (conditionalPanel on data_loaded)
#     Report accordion panel    -- mod_report_ui + chlorophyll source selector
#
#   Main panel (navset_card_tab, id = "main_tabs"):
#     Validate  -- mod_gallery_ui  (image browsing and class navigation)
#     Samples   -- mod_samples_ui  (sample exclusion table)
#     Images    -- mod_frontpage_ui (front-page mosaic designer)
#     Maps      -- image_count_map, biomass_map, chl_map (rendered in server.R)
#     Plots     -- heatmaps + stacked bar charts (rendered in server.R)
#     CTD       -- mod_ctd_ui (CTD profile + Chl-a time-series plots)
#     Summary   -- DT summary table (rendered in server.R)
#
# All sidebar modules write to / read from the shared `rv` reactiveValues
# defined in server.R.  Conditional visibility is gated on `output.data_loaded`,
# which is a reactive in server.R that mirrors `rv$data_loaded`.

ui <- bslib::page_sidebar(
  title = tags$div(
    style = "display: flex; align-items: center; width: 100%;",
    tags$a(
      href = "https://nodc-sweden.github.io/ifcb-algaware/",
      target = "_blank",
      rel = "noopener noreferrer",
      tags$img(src = "logo.png", class = "navbar-logo")
    ),
    tags$a(
      href = "https://github.com/nodc-sweden/ifcb-algaware",
      target = "_blank",
      rel = "noopener noreferrer",
      class = "navbar-version-link",
      paste("AlgAware-IFCB", utils::packageVersion("algaware"))
    )
  ),
  theme = bslib::bs_theme(
    bootswatch = "flatly", version = 5,
    bg = "#fff", fg = "#333",
    primary = "#2c5f7c",
    "navbar-bg" = "#1b3a4b"
  ),

  tags$head(
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$script(HTML("document.title = 'AlgAware-IFCB';")),
    tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32",
              href = "favicon-32.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "16x16",
              href = "favicon-16.png"),
    tags$script(src = "gallery.js"),
    tags$script(HTML("
      (function() {
        var shown = Date.now();
        var minMs = 600;
        function hideOverlay() {
          var overlay = document.getElementById('app-loading-overlay');
          if (!overlay) return;
          var elapsed = Date.now() - shown;
          var delay = Math.max(0, minMs - elapsed);
          setTimeout(function() {
            overlay.classList.add('fade-out');
            setTimeout(function() { overlay.remove(); }, 450);
          }, delay);
        }
        $(document).one('shiny:idle', hideOverlay);
      })();
    "))
  ),

  tags$div(
    id = "app-loading-overlay",
    tags$div(class = "app-loading-spinner"),
    tags$div(class = "app-loading-text", "Starting AlgAware...")
  ),

  # Sidebar: workflow accordion (Settings -> Data -> Validate -> Report)
  # The "Validate" and "Report" panels auto-open from the server once data loads.
  sidebar = bslib::sidebar(
    width = 350,

    bslib::accordion(
      id = "sidebar_accordion",
      multiple = TRUE,
      open = "data",

      bslib::accordion_panel(
        "Settings",
        value = "settings",
        icon = shiny::icon("gear"),
        mod_settings_ui("settings")
      ),

      bslib::accordion_panel(
        "Data",
        value = "data",
        icon = shiny::icon("database"),
        mod_data_loader_ui("data_loader")
      ),

      bslib::accordion_panel(
        "Validate",
        value = "validate",
        icon = shiny::icon("check-circle"),
        conditionalPanel(
          condition = "output.data_loaded",
          mod_validation_ui("validation")
        ),
        conditionalPanel(
          condition = "!output.data_loaded",
          tags$p(class = "text-muted small mb-0", "Load data first.")
        )
      ),

      bslib::accordion_panel(
        "Report",
        value = "report",
        icon = shiny::icon("file-word"),
        conditionalPanel(
          condition = "output.data_loaded",
          selectInput("chl_map_source", "Chlorophyll source",
                      choices = c("FerryBox" = "ferrybox"),
                      selected = "ferrybox",
                      width = "100%"),
          mod_report_ui("report")
        ),
        conditionalPanel(
          condition = "!output.data_loaded",
          tags$p(class = "text-muted small mb-0", "Load data first.")
        )
      )
    )
  ),

  # Main panel with tabs
  bslib::navset_card_tab(
    id = "main_tabs",

    bslib::nav_panel(
      "Validate",
      icon = shiny::icon("images"),
      mod_gallery_ui("gallery")
    ),

    bslib::nav_panel(
      "Samples",
      icon = shiny::icon("filter"),
      conditionalPanel(
        condition = "!output.data_loaded",
        div(class = "empty-state",
          shiny::icon("filter"),
          h4("No samples available"),
          p("Load data using the sidebar to review and exclude samples.")
        )
      ),
      conditionalPanel(
        condition = "output.data_loaded",
        mod_samples_ui("samples")
      )
    ),

    bslib::nav_panel(
      "Images",
      icon = shiny::icon("file-image"),
      conditionalPanel(
        condition = "!output.data_loaded",
        div(class = "empty-state",
          shiny::icon("file-image"),
          h4("No data available"),
          p("Load data using the sidebar to design image mosaics.")
        )
      ),
      conditionalPanel(
        condition = "output.data_loaded",
        mod_frontpage_ui("frontpage")
      )
    ),

    bslib::nav_panel(
      "Maps",
      icon = shiny::icon("map"),
      conditionalPanel(
        condition = "!output.data_loaded",
        div(class = "empty-state",
          shiny::icon("map"),
          h4("No map data available"),
          p("Load data using the sidebar to view station maps.")
        )
      ),
      conditionalPanel(
        condition = "output.data_loaded",
        div(class = "plot-card", plotOutput("image_count_map",
                                            height = "400px")),
        div(class = "plot-card", plotOutput("biomass_map",
                                            height = "400px")),
        div(class = "plot-card", plotOutput("chl_map", height = "400px"))
      )
    ),

    bslib::nav_panel(
      "Plots",
      icon = shiny::icon("chart-bar"),
      conditionalPanel(
        condition = "!output.data_loaded",
        div(class = "empty-state",
          shiny::icon("chart-bar"),
          h4("No plot data available"),
          p("Load data using the sidebar to view heatmaps and bar charts.")
        )
      ),
      conditionalPanel(
        condition = "output.data_loaded",
        div(
          class = "plots-container",
          div(class = "plot-card",
            h5("Baltic Sea"),
            plotOutput("baltic_heatmap", height = "800px"),
            plotOutput("baltic_stacked_bar", height = "600px")
          ),
          div(class = "plot-card",
            h5("West Coast"),
            plotOutput("westcoast_heatmap", height = "800px"),
            plotOutput("westcoast_stacked_bar", height = "600px")
          )
        )
      )
    ),

    bslib::nav_panel(
      shiny::uiOutput("ctd_tab_title", inline = TRUE),
      icon = shiny::icon("water"),
      mod_ctd_ui("ctd")
    ),

    bslib::nav_panel(
      "Summary",
      icon = shiny::icon("table"),
      conditionalPanel(
        condition = "!output.data_loaded",
        div(class = "empty-state",
          shiny::icon("table"),
          h4("No summary data available"),
          p("Load data using the sidebar to view the summary table.")
        )
      ),
      conditionalPanel(
        condition = "output.data_loaded",
        DT::DTOutput("summary_table")
      )
    )
  )
)
