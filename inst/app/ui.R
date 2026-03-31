ui <- bslib::page_sidebar(
  title = tags$div(
    style = "display: flex; align-items: center; width: 100%;",
    tags$span(paste("AlgAware-IFCB", utils::packageVersion("algaware"))),
    tags$img(src = "logo.png", class = "navbar-logo")
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
    tags$script(src = "gallery.js")
  ),

  # Sidebar: workflow goes top-to-bottom: Settings -> Load Data -> Validate -> Report
  sidebar = bslib::sidebar(
    width = 350,

    mod_settings_ui("settings"),

    hr(),

    mod_data_loader_ui("data_loader"),

    hr(),

    # conditionalPanel() shows/hides UI based on a JavaScript expression.
    # "output.data_loaded" reads the reactive flag set in server.R, so these
    # sections only appear after data has been successfully loaded.
    conditionalPanel(
      condition = "output.data_loaded",
      mod_validation_ui("validation")
    ),

    conditionalPanel(
      condition = "output.data_loaded",
      mod_report_ui("report")
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
      "Front Page",
      icon = shiny::icon("file-image"),
      conditionalPanel(
        condition = "!output.data_loaded",
        div(class = "empty-state",
          shiny::icon("file-image"),
          h4("No data available"),
          p("Load data using the sidebar to design the front page mosaic.")
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
