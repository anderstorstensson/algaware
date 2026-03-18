#' Settings Module UI
#'
#' @param id Module namespace ID.
#' @return A UI element.
#' @keywords internal
mod_settings_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::actionButton(ns("open_settings"), "Settings",
                        icon = shiny::icon("gear"),
                        class = "btn-outline-secondary btn-sm")
  )
}

#' Settings Module Server
#'
#' @param id Module namespace ID.
#' @param config Reactive values object for settings.
#' @return NULL (side effects only).
#' @keywords internal
mod_settings_server <- function(id, config) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observeEvent(input$open_settings, {
      shiny::showModal(shiny::modalDialog(
        title = "Settings",
        size = "l",
        easyClose = TRUE,
        footer = shiny::tagList(
          shiny::actionButton(ns("save_settings"), "Save",
                              class = "btn-primary"),
          shiny::modalButton("Cancel")
        ),

        shiny::fluidRow(
          shiny::column(6,
            shiny::h5("Data Sources"),
            shiny::textInput(ns("dashboard_url"), "Dashboard URL",
                             value = config$dashboard_url),
            shiny::textInput(ns("dashboard_dataset"), "Dashboard Dataset",
                             value = config$dashboard_dataset),
            shiny::textInput(ns("classification_path"),
                             "Classification Path (.h5 files)",
                             value = config$classification_path),
            shiny::textInput(ns("raw_data_path"), "Raw Data Path",
                             value = config$raw_data_path),
            shiny::textInput(ns("ferrybox_path"), "Ferrybox Data Path",
                             value = config$ferrybox_path)
          ),
          shiny::column(6,
            shiny::h5("Storage & Classes"),
            shiny::textInput(ns("local_storage_path"), "Local Storage Path",
                             value = config$local_storage_path),
            shiny::textInput(ns("db_folder"), "Database Folder (SQLite)",
                             value = config$db_folder),
            shiny::textInput(ns("non_biological_classes"),
                             "Non-biological Classes (comma-separated)",
                             value = config$non_biological_classes),
            shiny::textInput(ns("class_list_path"),
                             "Class List File (.txt)",
                             value = config$class_list_path),
            shiny::textInput(ns("annotator"), "Annotator Name",
                             value = config$annotator),
            shiny::hr(),
            shiny::h5("Report"),
            shiny::numericInput(ns("n_mosaic_taxa"),
                                "Number of mosaic taxa per region",
                                value = config$n_mosaic_taxa,
                                min = 1, max = 20),
            shiny::numericInput(ns("n_mosaic_images"),
                                "Images per mosaic",
                                value = config$n_mosaic_images,
                                min = 4, max = 100)
          )
        )
      ))
    })

    shiny::observeEvent(input$save_settings, {
      config$dashboard_url <- input$dashboard_url
      config$dashboard_dataset <- input$dashboard_dataset
      config$classification_path <- input$classification_path
      config$raw_data_path <- input$raw_data_path
      config$ferrybox_path <- input$ferrybox_path
      config$local_storage_path <- input$local_storage_path
      config$db_folder <- input$db_folder
      config$non_biological_classes <- input$non_biological_classes
      config$class_list_path <- input$class_list_path
      config$annotator <- input$annotator
      config$n_mosaic_taxa <- input$n_mosaic_taxa
      config$n_mosaic_images <- input$n_mosaic_images

      save_settings(shiny::reactiveValuesToList(config))
      shiny::removeModal()
      shiny::showNotification("Settings saved", type = "message")
    })
  })
}
