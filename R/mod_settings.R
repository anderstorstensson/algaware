#' Settings Module UI
#'
#' @param id Module namespace ID.
#' @return A UI element.
#' @export
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
#' @export
mod_settings_server <- function(id, config) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Load SHARK station names once for the selectize input
    shark_stations <- tryCatch(
      load_shark_stations(verbose = FALSE)$STATION_NAME,
      error = function(e) character(0)
    )

    shiny::observeEvent(input$open_settings, {
      # Populate station name selectize with SHARK station names
      shiny::updateSelectizeInput(
        session, "new_station_name",
        choices = shark_stations,
        server = TRUE
      )
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
                             value = config$ferrybox_path),
            shiny::hr(),
            shiny::h5("Image & Biovolume"),
            shiny::numericInput(ns("pixels_per_micron"),
                                "Pixels per micron",
                                value = config$pixels_per_micron,
                                min = 0.1, max = 20, step = 0.1),
            shiny::hr(),
            shiny::h5("Report"),
            shiny::numericInput(ns("n_mosaic_taxa"),
                                "Number of mosaic taxa per region",
                                value = config$n_mosaic_taxa,
                                min = 1, max = 20),
            shiny::numericInput(ns("n_mosaic_images"),
                                "Images per mosaic",
                                value = config$n_mosaic_images,
                                min = 4, max = 100),
            shiny::checkboxInput(ns("include_class_mosaics"),
                                 "Include per-class mosaics in report",
                                 value = isTRUE(config$include_class_mosaics))
          ),
          shiny::column(6,
            shiny::h5("Storage & Classes"),
            shiny::textInput(ns("local_storage_path"), "Local Storage Path",
                             value = config$local_storage_path),
            shiny::textInput(ns("db_folder"),
                             "Database Folder (annotations.sqlite)",
                             value = config$db_folder),
            shiny::textInput(ns("non_biological_classes"),
                             "Non-biological Classes (comma-separated)",
                             value = config$non_biological_classes),
            shiny::textInput(ns("annotator"), "Annotator Name",
                             value = config$annotator),
            shiny::hr(),
            shiny::h5("Extra Stations"),
            shiny::p(style = "font-size: 11px; color: #666;",
              "Add stations from the SHARK station register."),
            shiny::selectizeInput(ns("new_station_name"), "Station Name",
                                  choices = NULL,
                                  options = list(
                                    placeholder = "Type to search...",
                                    maxOptions = 50
                                  )),
            shiny::radioButtons(ns("new_station_coast"), "Coast",
                                choices = c("EAST", "WEST"),
                                selected = "EAST", inline = TRUE),
            shiny::textInput(ns("new_station_short"), "Short Name"),
            shiny::actionButton(ns("add_station"), "Add Station",
                                class = "btn-outline-primary btn-sm"),
            shiny::hr(),
            shiny::uiOutput(ns("extra_stations_list"))
          )
        )
      ))
    })

    shiny::observeEvent(input$save_settings, {
      # Validate URL
      url <- input$dashboard_url
      if (nzchar(url) && !grepl("^https?://", url)) {
        shiny::showNotification(
          "Dashboard URL must start with http:// or https://",
          type = "error")
        return()
      }

      # Validate numeric inputs
      ppm <- input$pixels_per_micron
      if (is.na(ppm) || ppm <= 0) {
        shiny::showNotification(
          "Pixels per micron must be a positive number", type = "error")
        return()
      }
      n_taxa <- input$n_mosaic_taxa
      if (is.na(n_taxa) || n_taxa < 1) {
        shiny::showNotification(
          "Number of mosaic taxa must be at least 1", type = "error")
        return()
      }
      n_imgs <- input$n_mosaic_images
      if (is.na(n_imgs) || n_imgs < 1) {
        shiny::showNotification(
          "Images per mosaic must be at least 1", type = "error")
        return()
      }

      # Normalize file paths
      normalize_path <- function(p) {
        if (nzchar(p)) normalizePath(p, mustWork = FALSE) else p
      }

      config$dashboard_url <- url
      config$dashboard_dataset <- input$dashboard_dataset
      config$classification_path <- normalize_path(input$classification_path)
      config$raw_data_path <- normalize_path(input$raw_data_path)
      config$ferrybox_path <- normalize_path(input$ferrybox_path)
      config$local_storage_path <- normalize_path(input$local_storage_path)
      config$db_folder <- normalize_path(input$db_folder)
      config$non_biological_classes <- input$non_biological_classes
      config$annotator <- input$annotator
      config$pixels_per_micron <- ppm
      config$n_mosaic_taxa <- n_taxa
      config$n_mosaic_images <- n_imgs
      config$include_class_mosaics <- isTRUE(input$include_class_mosaics)

      save_settings(shiny::reactiveValuesToList(config))
      shiny::removeModal()
      shiny::showNotification("Settings saved", type = "message")
    })

    # Add extra station
    shiny::observeEvent(input$add_station, {
      station_name <- input$new_station_name
      coast <- input$new_station_coast
      short_name <- input$new_station_short

      if (!nzchar(station_name)) {
        shiny::showNotification("Select a station name", type = "warning")
        return()
      }
      if (!nzchar(short_name)) {
        short_name <- station_name
      }

      # Check not already added
      existing <- config$extra_stations
      existing_names <- vapply(existing, function(s) s$STATION_NAME, "")
      if (station_name %in% existing_names) {
        shiny::showNotification("Station already added", type = "warning")
        return()
      }

      new_entry <- list(
        STATION_NAME = station_name,
        COAST = coast,
        STATION_NAME_SHORT = short_name
      )
      config$extra_stations <- c(existing, list(new_entry))
      save_settings(shiny::reactiveValuesToList(config))

      # Clear inputs
      shiny::updateSelectizeInput(session, "new_station_name", selected = "")
      shiny::updateTextInput(session, "new_station_short", value = "")

      shiny::showNotification(
        paste0("Added station: ", station_name),
        type = "message"
      )
    })

    # Remove extra station.
    # The remove buttons use an onclick handler that calls
    # Shiny.setInputValue() directly from JavaScript (see extra_stations_list
    # renderer below). This is a common Shiny pattern for dynamic lists where
    # each item needs its own action button: instead of observing N separate
    # button IDs, we funnel all clicks into a single input with the item index.
    shiny::observeEvent(input$remove_station, {
      idx <- input$remove_station
      existing <- config$extra_stations
      if (idx >= 1 && idx <= length(existing)) {
        removed <- existing[[idx]]$STATION_NAME
        config$extra_stations <- existing[-idx]
        save_settings(shiny::reactiveValuesToList(config))
        shiny::showNotification(
          paste0("Removed station: ", removed),
          type = "message"
        )
      }
    })

    # Render extra stations list
    output$extra_stations_list <- shiny::renderUI({
      stations <- config$extra_stations
      if (length(stations) == 0) {
        return(shiny::p(
          style = "font-size: 11px; color: #999;",
          "No extra stations added."
        ))
      }

      items <- lapply(seq_along(stations), function(i) {
        s <- stations[[i]]
        shiny::div(
          class = "d-flex align-items-center gap-2 mb-1",
          style = "font-size: 12px;",
          shiny::span(
            style = "flex: 1;",
            shiny::strong(s$STATION_NAME_SHORT),
            shiny::span(style = "color: #666;",
              paste0(" (", s$STATION_NAME, ", ", s$COAST, ")"))
          ),
          # Each remove button uses onclick to call Shiny.setInputValue()
          # with the station index. {priority: 'event'} forces Shiny to
          # treat repeated clicks on the same index as new events.
          shiny::actionButton(
            ns(paste0("remove_station_", i)), "",
            icon = shiny::icon("xmark"),
            class = "btn-outline-danger btn-sm",
            onclick = paste0(
              "Shiny.setInputValue('", ns("remove_station"),
              "', ", i, ", {priority: 'event'});"
            )
          )
        )
      })

      shiny::tagList(items)
    })
  })
}
