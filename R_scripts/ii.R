library(shiny)
library(shinyMobile)
library(shinyTime)
library(RPostgreSQL)
library(DBI)
library(dplyr)
library(tidyr)
library(exifr)
library(DT)
library(leaflet)
library(leaflet.extras)
library(htmltools)

# Source external R scripts
source("R/functions.R")
# source("R/ui_tabs.R")
source("R/static.R")
source("R/server_logic.R")

# Define UI
ui <- navbarPage(
  title = tags$div(
    class = "navbar-right",
    tags$img(src = "tunisia-flag-round-shape-png.png"),
    "Tunisia Fisheries App"
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(HTML("
      $(document).on('click', 'a[href^=\"#\"]', function(event) {
        event.preventDefault();
        var target = this.getAttribute('href').substring(1);
        $('a[data-value=\"' + target + '\"]').tab('show');
      });
    "))
  ),
  # Tabs are now sourced from an external file
  ui_home_tab,
  ui_new_catch_tab,
  ui_maps_tab,
  ui_sustainable_practices_tab
)

# Define Server
server <- function(input, output, session) {

  output$fishing_activity_map <- renderUI({
      tags$iframe(src = "fishing_activity_map.html", width = "100%", height = "600px")
    })
  
  output$fishing_effort_grid <- renderUI({
    tags$iframe(src = "fishing_effort_grid.html", width = "100%", height = "600px")
  })

  # Server-side selectize for species
  # updateSelectizeInput(session, 'species', choices = fetchSpeciesNames(), server = TRUE)
  updateSelectizeInput(session, 'port', choices = c("", as.character(tunisian_cities$port), "Other"), server = TRUE)
  updateSelectizeInput(session, 'gear', choices = c("", as.character(gear_list$gear), "Other"), server = TRUE)

observeEvent(input$submit, {
      session$reload()
      # Initialize lat and lon with NA
      lat = if (!is.null(input$latitude)) input$latitude else NA
      lon = if (!is.null(input$longitude)) input$longitude else NA
      imageName <- NA
      port = if (!is.null(input$port) && input$port != "") input$port else NA
      weight = if (!is.null(input$weight) && input$weight != "") input$port else NA
      gear = if (!is.null(input$gear) && input$gear != "") input$gear else NA
      angler = if (!is.null(input$angler) && input$angler != "") input$angler else NA
      boat = if (!is.null(input$boat) && input$boat != "") input$boat else NA
      notes = if (!is.null(input$notes) && input$notes != "") input$notes else NA

      if (!is.null(input$file)) {
        uploadedFile <- input$file
        destPath <- paste0(getwd(), "/www/", uploadedFile$name)
        file.rename(uploadedFile$datapath, destPath)
        imageName <- as.character(uploadedFile$name)
        
        # Attempt to extract EXIF data
        exifData <- read_exif(destPath)
        
        # Check if GPSLatitude and GPSLongitude exist in the exifData
        if ("GPSLatitude" %in% names(exifData) && "GPSLongitude" %in% names(exifData)) {
          lat <- exifData$GPSLatitude
          lon <- exifData$GPSLongitude
        }
      }

      new_entry <- data.frame(
        date = as.Date(input$catch_date),
        port = port,
        species_richness = as.numeric(input$species_richness),
        weight = as.numeric(weight),
        img_name = imageName,
        gear = gear,
        fishing_hours = as.numeric(input$fishing_hours),
        latitude = lat,
        longitude = lon,
        angler = angler,
        boat = boat,
        notes = notes,
        ais = input$ais,
        stringsAsFactors = FALSE
      )
      write.csv(new_entry, "/srv/shiny-server/comanage/test.csv", row.names = FALSE)

      con <- connectPelagic()
      tryCatch({
        dbWriteTable(con, "tun_catch", new_entry, append = TRUE, row.names = FALSE)
        dbDisconnect(con)
        shiny::showNotification("Catch logged successfully!", type = "message")
      }, error = function(e) {
        shiny::showNotification(as.character(e), type = "error")
      })
    })

output$map <- renderLeaflet({
  leaflet() %>%
    addTiles() %>%
    setView(lng = 11, lat = 35.8, zoom = 6.5) %>% # Centered around the Tunisian coast
    addDrawToolbar(
      targetGroup = 'selected',
      editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()),
      polylineOptions = FALSE,
      polygonOptions = FALSE,
      rectangleOptions = FALSE,
      circleOptions = FALSE,
      markerOptions = drawMarkerOptions(repeatMode = FALSE),
      circleMarkerOptions = FALSE
    ) %>%
    addLayersControl(
      overlayGroups = c('selected'),
      options = layersControlOptions(collapsed = FALSE)
    )
})

observeEvent(input$map_draw_new_feature, {
  # Remove existing markers
  leafletProxy("map") %>% clearGroup("selected")
  
  feature <- input$map_draw_new_feature
  if (!is.null(feature)) {
    # Ensure coordinates are numeric
    lat <- as.numeric(feature$geometry$coordinates[2])
    lon <- as.numeric(feature$geometry$coordinates[1])
    updateNumericInput(session, "latitude", value = lat)
    updateNumericInput(session, "longitude", value = lon)
    
    # Add the new marker
    leafletProxy("map") %>% addMarkers(
      lng = lon, lat = lat, group = "selected"
    )
  }
})

}
# Run the app
shinyApp(ui, server)
