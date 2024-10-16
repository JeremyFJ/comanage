require(dplyr)
require(rinat)
require(rgbif)
require(leaflet)
require(sf)
require(htmlwidgets)
require(leaflet.extras)  # For the slider and extra map features

# Set working directory
setwd("/srv/shiny-server/comanage/R_scripts/")
source("/var/www/html/sharkPulse/sp-iNat/s/get_inat_obs2.R")

# Bounding box for the Mediterranean Sea
med_bounds <- list(swlat = 30.0, swlng = -6.0, nelat = 46.0, nelng = 36.0)

# Function to filter data for Mediterranean sightings
filter_for_mediterranean <- function(data, lat_col, lon_col) {
  data %>%
    filter(!!sym(lat_col) >= med_bounds$swlat,
           !!sym(lat_col) <= med_bounds$nelat,
           !!sym(lon_col) >= med_bounds$swlng,
           !!sym(lon_col) <= med_bounds$nelng)
}

# TryCatch block to source and combine data
tryCatch({
  blue_crab_data <- get_inat_obs2(
    taxon_name = "Portunus segnis",  # Scientific name of the blue crab
    geo = TRUE,                          # Ensure only georeferenced observations are returned
    bounds = med_bounds,       # Mediterranean Sea bounds
    maxresults = 10000                   # Adjust maxresults as needed
  )

  blue_crab_data2 <- get_inat_obs2(
    taxon_name = "Callinectes sapidus",
    geo = TRUE,                          # Ensure only georeferenced observations are returned
    bounds = med_bounds,       # Mediterranean Sea bounds
    maxresults = 10000                   # Adjust maxresults as needed
  )
  blue_crab_data <- rbind(blue_crab_data, blue_crab_data2)

  # Step 2: Clean and standardize blue_crab_data
  blue_crab_clean <- blue_crab_data %>%
    filter(!is.na(latitude) & !is.na(longitude)) %>%
    select(latitude, longitude, datetime, scientific_name, url, image_url) %>%
    rename(decimalLatitude = latitude,
           decimalLongitude = longitude,
           eventDate = datetime) %>%
    mutate(eventDate = as.Date(eventDate, format = "%Y-%m-%d"))

  combined_data <- blue_crab_clean

}, error = function(e) {
  message("Error combining dataframes: ", e)
})

write.csv(combined_data, "../www/data/bluecrab_observations.csv", row.names=FALSE)

# Convert to sf object, ensuring 'eventDate' is the time column
combined_sf <- st_as_sf(combined_data, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>%
  mutate(time = as.POSIXct(eventDate, format = "%Y-%m-%d")) %>%
  filter(!is.na(image_url))

# Load custom crab icon
custom_icon_bc <- makeIcon(
  iconUrl = "/srv/shiny-server/comanage/www/icons/blue-crab1.png",  # Use a path to your custom icon
  iconWidth = 27, iconHeight = 27
)

custom_icon_ac <- makeIcon(
  iconUrl = "/srv/shiny-server/comanage/www/icons/african-crab.png",  # Use a path to your custom icon
  iconWidth = 30, iconHeight = 30
)

# Create leaflet map with species toggle
map <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  setView(lng = 27.476597, lat = 35.372743, zoom = 6) %>%
  
  # Layer for African blue crab (Portunus segnis)
  addMarkers(data = combined_sf %>% filter(scientific_name == "Portunus segnis"),
             popup = ~paste0(
               "<b>Species Name: </b>", scientific_name, "<br>",
               "<b>Date Observed: </b>", eventDate, "<br>",
               "<a href='", url, "' target='_blank'>", url, "</a><br>",
               "<br><img src='", image_url, "' width='175'>"),
             icon = custom_icon_ac, group = "African blue crab (Portunus segnis)") %>%
  
  # Layer for Atlantic blue crab (Callinectes sapidus)
  addMarkers(data = combined_sf %>% filter(scientific_name == "Callinectes sapidus"),
             popup = ~paste0(
               "<b>Species Name: </b>", scientific_name, "<br>",
               "<b>Date Observed: </b>", eventDate, "<br>",
               "<a href='", url, "' target='_blank'>", url, "</a><br>",
               "<br><img src='", image_url, "' width='175'>"),
             icon = custom_icon_bc, group = "Atlantic blue crab (Callinectes sapidus)") %>%
  
  # Add layer control to toggle between species
  addLayersControl(
    overlayGroups = c("African blue crab (Portunus segnis)", "Atlantic blue crab (Callinectes sapidus)"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  # Hide African blue crab by default
  hideGroup("Atlantic blue crab (Callinectes sapidus)")
  

# Save the map as an HTML file
saveWidget(map, file = "../www/maps/blue_crab_map.html", selfcontained = TRUE)
