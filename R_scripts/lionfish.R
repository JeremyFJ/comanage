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
  lionfish1 <- get_inat_obs2(
    taxon_name = "Pterois miles",  # Scientific name of the blue crab
    geo = TRUE,                          # Ensure only georeferenced observations are returned
    bounds = med_bounds,       # Mediterranean Sea bounds
    maxresults = 10000                   # Adjust maxresults as needed
  )

  lionfish2 <- get_inat_obs2(
    taxon_name = "Pterois volitans",
    geo = TRUE,                          # Ensure only georeferenced observations are returned
    bounds = med_bounds,       # Mediterranean Sea bounds
    maxresults = 10000                   # Adjust maxresults as needed
  )
  lionfish <- rbind(lionfish1, lionfish2)

  # Step 2: Clean and standardize blue_crab_data
  lionfish_clean <- lionfish %>%
    filter(!is.na(latitude) & !is.na(longitude)) %>%
    select(latitude, longitude, datetime, scientific_name, url, image_url) %>%
    rename(decimalLatitude = latitude,
           decimalLongitude = longitude,
           eventDate = datetime) %>%
    mutate(eventDate = as.Date(eventDate, format = "%Y-%m-%d"))

  combined_data <- lionfish_clean

}, error = function(e) {
  message("Error combining dataframes: ", e)
})

write.csv(combined_data, "../www/data/lionfish_observations.csv", row.names=FALSE)
