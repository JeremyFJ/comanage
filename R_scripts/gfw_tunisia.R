library(leaflet)
library(lubridate)
library(dplyr)
library(htmlwidgets)
library(sf)
setwd("/srv/shiny-server/comanage/R_scripts")
# current_date <- Sys.Date()
# List of coastal Tunisian cities with their coordinates
tunisian_cities <- read.csv("../www/data/port_positions.csv")
gfw_rt = read.csv("../www/data/gfw_data.csv")

# Define bounding box for the Mediterranean Sea
med_bbox <- list(
  west = 7.26,
  east = 16.15,
  north = 40.45,
  south = 32
)

# Prepare data for mapping (ensure lat/lon columns exist)
gfw_rt <- gfw_rt %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  mutate(time_range = as.Date(time_range))

# Jitter the points to avoid overlap
set.seed(42) # Set seed for reproducibility
jitter_amount <- 0.005
gfw_rt <- gfw_rt %>%
  mutate(
    latitude_jitter = latitude + runif(n(), -jitter_amount, jitter_amount),
    longitude_jitter = longitude + runif(n(), -jitter_amount, jitter_amount)
  )

# Define the color mapping
gear_colors <- c(
  "purse_seines" = "blue",
  "trawlers" = "red",
  "drifting_longlines" = "green",
  "fishing" = "orange",
  "tuna_purse_seines" = "purple",
  "other_purse_seines" = "yellow",
  "inconclusive" = "#989090",
  "fixed_gear" = "white",
  "dredge_fishing" = "#0fa2a4",
  "set_longlines" = "#584809",
  "set_gillnets" = "#091158",
  "default" = "black"
)
gfw_rt$gear <- as.character(gfw_rt$gear)
# Map the colors to the gear types in the dataframe
gfw_rt <- gfw_rt %>%
  mutate(color = ifelse(gear %in% names(gear_colors), gear_colors[gear], gear_colors["default"]))

# Assuming gfw_rt$time_range is a character vector of date ranges
time_range_min <- min(as.Date(gfw_rt$time_range, format = "%Y-%m-%d"), na.rm = TRUE)
time_range_max <- max(as.Date(gfw_rt$time_range, format = "%Y-%m-%d"), na.rm = TRUE)

eez <- st_read("../www/eez/eez.shp")

custom_icon <- makeIcon(
  iconUrl = "https://sp2.cs.vt.edu/shiny/comanage/port.png",
  iconWidth = 20, iconHeight = 20
)

# Function to fetch weather data from WeatherAPI
get_weather_data <- function(lat, lon) {
  api_key <- "8a30418e7d9e4acb94505328242507"  # Replace with your WeatherAPI key
  url <- paste0("http://api.weatherapi.com/v1/current.json?key=", api_key, "&q=", lat, ",", lon)
  response <- httr::GET(url)
  weather_data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
  
  if (!is.null(weather_data$error)) {
    return(NULL)
  }
  
  weather_info <- paste(
    "Date: ", Sys.Date(),
    "<br>Temperature: ", weather_data$current$temp_c, "°C",
    "<br>Condition: ", weather_data$current$condition$text,
    "<br>Wind: ", weather_data$current$wind_kph, " kph ",
    weather_data$current$wind_dir
  )
  return(weather_info)
}

# Add weather data to port popups
tunisian_cities <- tunisian_cities %>%
  rowwise() %>%
  mutate(popup = paste0(port, "<br>",
                        get_weather_data(latitude, longitude)))

# Create a leaflet map to visualize the fishing activity in the Mediterranean
m <- leaflet() %>%
  addTiles() %>%
  addPolygons(
    data = eez,
    color = "blue",
    weight = 1,
    opacity = 0.35,
    fillOpacity = 0.2
  ) %>%
  addCircleMarkers(
    data = gfw_rt,
    lat = ~latitude_jitter,
    lng = ~longitude_jitter,
    radius = 5,
    color = ~color,
    stroke = FALSE,
    fillOpacity = 0.8,
    popup = ~paste("Flag:", flag, "<br>", "Date:", time_range, "<br>", "Gear:", gear, "<br>", "Fishing hours:", apparent_fishing_hours)
  ) %>%
  addControl(html = paste0(
    "<div style='background: white; padding: 5px;'>",
    "<strong>Gear Type</strong><br>",
    paste0("<div><span style='background:", gear_colors["purse_seines"], "; width: 12px; height: 12px; display: inline-block;'></span> Purse Seines</div>"),
    paste0("<div><span style='background:", gear_colors["trawlers"], "; width: 12px; height: 12px; display: inline-block;'></span> Trawlers</div>"),
    paste0("<div><span style='background:", gear_colors["drifting_longlines"], "; width: 12px; height: 12px; display: inline-block;'></span> Drifting Longlines</div>"),
    paste0("<div><span style='background:", gear_colors["set_longlines"], "; width: 12px; height: 12px; display: inline-block;'></span> Set Longlines</div>"),
    paste0("<div><span style='background:", gear_colors["fishing"], "; width: 12px; height: 12px; display: inline-block;'></span> Fishing</div>"),
    paste0("<div><span style='background:", gear_colors["tuna_purse_seines"], "; width: 12px; height: 12px; display: inline-block;'></span> Tuna Purse Seines</div>"),
    paste0("<div><span style='background:", gear_colors["other_purse_seines"], "; width: 12px; height: 12px; display: inline-block;'></span> Other Purse Seines</div>"),
    paste0("<div><span style='background:", gear_colors["set_gillnets"], "; width: 12px; height: 12px; display: inline-block;'></span> Set Gillnets</div>"),
    paste0("<div><span style='background:", gear_colors["fixed_gear"], "; width: 12px; height: 12px; display: inline-block;'></span> Fixed Gear</div>"),
    paste0("<div><span style='background:", gear_colors["dredge_fishing"], "; width: 12px; height: 12px; display: inline-block;'></span> Dredge Fishing</div>"),
    paste0("<div><span style='background:", gear_colors["inconclusive"], "; width: 12px; height: 12px; display: inline-block;'></span> Inconclusive</div>"),
    "</div>"
  ), position = "bottomright") %>%
  addControl(html = paste0(
    "<div style='background: white; padding: 5px; font-size: 16px; font-weight: bold;'>",
    "Dates: ", time_range_min, " to ", time_range_max,
    "</div>"
  ), position = "topright") %>%
  addMarkers(
    data = tunisian_cities,
    lat = ~latitude,
    lng = ~longitude,
    popup = ~popup,
    icon = custom_icon
  ) %>%
  setView(lng = 10, lat = 36, zoom = 7)  # Set the initial zoom level and center

# Save the map
saveWidget(m, "../www/maps/fishing_activity_map.html", selfcontained = FALSE)
