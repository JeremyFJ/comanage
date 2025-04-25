library(leaflet)
library(lubridate)
library(dplyr)
library(htmlwidgets)
library(sf)
setwd("/srv/shiny-server/comanage/R_scripts")
# Read the port positions from the provided CSV file
tunisian_cities <- read.csv("../www/data/port_positions.csv")
gfw_rt <- read.csv("../www/data/gfw_data.csv")  # Adjust path as needed
# Set the bounding box for the Mediterranean Sea
med_bbox <- list(
  west = 7.26,
  east = 16.15,
  north = 40.45,
  south = 32
)
# Filter and process the fishing activity data
gfw_rt <- gfw_rt %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  mutate(time_range = as.Date(time_range))

# Define the grid size
grid_size <- 0.25

# Create the grid
grid <- st_make_grid(
  st_bbox(c(
    xmin = med_bbox$west, ymin = med_bbox$south,
    xmax = med_bbox$east, ymax = med_bbox$north
  )),
  cellsize = c(grid_size, grid_size),
  what = "polygons"
)

# Convert the grid to an sf object and set the CRS to WGS 84
grid_sf <- st_sf(geometry = grid)
st_crs(grid_sf) <- 4326

# Convert the fishing activity data to an sf object and set the CRS to WGS 84
gfw_sf <- st_as_sf(gfw_rt, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")

# Assign fishing activity to grid cells
gfw_grid <- st_join(st_sf(geometry = grid_sf), gfw_sf, join = st_intersects)

# Summarize fishing effort by grid cell
grid_summary <- gfw_grid %>%
  group_by(geometry) %>%
  summarise(
    total_effort = sum(apparent_fishing_hours, na.rm = TRUE),
    top_flag = ifelse(length(table(flag)) > 0, names(which.max(table(flag))), "None"),
    top_gear = ifelse(length(table(gear)) > 0, names(which.max(table(gear))), "None")
  ) %>%
  filter(total_effort > 0)  # Keep only cells with fishing effort

# Apply log transformation to total_effort
grid_summary <- grid_summary %>%
  mutate(log_total_effort = log1p(total_effort))  # log1p is log(1 + x) to handle log(0)
# print(grid_summary$log_total_effort)

# Create a color palette for the grid
effort_pal <- colorNumeric(palette = "YlOrRd", domain = grid_summary$log_total_effort)

# Assuming gfw_rt$time_range is a character vector of date ranges
time_range_min <- min(as.Date(gfw_rt$time_range, format = "%Y-%m-%d"), na.rm = TRUE)
time_range_max <- max(as.Date(gfw_rt$time_range, format = "%Y-%m-%d"), na.rm = TRUE)

# eez <- st_read("./www/eez/eez.shp")

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
  mutate(popup = paste0(port, "<br>"))  # , get_weather_data(latitude, longitude)))

# Create the leaflet map
m <- leaflet() %>%
  addTiles() %>%
  addPolygons(
    data = grid_summary,
    fillColor = ~effort_pal(log_total_effort),
    fillOpacity = 0.75,
    color = "white",
    weight = 1,
    popup = ~paste(
      "Total Effort (hours):", total_effort, "<br>",
      "Top Flag:", top_flag, "<br>",
      "Top Gear:", top_gear
    )
  ) %>%
  addControl(html = paste0(
    "<div style='background: white; padding: 5px; font-size: 16px; font-weight: bold;'>",
    "Dates: ", time_range_min, " to ", time_range_max,
    "</div>"
  ), position = "topright") %>%
  addLegend(
    position = "bottomright",
    pal = effort_pal,
    values = grid_summary$log_total_effort,
    title = "Total Effort (hours)",
    opacity = 0.7,
    labFormat = labelFormat(transform = function(x) round(expm1(x)))  # Convert log scale back to original scale for labels
  ) %>%
    addMarkers(
    data = tunisian_cities,
    lat = ~latitude,
    lng = ~longitude,
    popup = ~popup,
    icon = custom_icon,
    clusterOptions = markerClusterOptions()
  ) %>%
  setView(lng = 10, lat = 36, zoom = 7)  # Set the initial zoom level and center

# Save the map
saveWidget(m, "../www/maps/fishing_effort_grid.html", selfcontained = FALSE)
saveWidget(m, "../www/maps/fishing_effort_grid_sc.html", selfcontained = TRUE)