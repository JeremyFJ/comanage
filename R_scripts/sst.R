reticulate::use_python("/home/spr/anaconda3/envs/detect/bin/python")
library(lubridate)
library(leaflet)
library(leaflet.extras)
library(ncdf4)
library(raster)
library(sf)
library(htmlwidgets)
setwd("/srv/shiny-server/comanage/R_scripts")
source("process_oceanographic_raster.R")
# Load the reticulate package

start_datetime <- Sys.Date() - 7
end_datetime <- Sys.Date()

# Function to automate data retrieval
automate_copernicus_retrieval <- function(start_datetime, end_datetime) {

  # Convert dates to the required format
  end_datetime_str <- format(end_datetime, "%Y-%m-%dT00:00:00")
  start_datetime_str <- format(start_datetime, "%Y-%m-%dT00:00:00")

  # Coordinates for the Tunisian marine exclusive economic zone
  minimum_longitude <- 6.524
  maximum_longitude <- 14.454
  minimum_latitude <- 30.236
  maximum_latitude <- 38.8

  # Output filename based on the end date
  output_filename <- paste0("../www/data/", "sst_", format(end_datetime, "%Y-%m-%d"), ".nc")

  # Use reticulate to run the Python script with the calculated parameters
  reticulate::py_run_string(paste0("
import copernicusmarine as cm
import subprocess

# Define the Python script as a string
python_script = '''
import copernicusmarine as cm

cm.subset(
    dataset_id=\"METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2\",
    variables=[\"analysed_sst\"],
    minimum_longitude=", minimum_longitude, ",
    maximum_longitude=", maximum_longitude, ",
    minimum_latitude=", minimum_latitude, ",
    maximum_latitude=", maximum_latitude, ",
    start_datetime=\"", start_datetime_str, "\",
    end_datetime=\"", end_datetime_str, "\",
    output_filename=\"", output_filename, "\",
    username=\"jjenrette\",
    password=\"VTsandtiger1!\"
)
'''

# Run the Python script and simulate 'Y' input for confirmation
process = subprocess.Popen(['python', '-c', python_script], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
stdout_data, stderr_data = process.communicate(input=b'Y\\n')

print(stdout_data.decode())
print(stderr_data.decode())
"))

return(output_filename)
}

# Run the function to retrieve the data
nc_file = automate_copernicus_retrieval(start_datetime, end_datetime)
# Open the .nc file and extract the 'analysed_sst' variable
nc_data <- nc_open(nc_file)
sst_raster <- raster(nc_file, varname = "analysed_sst")

# Close the .nc file connection
nc_close(nc_data)
sst_raster_celsius <- sst_raster - 273.15
sst_raster_celsius <- terra::rast(sst_raster_celsius)
# Load the Tunisian EEZ shapefile as an sf object
tunisian_eez <- st_read("../www/eez/eez.shp")

sst_final = process_oceanographic_raster(sst_raster_celsius, tunisian_eez)

# Define a color palette for Celsius temperatures
pal <- colorNumeric(palette = "viridis", domain = values(sst_final), na.color = "transparent")

# Create the leaflet map and add the final masked raster
m <- leaflet() %>%
  addTiles() %>%
  addRasterImage(sst_final, colors = pal, opacity = 0.8) %>%
  #   addPolygons(
  #   data = tunisian_eez,
  #   color = "blue",
  #   weight = 1,
  #   opacity = 0.35,
  #   fillOpacity = 0
  # ) %>%
  addControl(html = paste0(
    "<div style='background: white; padding: 5px; font-size: 16px; font-weight: bold;'>",
    "Dates: ", start_datetime, " to ", end_datetime,
    "</div>"
  ), position = "topright") %>%
  addLegend(pal = pal, values = values(sst_final), title = "Temp (°C)") %>%
  setView(lng = 10, lat = 36, zoom = 7)  # Centered around Tunisia

# Print the map to the R console (optional)
# m

# Print the map to the R console (optional)
# Save the leaflet map as an HTML file
saveWidget(m, file = "../www/maps/tunisian_sst_map_celsius.html")
