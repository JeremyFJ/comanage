process_oceanographic_raster <- function(raster_data, eez, buffer_distance_miles = 1) {
  library(terra)
  library(sf)
  
  # Ensure the CRS of the EEZ matches the raster
  eez <- st_transform(eez, crs(raster_data))
  
  # Load the land mask shapefile (replace with your own file path)
  land <- st_read("../www/data/tunisia_Tunisia_Country_Boundary/tunisia_Tunisia_Country_Boundary.shp")
  
  # Ensure the CRS matches the raster
  land <- st_transform(land, crs(raster_data))
  
  # Convert land mask to a format compatible with terra
  land_vect <- vect(land)
  
  # Mask the raster to keep only water areas (raster - land)
  raster_masked <- mask(raster_data, land_vect, inverse = TRUE)
  
  # Convert EEZ to terra format for cropping
  eez_vect <- vect(eez)
  
  # Crop the masked raster to the extent of the EEZ
  raster_cropped <- crop(raster_masked, eez_vect)
  
  # Convert buffer distance from miles to meters (1 mile = 1609.34 meters)
  buffer_distance_meters <- buffer_distance_miles * 1609.34
  
  # Buffer the EEZ by the specified distance
  eez_buffered <- st_buffer(eez, dist = buffer_distance_meters)
  
  # Convert the buffered EEZ to terra format for masking
  eez_buffered_vect <- vect(eez_buffered)
  
  # Mask the cropped raster using the buffered EEZ
  raster_final <- mask(raster_cropped, eez_buffered_vect)
  
  return(raster_final)
}
