library(gfwr)
current_date <- Sys.Date()
api_key = ""
gfw_rt <- get_raster(spatial_resolution = "high",
                          temporal_resolution = "daily",
                          group_by = "flagAndGearType",
                          date_range = paste(current_date-30, current_date, sep=","),
                          region = 8366,
                          region_source = "eez",
                          key = api_key)

# Rename columns
colnames(gfw_rt) <- c(
  "latitude", "longitude", "time_range", "flag", "gear", "id",
  "apparent_fishing_hours"
)
write.csv(gfw_rt, "./www/gfw_data.csv", row.names=FALSE)
