# Should be run by sudo

library(gfwr)
setwd("/srv/shiny-server/comanage/R_scripts")
current_date <- Sys.Date()
api_key = "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6ImtpZEtleSJ9.eyJkYXRhIjp7Im5hbWUiOiJuZXcxIiwidXNlcklkIjozMzg2OSwiYXBwbGljYXRpb25OYW1lIjoibmV3MSIsImlkIjoxNDM5LCJ0eXBlIjoidXNlci1hcHBsaWNhdGlvbiJ9LCJpYXQiOjE3MTM0NjY5ODQsImV4cCI6MjAyODgyNjk4NCwiYXVkIjoiZ2Z3IiwiaXNzIjoiZ2Z3In0.UJK4UT68UGd0KrZ-Qcc8-yq-SWAz6Uvs_gFrHzTj9jQg0MaSdFxecZ1Q-MAvbx2JiTW-Y3TLNTte632X0ib24a0_sM1wc1SFuPOUy3_Y0umSdN1Vo-pmdTHl9Kx2UyA9JujGUbuV2xE3BQFvSl8-d8BeQlZ-rz8nk6t-oaTsIuh7E9u9GVGB57MVVSodsMq7Nmt0qjaeWaxrXBX2KWSpx3_Ngr17uSttM8OobmtHjQsLO5RO3Gz15BK7HgjQqJ7-tVkGvvv-el2hSdFolPUt3TZrv5qJK3n539hAsUu8mjBjSoqh2c28ekwgC4ouf6iI1KGOs2vdsDUZh47WUfSdb_Yx9iQS_-7BtRmmbEFD1Tz_Mddg1re508TacHGtPQw8q7zdQAZoSC8tWcP4PmY_uhlKiHKIUr7KNP9qRLH0l_82I-5cleggy6c0BUR-M9JxwBD40rsFi_GOO_7TGARz--PIIfhLsXe3Q0rHlVZTN2wdUeV5YQN3xmM5RfMSFZCs"

gfw_rt <- get_raster(spatial_resolution = "HIGH",
                     temporal_resolution = "DAILY",
                     group_by = "FLAGANDGEARTYPE",
                     start_date = paste(current_date-20),
                     end_date = current_date,
                     region = 8366,
                     region_source = "eez",
                     key = api_key)

# Rename columns
colnames(gfw_rt) <- c(
  "latitude", "longitude", "time_range", "flag", "gear", "id",
  "apparent_fishing_hours"
)
write.csv(gfw_rt, "../www/data/gfw_data.csv", row.names=FALSE)