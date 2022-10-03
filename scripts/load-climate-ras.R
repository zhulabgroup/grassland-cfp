# load all climate raster files

# chelsa
chelsa_ras <- raster::stack(
  str_c(.path$cli_chelsa, "bio1.tif"),
  str_c(.path$cli_chelsa, "bio12.tif"),
  str_c(.path$cli_chelsa, "vpd_max.tif")
)
names(chelsa_ras) <- c("tmp", "ppt", "vpd")
raster::proj4string(chelsa_ras) # WGS84
