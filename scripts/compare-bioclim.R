# compare CHELSA bioclim variables (https://www.worldclim.org/data/bioclim.html)
# read GBIF data
gbif_sf <- read_rds(.path$occ_gbif) %>%
  select(longitude, latitude) %>%
  distinct() %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = "+proj=longlat +datum=WGS84 +no_defs"
  )

# load all climate rasters and extract values
# temperature
tmp_ras <- raster::stack(
  str_c(.path$cli_chelsa, "bio1.tif"), # Annual Mean Temperature
  # str_c(.path$cli_chelsa, "bio5.tif"), # Max Temperature of Warmest Month
  # str_c(.path$cli_chelsa, "bio6.tif"), # Min Temperature of Coldest Month
  str_c(.path$cli_chelsa, "bio8.tif"), # Mean Temperature of Wettest Quarter
  str_c(.path$cli_chelsa, "bio9.tif"), # Mean Temperature of Driest Quarter
  str_c(.path$cli_chelsa, "bio10.tif"), # Mean Temperature of Warmest Quarter
  str_c(.path$cli_chelsa, "bio11.tif") # Mean Temperature of Coldest Quarter
)

tmp_mat <- tmp_ras %>%
  raster::extract(gbif_sf)

tmp_cor <- cor(tmp_mat)

# precipitation
ppt_ras <- raster::stack(
  str_c(.path$cli_chelsa, "bio12.tif"), # Annual Precipitation
  str_c(.path$cli_chelsa, "bio13.tif"), # Precipitation of Wettest Month
  # str_c(.path$cli_chelsa, "bio14.tif"), # Precipitation of Driest Month
  str_c(.path$cli_chelsa, "bio16.tif"), # Precipitation of Wettest Quarter
  # str_c(.path$cli_chelsa, "bio17.tif"), # Precipitation of Driest Quarter
  # str_c(.path$cli_chelsa, "bio18.tif"), # Precipitation of Warmest Quarter
  str_c(.path$cli_chelsa, "bio19.tif") # Precipitation of Coldest Quarter
)

ppt_mat <- ppt_ras %>%
  raster::extract(gbif_sf)

ppt_cor <- cor(ppt_mat)
