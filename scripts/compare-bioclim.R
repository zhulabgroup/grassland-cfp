# read GBIF data in both CRS
gbif_sf_wgs84 <- read_rds(.path$occ_gbif) %>%
  mutate(
    species = consolidatedName,
    key = as.character(key)
  ) %>%
  dplyr::select(key, species, longitude, latitude) %>%
  distinct() %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = "+proj=longlat +datum=WGS84 +no_defs"
  )

gbif_sf_nad83 <- gbif_sf_wgs84 %>%
  st_transform(crs = "+proj=longlat +datum=NAD83 +no_defs")


# load all climate raster files

# temp
temp_ras <- raster::stack(
  str_c(.path$cli_chelsa, "bio1.tif"),
  str_c(.path$cli_chelsa, "bio5.tif"),
  str_c(.path$cli_chelsa, "bio6.tif"),
  str_c(.path$cli_chelsa, "bio8.tif"),
  str_c(.path$cli_chelsa, "bio9.tif"),
  str_c(.path$cli_chelsa, "bio10.tif"),
  str_c(.path$cli_chelsa, "bio11.tif")
)
names(temp_ras) <- c("bio1", "bio5", "bio6", "bio8", "bio9", "bio10", "bio11")

# extract climate data on GBIF locations
temp_gbif_sf <- temp_ras %>%
  raster::extract(gbif_sf_wgs84) %>%
  as_tibble() %>%
  bind_cols(gbif_sf_wgs84)

library("PerformanceAnalytics")
chart.Correlation(temp_gbif_sf[, 1:7] %>% sample_n(1000), histogram = TRUE, pch = 19)


# prcp
prcp_ras <- raster::stack(
  str_c(.path$cli_chelsa, "bio12.tif"),
  str_c(.path$cli_chelsa, "bio13.tif"),
  str_c(.path$cli_chelsa, "bio14.tif"),
  str_c(.path$cli_chelsa, "bio16.tif"),
  str_c(.path$cli_chelsa, "bio17.tif"),
  str_c(.path$cli_chelsa, "bio18.tif"),
  str_c(.path$cli_chelsa, "bio19.tif")
)
names(prcp_ras) <- c("bio12", "bio13", "bio14", "bio16", "bio17", "bio18", "bio19")

# extract climate data on GBIF locations
prcp_gbif_sf <- prcp_ras %>%
  raster::extract(gbif_sf_wgs84) %>%
  as_tibble() %>%
  bind_cols(gbif_sf_wgs84)

chart.Correlation(prcp_gbif_sf[, 1:7] %>% sample_n(1000), histogram = TRUE, pch = 19)

# vpd
vpd_ras <- raster::stack(
  str_c(.path$cli_chelsa, "vpd_max.tif"),
  str_c(.path$cli_chelsa, "vpd_mean.tif"),
  str_c(.path$cli_chelsa, "vpd_min.tif")
)
names(vpd_ras) <- c("vpd_max", "vpd_mean", "vpd_min")

# extract climate data on GBIF locations
vpd_gbif_sf <- vpd_ras %>%
  raster::extract(gbif_sf_wgs84) %>%
  as_tibble() %>%
  bind_cols(gbif_sf_wgs84)

chart.Correlation(vpd_gbif_sf[, 1:3] %>% sample_n(1000), histogram = TRUE, pch = 19)

# vpd_max does not correlate well with the vpd_min but bio1 and bio12 seem okay
