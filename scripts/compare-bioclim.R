# compare CHELSA bioclim variables (https://www.worldclim.org/data/bioclim.html)
# read GBIF data
gbif_sf <- read_rds(.path$occ_gbif) %>%
  select(longitude, latitude) %>%
  distinct() %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = "+proj=longlat +datum=WGS84 +no_defs"
  )

# load all climate rasters
bioclim_ras <- .path$cli_chelsa %>%
  list.files(full.names = TRUE) %>%
  raster::stack()

# extract values at GBIF occurrences
bioclim_sf <- bioclim_ras %>%
  raster::extract(gbif_sf) %>%
  as_tibble() %>%
  bind_cols(gbif_sf, .)

write_rds(bioclim_sf, .path$cli_bioclim_gbif)

# temperature variables
bioclim_sf %>%
  st_drop_geometry() %>%
  select(str_c("bio", c(1, 5, 6, 8:11))) %>%
  sample_n(1e3) %>%
  PerformanceAnalytics::chart.Correlation(histogram = TRUE, pch = 19)

# precipitation variables
bioclim_sf %>%
  st_drop_geometry() %>%
  select(str_c("bio", c(12:14, 16:19))) %>%
  sample_n(1e3) %>%
  PerformanceAnalytics::chart.Correlation(histogram = TRUE, pch = 19)

# vpd variables
bioclim_sf %>%
  st_drop_geometry() %>%
  select(str_c("vpd_", c("max", "mean", "min"))) %>%
  sample_n(1e3) %>%
  PerformanceAnalytics::chart.Correlation(histogram = TRUE, pch = 19)
