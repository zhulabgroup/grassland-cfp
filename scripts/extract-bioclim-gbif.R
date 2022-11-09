# extract all bioclim variables at GBIF locations
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
