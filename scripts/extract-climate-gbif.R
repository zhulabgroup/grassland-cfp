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

# extract climate data on GBIF locations
chelsa_gbif_sf <- chelsa_ras %>%
  raster::extract(gbif_sf_wgs84) %>%
  as_tibble() %>%
  rename_with(~ str_c("chelsa_", .)) %>%
  bind_cols(gbif_sf_wgs84)

prism_gbif_sf <- prism_ras %>%
  raster::extract(gbif_sf_nad83) %>%
  as_tibble() %>%
  mutate(vpd = vpd * 100) %>% # prism vpd * 100 = chelsa vpd unit
  rename_with(~ str_c("prism_", .)) %>%
  bind_cols(gbif_sf_nad83)

terraclim_gbif_sf <- terraclim_ras %>%
  raster::extract(gbif_sf_wgs84) %>%
  as_tibble() %>%
  mutate(vpd = vpd * 1000) %>% # terraclim vpd * 1000 = chelsa vpd unit
  rename_with(~ str_c("terraclim_", .)) %>%
  bind_cols(gbif_sf_wgs84)

# combine and write out
bind_cols(
  chelsa_gbif_sf %>%
    dplyr::select(geometry, key, species, starts_with("chelsa_")),
  prism_gbif_sf %>%
    dplyr::select(starts_with("prism_")),
  terraclim_gbif_sf %>%
    dplyr::select(starts_with("terraclim_"))
) %>%
  write_rds(.path$cli_all_gbif)
