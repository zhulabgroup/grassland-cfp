# get CCH data
# prep species list
source("scripts/compile-all-species.R")

# read CFP file
cfp_sf <- st_read(.path$geo_cfp) %>%
  filter(
    NAME == "California Floristic Province",
    Type == "hotspot area"
  )

# read manually downloaded file
cch_all_tbl <- "data/occurrence/cch/occurrences.csv" %>%
  read_csv(col_types = cols_only(
    id = "i",
    scientificName = "c",
    decimalLongitude = "d",
    decimalLatitude = "d"
  )) %>%
  filter(scientificName %in% spp_tbl$query_name) %>%
  drop_na(decimalLongitude, decimalLatitude) %>%
  select(
    queryName = scientificName,
    longitude = decimalLongitude,
    latitude = decimalLatitude,
    key = id
  ) %>%
  left_join(spp_tbl, by = c("queryName" = "query_name")) %>%
  mutate(species_name = ifelse(
    is.na(consolidated_name),
    queryName,
    consolidated_name
  )) %>%
  select(species_name, longitude, latitude, key)

# filter by CFP
cch_cfp_tbl <- cch_all_tbl %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_intersection(cfp_sf) %>% # find CCH and CFP intersection
  as_tibble() %>% # drop geometry
  select(key) %>% # use key to join (fast)
  right_join(cch_all_tbl, ., by = "key")

write_rds(cch_cfp_tbl, .path$occ_cch)
