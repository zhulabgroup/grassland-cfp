# get CCH data
# downloaded the data manually from https://www.cch2.org/portal/collections/harvestparams.php
# downloading limit is 1000000 records, so downloaded in three latitudinal bands
# 125-115W
# 28-34N 447335 records
# 34-38N 914648 records
# 38-44N 705205 records
# retrieved on 11-08-2022 2pm

# prep species list
source("scripts/compile-all-species.R")

# read CFP file
cfp_sf <- st_read(.path$geo_cfp) %>%
  filter(
    NAME == "California Floristic Province",
    Type == "hotspot area"
  )

# read manually downloaded file
cch_south<-"data/occurrence/cch/south.csv" %>%
  read_csv(col_types = cols_only(
    id = "i",
    scientificName = "c",
    decimalLongitude = "d",
    decimalLatitude = "d"
  ))
nrow(cch_south)

cch_mid<-"data/occurrence/cch/mid.csv" %>%
  read_csv(col_types = cols_only(
    id = "i",
    scientificName = "c",
    decimalLongitude = "d",
    decimalLatitude = "d"
  ))
nrow(cch_mid)

cch_north<-"data/occurrence/cch/north.csv" %>%
  read_csv(col_types = cols_only(
    id = "i",
    scientificName = "c",
    decimalLongitude = "d",
    decimalLatitude = "d"
  ))
nrow(cch_north)

cch_all_tbl <-bind_rows(cch_south,
                        cch_mid,
                        cch_north) %>%
  filter(scientificName %in% spp_tbl$query_name) %>%
  drop_na(decimalLongitude, decimalLatitude) %>%
  select(
    queryName = scientificName,
    longitude = decimalLongitude,
    latitude = decimalLatitude,
    key = id
  ) %>%
  distinct(key, .keep_all = T) %>% 
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

ggplot(cch_cfp_tbl)+
  geom_point(aes(x=longitude, y=latitude), alpha=0.01)
