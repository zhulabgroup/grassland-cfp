# get GBIF data
# ~ 15 min to run
library(foreach)
library(doSNOW)

# prep species list
source("scripts/compile-all-species.R")

# read CFP file
cfp_sf <- st_read(.path$geo_cfp) %>%
  filter(
    NAME == "California Floristic Province",
    Type == "hotspot area"
  )

num_cores <- 22 # socs-stats has 44 cores
cl <- makeCluster(num_cores)
registerDoSNOW(cl)
gbif_box_ls <-
  foreach(
    i = 1:length(spp_tbl$query_name),
    .packages = c("spocc", "sf", "tidyverse")
  ) %dopar% {
    sp <- spp_tbl$query_name[i]
    res <- occ(
      query = sp, from = "gbif", has_coords = TRUE, limit = 1e6,
      geometry = st_bbox(cfp_sf),
      gbifopts = list(
        # occ_options(from = "gbif", where = "console")
        hasGeospatialIssue = FALSE
      )
    )
    print(i)
    tibble(queryName = sp, gbif = res$gbif$data) %>%
      unnest(cols = c(gbif))
  }
stopCluster(cl)

gbif_box_tbl <- bind_rows(gbif_box_ls) # collapse from list to tibble

gbif_cfp_tbl <- gbif_box_tbl %>%
  select(key, longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_intersection(cfp_sf) %>% # find GBIF and CFP intersection
  as_tibble() %>% # drop geometry
  select(key) %>% # use key to join (fast)
  right_join(gbif_box_tbl, ., by = "key")

# filter coords and consolidate species
gbif_cfp_tbl %>%
  filter(coordinatePrecision < 0.01 | is.na(coordinatePrecision)) %>%
  filter(coordinateUncertaintyInMeters < 10000 | is.na(coordinateUncertaintyInMeters)) %>%
  filter(!coordinateUncertaintyInMeters %in% c(301, 3036, 999, 9999)) %>%
  left_join(spp_tbl, by = c("queryName" = "query_name")) %>%
  select(consolidatedName = consolidated_name, everything()) %>%
  mutate(
    consolidatedName =
      ifelse(
        is.na(consolidatedName),
        queryName,
        consolidatedName
      )
  ) %>%
  write_rds(.path$occ_gbif)
