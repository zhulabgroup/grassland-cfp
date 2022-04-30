# tested on 20220424

##### Getting today's occ data
sp <- "Achillea millefolium"
res <- occ(
  query = sp, from = "gbif", has_coords = TRUE, limit = 1e6,
  geometry = st_bbox(cfp_sf),
  gbifopts = list(
    hasGeospatialIssue = FALSE
  )
)
sp_df <- tibble(queryName = sp, gbif = res$gbif$data) %>%
  unnest(cols = c(gbif))

gbif_occ_box_df <- sp_df
occ_sf_box <- st_as_sf(
  x = gbif_occ_box_df %>% dplyr::select(key, longitude, latitude),
  coords = c("longitude", "latitude"),
  crs = 4326
)
occ_sf <- st_intersection(occ_sf_box, cfp_sf)
gbif_occ_df <- gbif_occ_box_df %>%
  right_join(as_tibble(occ_sf) %>% dplyr::select(key), by = "key")

gbif_occ_df_today <- gbif_occ_df
write_rds(gbif_occ_df_today, "data/occurrence/gbif/all-spp-cfp-2022-04-27-test.rds")
##### Comparing with previous data

# Comparing with 0323 data
gbif_occ_df_0323 <- read_rds("data/occurrence/gbif/all-spp-cfp-2022-03-23.rds")
# some record keys seem to be deleted, some added
# in 0323 but not in today
diff1 <- anti_join(gbif_occ_df_0323 %>% filter(queryName == sp), gbif_occ_df_today, by = c("queryName", "key"))
diff1 %>% nrow()
diff1 %>%
  pull(lastCrawled) %>%
  as.Date() %>%
  unique() %>%
  sort()
# in today but not in 0323
diff2 <- anti_join(gbif_occ_df_today, gbif_occ_df_0323 %>% filter(queryName == sp), by = c("queryName", "key"))
diff2 %>% nrow()
diff2 %>%
  pull(lastCrawled) %>%
  as.Date() %>%
  unique() %>%
  sort()

# Comparing with 0418 data
gbif_occ_df_0418 <- read_rds("data/occurrence/gbif/all-spp-cfp-2022-04-18.rds")
# no difference in record keys but some records have been re-crawled judging from the crawId
# in 0418 but not in today
diff1 <- anti_join(gbif_occ_df_0418 %>% filter(queryName == sp), gbif_occ_df_today, by = c("queryName", "key", "crawlId"))
diff1 %>% nrow()
diff1 %>%
  pull(lastCrawled) %>%
  as.Date() %>%
  unique() %>%
  sort()
# in today but not in 0418
diff2 <- anti_join(gbif_occ_df_today, gbif_occ_df_0418 %>% filter(queryName == sp), by = c("queryName", "key", "crawlId"))
diff2 %>% nrow()
diff2 %>%
  pull(lastCrawled) %>%
  as.Date() %>%
  unique() %>%
  sort()

# 0425 data do not have this species for some reason?
