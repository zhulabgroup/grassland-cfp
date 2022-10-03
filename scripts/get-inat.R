# direct query, not run
if (FALSE) {
  res <- occ(
    query = gbif_occ_df_today, from = "inat", has_coords = TRUE, limit = 10000,
    geometry = st_bbox(cfp_sf)
  )
}

# read GBIF file and filter
read_rds(.path$occ_gbif) %>%
  filter(datasetName == "iNaturalist research-grade observations") %>%
  write_rds(.path$occ_inat)
