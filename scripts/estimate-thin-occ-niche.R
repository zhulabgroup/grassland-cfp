# (re)estimate niche by thinning occurrence
# spThin package
# run time = 22 min

# source("scripts/utils/setup.R")

occ_clim_sf <- read_rds(.path$cli_all_gbif) %>%
  sf::st_as_sf()

occ_clim_tbl <- sf::st_coordinates(occ_clim_sf) %>%
  as_tibble() %>%
  bind_cols(occ_clim_sf) %>%
  select(lat = Y, lon = X, key, species, tmp = terraclim_tmp, ppt = terraclim_ppt)

# species climate summary for full dataset
sp_sum_tbl <- occ_clim_tbl %>%
  group_by(species) %>%
  summarize(
    tmp_mean_full = mean(tmp), ppt_mean_full = mean(ppt),
    tmp_median_full = median(tmp), ppt_median_full = median(ppt)
  ) %>%
  arrange(species)

sp_thin_tbl <- tibble()

set.seed(31416)

for (i in seq_along(sp_sum_tbl$species)) {
  # subset species
  sp <- sp_sum_tbl$species[i]
  print(sp)
  loc_full_tbl <- occ_clim_tbl %>%
    filter(species == sp)

  # thin data
  loc_thin_ls <- spThin::thin(
    loc.data = loc_full_tbl,
    lat.col = "lat",
    long.col = "lon",
    spec.col = "species",
    thin.par = 10, # distance in km that records to be separated by
    reps = 1,
    locs.thinned.list.return = TRUE,
    write.files = FALSE,
    write.log.file = FALSE
  )

  # summarize climate
  sp_thin_tbl <- loc_full_tbl[row.names(loc_thin_ls[[1]]), ] %>%
    summarize(
      tmp_mean_thin = mean(tmp), ppt_mean_thin = mean(ppt),
      tmp_median_thin = median(tmp), ppt_median_thin = median(ppt)
    ) %>%
    bind_cols(species = sp, .) %>%
    bind_rows(sp_thin_tbl, .)
}

sp_sum_tbl %>%
  full_join(sp_thin_tbl, by = "species") %>%
  write_rds(.path$sum_thin)
