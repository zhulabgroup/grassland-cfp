# (re)estimate niche by thinning occurrence
# spThin package
# run time = 8 min

# source("scripts/_setup.R")

thin_occ <- function(full_tbl) {
  # define function to use spThin to sample and return data frame

  full_tbl <- tibble(species = "dummy", full_tbl) # create dummy species for spThin function

  thin_ls <- spThin::thin(
    loc.data = full_tbl,
    lat.col = "lat",
    long.col = "lon",
    spec.col = "species",
    thin.par = 10, # distance in km that records to be separated by
    reps = 1,
    locs.thinned.list.return = TRUE,
    write.files = FALSE,
    write.log.file = FALSE
  )

  thin_tbl <- thin_ls[[1]] %>%
    rownames() %>%
    as.numeric() %>%
    slice(full_tbl, .) %>%
    select(-species)
}

set.seed(31416)

# resample full to thin data
occ_clim_tbl <- read_rds(.path$cli_all_gbif) %>%
  # slice(1:1e4) %>% # DEBUG
  mutate(
    lon = unlist(map(.$geometry, 1)),
    lat = unlist(map(.$geometry, 2))
  ) %>%
  select(lon, lat, species, tmp = terraclim_tmp, ppt = terraclim_ppt) %>%
  distinct() %>% # remove duplicated records
  group_by(species) %>%
  nest() %>%
  rename(full_data = data) %>%
  mutate(thin_data = map(full_data, thin_occ))

# summary stats
sum_full_tbl <- occ_clim_tbl %>%
  unnest(cols = full_data) %>%
  summarize(
    full_mean_tmp = mean(tmp, na.rm = TRUE),
    full_median_tmp = median(tmp, na.rm = TRUE),
    full_mean_ppt = mean(ppt, na.rm = TRUE),
    full_median_ppt = median(ppt, na.rm = TRUE)
  )

sum_thin_tbl <- occ_clim_tbl %>%
  unnest(cols = thin_data) %>%
  summarize(
    thin_mean_tmp = mean(tmp, na.rm = TRUE),
    thin_median_tmp = median(tmp, na.rm = TRUE),
    thin_mean_ppt = mean(ppt, na.rm = TRUE),
    thin_median_ppt = median(ppt, na.rm = TRUE)
  )

sum_full_tbl %>%
  full_join(sum_thin_tbl) %>%
  write_rds(.path$sum_thin)
