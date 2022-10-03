# read data
bien_tbl <- read_rds(.path$occ_bien) %>%
  mutate(
    dataset = "bien",
    species = queryName,
    key = as.character(key)
  ) %>%
  select(dataset, key, species, longitude, latitude)

cch_tbl <- read_rds(.path$occ_cch) %>%
  mutate(
    dataset = "cch",
    species = species_name,
    key = as.character(key)
  ) %>%
  select(dataset, key, species, longitude, latitude)

gbif_tbl <- read_rds(.path$occ_gbif) %>%
  mutate(
    dataset = "gbif",
    species = consolidatedName,
    key = as.character(key)
  ) %>%
  filter(coordinatePrecision < 0.01 | is.na(coordinatePrecision)) %>%
  filter(coordinateUncertaintyInMeters < 10000 | is.na(coordinateUncertaintyInMeters)) %>%
  filter(!coordinateUncertaintyInMeters %in% c(301, 3036, 999, 9999)) %>%
  select(dataset, key, species, longitude, latitude)

inat_tbl <- read_rds(inat_file) %>%
  mutate(
    dataset = "inat",
    species = consolidatedName,
    key = as.character(key)
  ) %>%
  filter(coordinatePrecision < 0.01 | is.na(coordinatePrecision)) %>%
  filter(coordinateUncertaintyInMeters < 10000 | is.na(coordinateUncertaintyInMeters)) %>%
  filter(!coordinateUncertaintyInMeters %in% c(301, 3036, 999, 9999)) %>%
  select(dataset, key, species, longitude, latitude)

# combine them and convert to sf
occ_sf <- bien_tbl %>%
  bind_rows(cch_tbl) %>%
  bind_rows(gbif_tbl) %>%
  bind_rows(inat_tbl) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) # WGS84

# compare sample sizes across data sets
occ_sf %>%
  as_tibble() %>%
  group_by(dataset) %>%
  summarise(
    record_number = n() %>% format(trim = TRUE, scientific = FALSE, big.mark = ","),
    species_number = unique(species) %>% length() %>% format(trim = TRUE, scientific = FALSE, big.mark = ",")
  ) %>%
  knitr::kable()

# ompare occurrence locations across data sets
# randomly sample a proportion of records to save render time
set.seed(618)
prop_samp <- .1 # prop to sample and plot
ggplot() +
  geom_sf(
    data = rnaturalearth::ne_states(
      country = c("Mexico", "United States of America"),
      returnclass = "sf"
    ),
    fill = NA,
    color = alpha("black", .1)
  ) +
  geom_sf(data = cfp_sf, fill = "white", alpha = .5) +
  geom_sf(
    data = occ_sf %>% slice_sample(prop = prop_samp), # randomly sample certain proportion, otherwise too slow to render
    color = "black", alpha = .1, size = .1
  ) +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(-125, -115), ylim = c(28, 44)) +
  facet_wrap(. ~ dataset, nrow = 1)
