source("_setup.R")
library(sf)
theme_set(ggthemes::theme_few())

# estimate tree temp niche ------------------------------------------------
# read tree occurrences
tree_occ <- .path$laughlin_mcgill %>%
  read_csv(col_types = "icdddddf")

# BIO1 = Annual Mean Temperature
# BIO5 = Max Temperature of Warmest Month
# BIO6 = Min Temperature of Coldest Month

# plot temp distributions
tree_occ %>%
  # filter(species %in% c("Picea mariana", "Pinus elliottii")) %>%
  ggplot(aes(bio1, color = source)) +
  geom_density() +
  facet_wrap(~species)

# summarize temp distributions
niche_sum <- tree_occ %>%
  group_by(species, source) %>%
  summarize(
    mean = mean(bio1), sd = sd(bio1),
    mid = median(bio1), lwr = quantile(bio1, .01), upr = quantile(bio1, .99),
    .groups = "drop"
  )

niche_sum %>%
  ggplot(aes(mean, mid, color = source)) +
  geom_point()

niche_sum %>%
  # filter(species %in% c("Picea mariana", "Pinus elliottii")) %>%
  ggplot(aes(x = mean, y = source)) +
  geom_pointrange(aes(xmin = lwr, xmax = upr)) +
  facet_wrap(~species)

niche_sum %>%
  ggplot(aes(x = mean)) +
  geom_histogram()


# calculate FIA CTI -------------------------------------------------------
# read FIA data
fia_tree <- .path$fia_rds %>%
  str_c("TREE_FOREST.rds") %>%
  read_rds()

fia_tree <- fia_tree %>%
  filter(
    LON > -125, LON < -65,
    LAT > 25, LAT < 50
  ) %>%
  mutate(species = str_c(GENUS, SPECIES, sep = " ")) %>%
  select(
    plt = PLT_CN, lat = LAT, lon = LON,
    species, dbh = DIA
  )

fia_cti <- niche_sum %>%
  filter(source == "bien") %>%
  inner_join(fia_tree, ., by = "species") %>%
  group_by(plt, lat, lon) %>%
  summarize(cti = sum(mean * dbh^2, na.rm = TRUE) / sum(dbh^2, na.rm = TRUE))

sf_usa <- maps::map("state", plot = F, fill = T) %>%
  st_as_sf()

ggplot() +
  geom_sf(data = sf_usa, fill = NA, col = "gray") +
  geom_point(data = fia_cti, aes(lon, lat, col = cti), alpha = .5, size = .1) +
  scale_color_viridis_c() +
  labs(
    x = "Longitude", y = "Latitude",
    color = "CTI (°C)",
    title = "Community Temperature Index (CTI, °C)"
  )
