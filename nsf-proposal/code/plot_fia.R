source("_setup.R")
library(sf)
library(patchwork)
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
    species, dbh = DIA, year = MEASYEAR
  )

# aggregate coordinates to 1 degree diameter hexagons
source("nsf-proposal/code/aggregate_hexagon.R")
fia_tree <- aggregate_hexagon(fia_tree, cellsize = 1)

# plot number of records by year
fia_tree %>%
  ggplot(aes(year)) +
  geom_histogram(binwidth = 1) +
  labs(
    x = "Year", y = "Number of records",
    title = "FIA tree records by year"
  )

fia_tbl <- niche_sum %>%
  filter(source == "bien") %>%
  inner_join(fia_tree, ., by = "species") %>%
  group_by(hex_id, hex_lat, hex_lon) %>%
  summarize(cti = sum(mean * dbh^2, na.rm = TRUE) / sum(dbh^2, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(mat = terra::rast(.path$clim) %>%
    terra::extract(cbind(hex_lon, hex_lat)) %>%
    unlist()) %>%
  mutate(dis = mat - cti) %>%
  gather(key = "var", value = "value", -hex_id, -hex_lat, -hex_lon) %>%
  mutate(var = factor(var, levels = c("cti", "mat", "dis")))

fia_sf <- fia_tbl %>%
  st_as_sf(coords = c("hex_lon", "hex_lat"), crs = 4326)

ls_fia_p <- vector(mode = "list")
for (varoi in c("mat", "cti", "dis")) {
  p <- ggplot() +
    geom_sf(data = maps::map("state", plot = F, fill = T) %>%
      st_as_sf(), fill = NA, col = "gray") +
    stat_summary_hex(data = fia_tbl %>% filter(var == varoi), aes(hex_lon, hex_lat, z = value), col = NA, fun = mean, alpha = 0.8, binwidth = c(1, 1)) +
    labs(
      x = "Longitude", y = "Latitude",
      fill = NULL
    ) +
    facet_wrap(. ~ var,
      nrow = 1, ncol = 1,
      labeller = labeller(var = c(
        cti =  "Community temperature index (CTI, °C)",
        mat = "Mean annual temperature (MAT, °C)",
        dis = "Disequilibrium (MAT - CTI, °C)"
      ))
    ) +
    theme(legend.position = "bottom")

  if (varoi != "dis") {
    p <- p + scale_fill_continuous(type = "viridis", limits = c(-5, 25))
  } else {
    p <- p + scale_fill_gradient2(
      low = "#01665e",
      mid = "white",
      high = "#8c510a",
      midpoint = 0,
      limits = c(-10, 15)
    )
  }
  ls_fia_p[[varoi]] <- p
}


fia_p <- ls_fia_p[[1]] + ls_fia_p[[2]] + ls_fia_p[[3]] +
  plot_layout(design = "
              AB
              C#")

ggsave(
  plot = fia_p,
  filename = "nsf-proposal/figures/fia.pdf",
  width = 8,
  height = 7,
  device = pdf
)
