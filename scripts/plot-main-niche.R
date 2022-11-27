# import data
niche_tbl <- read_rds(.path$sum_niche) %>%
  filter(occ_n > 100) # no dummy species

gbif_chelsa_sf <- read_rds(.path$geo_clim) %>%
  select(geometry, key, species, tmp = chelsa_tmp, ppt = chelsa_ppt, vpd = chelsa_vpd) %>%
  filter(species %in% niche_tbl$species) %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")

# choose example species
cool_species <- "Danthonia californica" # Michael: important native species in California
warm_species <- "Stipa pulchra" # Susan: Stipa pulchra is the state grass, and is the subject of a lot of ecological research and restoration effort

gbif_chelsa_sf <- gbif_chelsa_sf %>%
  mutate(species_type = case_when(
    species == cool_species ~ "cool",
    species == warm_species ~ "warm",
    TRUE ~ "other"
  ))

niche_tbl <- niche_tbl %>%
  mutate(species_type = case_when(
    species == cool_species ~ "cool",
    species == warm_species ~ "warm",
    TRUE ~ "other"
  ))

# create panels
occ_geog_gg <-
  ggplot() +
  geom_sf(
    data = rnaturalearth::ne_states(
      country = c("Mexico", "United States of America"),
      returnclass = "sf"
    ),
    fill = NA,
    color = gray(.25)
  ) +
  geom_sf(
    data = gbif_chelsa_sf,
    aes(color = species_type, shape = species_type, alpha = species_type)
  ) +
  scale_color_manual(values = c(cool = "blue", other = gray(.9), warm = "red")) +
  scale_shape_manual(values = c(cool = 19, other = 20, warm = 19)) +
  scale_alpha_manual(values = c(cool = .1, other = .002, warm = .1)) + # more transparent
  coord_sf(xlim = c(-126, -114), ylim = c(28, 44)) +
  scale_x_continuous(breaks = c(-125, -120, -115)) +
  scale_y_continuous(breaks = c(30, 35, 40)) +
  labs(x = "Longitude", y = "Latitude") +
  guides(color = "none", shape = "none", alpha = "none")

occ_clim_gg <-
  ggplot(
    data = gbif_chelsa_sf %>%
      filter(
        tmp > quantile(tmp, .0001), tmp < quantile(tmp, .9999), # remove extreme climate points for plotting
        ppt > quantile(ppt, .0001), ppt < quantile(ppt, .9999)
      ),
    mapping = aes(tmp, ppt)
  ) +
  geom_point(
    aes(color = species_type, shape = species_type, alpha = species_type)
  ) +
  scale_color_manual(values = c(cool = "blue", other = gray(.9), warm = "red")) +
  scale_shape_manual(values = c(cool = 19, other = 20, warm = 19)) +
  scale_alpha_manual(values = c(cool = .6, other = .005, warm = .6)) +
  labs(x = "Mean annual temperature (°C)", y = "Mean annual precipitation (mm)") +
  guides(color = "none", shape = "none", alpha = "none")

clim_niche_gg <-
  ggplot(
    data = niche_tbl,
    mapping = aes(
      x = tmp_occ_median,
      y = ppt_occ_median,
      xmin = tmp_occ_median - tmp_occ_sd / sqrt(occ_n),
      xmax = tmp_occ_median + tmp_occ_sd / sqrt(occ_n),
      ymin = ppt_occ_median - ppt_occ_sd / sqrt(occ_n),
      ymax = ppt_occ_median + ppt_occ_sd / sqrt(occ_n),
      label = species,
      color = species_type
    )
  ) +
  geom_point() +
  geom_errorbar() +
  geom_errorbarh() +
  scale_color_manual(values = c(cool = "blue", other = gray(.75), warm = "red")) +
  labs(x = "Mean annual temperature (°C)", y = "Mean annual precipitation (mm)") +
  guides(color = "none") +
  geom_label(
    data = niche_tbl %>% filter(species_type %in% c("cool", "warm")),
    fill = NA,
    fontface = "italic",
    label.padding = unit(.5, "lines"),
    label.size = 0,
    vjust = "outward", hjust = "outward"
  )

# combine panels
niche_gg <- occ_geog_gg + occ_clim_gg + clim_niche_gg +
  plot_annotation(tag_levels = "A") +
  plot_layout(design = "
  ABB
  ABB
  CCC
  CCC
  CCC
  ")

# save figure file
if (.fig_save) {
  ggsave(
    plot = niche_gg,
    filename = str_c(.path$out_fig, "fig-main-niche.pdf"),
    width = 7.5,
    height = 7.5 * 1.618
  )
}

# report stats
n_spp <- nrow(niche_tbl)
n_occ <- sum(niche_tbl$occ_n) %>%
  format(big.mark = ",")
n_occ_cool <- niche_tbl %>%
  filter(species_type == "cool") %>%
  pull(occ_n) %>%
  format(big.mark = ",")
n_occ_warm <- niche_tbl %>%
  filter(species_type == "warm") %>%
  pull(occ_n) %>%
  format(big.mark = ",")
