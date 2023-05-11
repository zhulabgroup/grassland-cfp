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
    color = gray(0.5),
    shape = 20,
    alpha = 0.01
  ) +
  geom_sf(
    data = gbif_chelsa_sf %>%
      filter(species_type == "cool") %>%
      st_buffer(1e4) %>% # buffer to better show overlapping points
      st_union(),
    fill = "blue",
    color = NA,
    alpha = 0.4
  ) +
  geom_sf(
    data = gbif_chelsa_sf %>%
      filter(species_type == "warm") %>%
      st_buffer(1e4) %>% # buffer to better show overlapping points
      st_union(),
    fill = "red",
    color = NA,
    alpha = 0.4
  ) +
  coord_sf(xlim = c(-126, -114), ylim = c(28, 44)) +
  scale_x_continuous(breaks = c(-125, -120, -115)) +
  scale_y_continuous(breaks = c(30, 35, 40)) +
  labs(x = "Longitude", y = "Latitude") +
  guides(fill = "none", color = "none", shape = "none", alpha = "none")

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
  scale_color_manual(values = c(cool = "blue", other = gray(.5), warm = "red")) +
  scale_shape_manual(values = c(cool = 19, other = 20, warm = 19)) +
  scale_alpha_manual(values = c(cool = .6, other = .01, warm = .6)) +
  labs(x = .varname$tmp, y = .varname$ppt) +
  guides(color = "none", shape = "none", alpha = "none")

clim_niche_gg <-
  ggplot(
    mapping = aes(
      x = tmp_occ_median,
      y = ppt_occ_median,
      label = species,
      color = species_type
    )
  ) +
  geom_point(data = niche_tbl %>% filter(species_type == "other")) +
  geom_point(data = niche_tbl %>% filter(species_type %in% c("cool", "warm"))) +
  geom_label(
    data = niche_tbl %>% filter(species_type %in% c("cool", "warm")),
    fill = NA,
    fontface = "italic",
    label.padding = unit(.5, "lines"),
    label.size = 0,
    vjust = "outward", hjust = "outward"
  ) +
  scale_color_manual(values = c(cool = "blue", other = gray(.75), warm = "red")) +
  labs(x = .varname$tmp, y = .varname$ppt) +
  guides(color = "none")

# combine panels
niche_gg <- occ_geog_gg + occ_clim_gg + clim_niche_gg +
  plot_annotation(tag_levels = "A") +
  plot_layout(design = "
  AABB
  AABB
  CCCC
  CCCC
  CCCC
  ")

# save figure file
if (.fig_save) {
  ggsave(
    plot = niche_gg,
    filename = str_c(.path$out_fig, "fig-main-niche2.png"),
    width = 8,
    height = 8 * 1.618
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
niche_cor <- cor.test(niche_tbl$tmp_occ_median, niche_tbl$ppt_occ_median)

# for slides
if (.fig_save) {
  ggsave(
    plot = (occ_geog_gg + occ_clim_gg),
    filename = str_c(.path$out_fig, "fig-slide-occ.png"),
    width = 8,
    height = 8 * .618
  )
  ggsave(
    plot = clim_niche_gg,
    filename = str_c(.path$out_fig, "fig-slide-niche.png"),
    width = 5.5,
    height = 5.5
  )
}
