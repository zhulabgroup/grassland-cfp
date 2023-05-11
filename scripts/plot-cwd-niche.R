# import data
niche_tbl <- read_rds(.path$sum_niche_cwd) %>%
  filter(occ_n > 100) # no dummy species

gbif_terraclim_sf <- read_rds(.path$geo_clim_cwd) %>%
  select(geometry, key, species, tmp = terraclim_tmp, ppt = terraclim_ppt, vpd = terraclim_vpd, cwd = terraclim_cwd) %>%
  filter(species %in% niche_tbl$species) %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")

# choose example species
cool_species <- "Danthonia californica" # Michael: important native species in California
warm_species <- "Stipa pulchra" # Susan: Stipa pulchra is the state grass, and is the subject of a lot of ecological research and restoration effort

gbif_terraclim_sf <- gbif_terraclim_sf %>%
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

tmp_cwd_gg <-
  ggplot(
    data = gbif_terraclim_sf %>%
      filter(
        tmp > quantile(tmp, .0001, na.rm = T), tmp < quantile(tmp, .9999, na.rm = T), # remove extreme climate points for plotting
        cwd > quantile(cwd, .0001, na.rm = T), cwd < quantile(cwd, .9999, na.rm = T)
      ),
    mapping = aes(tmp, cwd)
  ) +
  geom_point(
    aes(color = species_type, shape = species_type, alpha = species_type)
  ) +
  scale_color_manual(values = c(cool = "blue", other = gray(.5), warm = "red")) +
  scale_shape_manual(values = c(cool = 19, other = 20, warm = 19)) +
  scale_alpha_manual(values = c(cool = .6, other = .01, warm = .6)) +
  labs(x = .varname$tmp, y = .varname$cwd) +
  guides(color = "none", shape = "none", alpha = "none")

ppt_cwd_gg <-
  ggplot(
    data = gbif_terraclim_sf %>%
      filter(
        ppt > quantile(ppt, .0001, na.rm = T), ppt < quantile(ppt, .9999, na.rm = T), # remove extreme climate points for plotting
        cwd > quantile(cwd, .0001, na.rm = T), cwd < quantile(cwd, .9999, na.rm = T)
      ),
    mapping = aes(ppt, cwd)
  ) +
  geom_point(
    aes(color = species_type, shape = species_type, alpha = species_type)
  ) +
  scale_color_manual(values = c(cool = "blue", other = gray(.5), warm = "red")) +
  scale_shape_manual(values = c(cool = 19, other = 20, warm = 19)) +
  scale_alpha_manual(values = c(cool = .6, other = .01, warm = .6)) +
  labs(x = .varname$ppt, y = .varname$cwd) +
  guides(color = "none", shape = "none", alpha = "none")

tmp_ppt_gg <-
  ggplot(
    data = gbif_terraclim_sf %>%
      filter(
        ppt > quantile(ppt, .0001, na.rm = T), ppt < quantile(ppt, .9999, na.rm = T), # remove extreme climate points for plotting
        tmp > quantile(tmp, .0001, na.rm = T), tmp < quantile(tmp, .9999, na.rm = T)
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

# combine panels
cwd_niche_gg <- ppt_cwd_gg + tmp_cwd_gg + tmp_ppt_gg +
  plot_annotation(tag_levels = "A") +
  plot_layout(design = "
  ABC
  ")

# save figure file
if (.fig_save) {
  ggsave(
    plot = cwd_niche_gg,
    filename = str_c(.path$out_fig, "fig-supp-niche-cwd.png"),
    width = 12,
    height = 5
  )
}

# report stats
cwd_ppt_cor <- cor.test(~ cwd + ppt, data = gbif_terraclim_sf)
cwd_tmp_cor <- cor.test(~ cwd + tmp, data = gbif_terraclim_sf)
ppt_tmp_cor <- cor.test(~ ppt + tmp, data = gbif_terraclim_sf)
