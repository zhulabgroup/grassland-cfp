# import niche and observational data, calculate CTI and CPI
niche_tbl <- read_rds(.path$sum_niche) %>%
  filter(occ_n > 100 | is.na(occ_n)) # species with many observations and dummy species

obs_tbl <- read_rds(.path$com_obs) %>%
  inner_join(niche_tbl, by = "species") %>%
  group_by(site, year, plot) %>%
  summarize(
    tmp_com_mean = sum(abund * tmp_occ_median) / sum(abund),
    ppt_com_mean = sum(abund * ppt_occ_median) / sum(abund)
  )

# setup site labels
site_vec <- c(
  angelo = "Angelo Coast",
  carrizo = "Carrizo Plain",
  elkhorn = "Elkhorn Slough",
  jasper = "Jasper Ridge Serpentine",
  mclann = "McLaughlin Annual",
  mclserp = "McLaughlin Serpentine",
  morganterritory = "Morgan Territory",
  pleasantonridge = "Pleasanton Ridge",
  sunol = "Sunol",
  swanton = "Swanton Ranch",
  ucsc = "UC Santa Cruz",
  vascocaves = "Vasco Caves"
)

# reshape data
obs_idx_tbl <- obs_tbl %>%
  dplyr::select(site, year, plot, tmp_com_mean, ppt_com_mean) %>%
  pivot_longer(cols = tmp_com_mean:ppt_com_mean, names_to = "com_idx_name", values_to = "com_idx_value") %>%
  mutate(com_idx_name = factor(com_idx_name,
    levels = c("tmp_com_mean", "ppt_com_mean"),
    labels = c("CTI", "CPI")
  ))

samp_size_tbl <- obs_tbl %>%
  group_by(site) %>%
  summarize(
    n_plot = n_distinct(plot),
    n_year = n_distinct(year)
  )

# define plotting function
plot_cwm <- function(tbl, site_name, cti_lab = "", cpi_lab = "", yr_lab = NULL, yr_axis = FALSE) {
  # prepare site data
  site_lab <- site_vec[site_name]
  site_lab <- str_c(LETTERS[which(site_vec == site_lab)], site_lab, sep = ". ")

  site_tbl <- tbl %>%
    filter(site == site_name) %>%
    group_by(com_idx_name) %>%
    nest() %>%
    mutate( # lm and p value
      p_val = map(data, ~ lm(com_idx_value ~ year, data = .)) %>%
        map_dbl(~ broom::glance(.) %>% pull(p.value))
    ) %>%
    unnest(cols = data)

  # plot
  out_gg <- ggplot(site_tbl, aes(year, com_idx_value)) +
    geom_boxplot(aes(group = year), color = "gray", outlier.shape = 20) +
    geom_smooth( # add lm trend line when significant
      data = . %>% filter(p_val <= 0.05),
      method = "lm", formula = y ~ x, se = FALSE,
      color = "red"
    ) +
    facet_wrap(~com_idx_name,
      ncol = 1, scales = "free_y",
      strip.position = "left",
      labeller = labeller(com_idx_name = c(
        CTI = cti_lab, # "Community Temperature Index\n(CTI, °C)",
        CPI = cpi_lab # "Community Precipitation Index\n(CPI, mm)"
      ))
    ) +
    expand_limits(x = range(tbl$year)) +
    labs(
      x = yr_lab, y = NULL,
      title = site_lab
    ) +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside",
      plot.title = element_text(size = 11)
    )

  # remove yr axis text
  if (yr_axis) {
    return(out_gg)
  } else {
    return(out_gg + theme(axis.text.x = element_blank()))
  }
}

# read site data
site_sf <- read_rds(.path$geo_site) %>%
  filter(abbr %in% c("angelo", "carrizo", "elkhorn", "jasper", "mclann", "morganterritory", "pleasantonridge", "sunol", "swanton", "ucsc", "vascocaves")) %>%
  arrange(abbr) %>%
  add_column(lab = c(LETTERS[1:4], "E/F", LETTERS[7:12]))

# read cfp data
cfp_sf <- st_read(.path$geo_cfp, quiet = TRUE) %>%
  filter(
    NAME == "California Floristic Province",
    Type == "hotspot area"
  )

# use terra (not raster) to process grassland percent cover
if (FALSE) {
  grass_ras <- .path$geo_grass %>%
    str_c("MCD12C1.A2020001.006.2021362215328.hdf") %>%
    terra::rast() %>%
    terra::crop(terra::ext(cfp_sf)) %>%
    terra::mask(cfp_sf) %>%
    terra::subset("Land_Cover_Type_1_Percent_10") # User guide at https://lpdaac.usgs.gov/documents/101/MCD12_User_Guide_V6.pdf

  terra::plot(grass_ras)

  terra::writeRaster(grass_ras, str_c(.path$geo_grass, "cfp-grassland-percent-cover.tif"))
}

grass_tbl <- .path$geo_grass %>%
  str_c("cfp-grassland-percent-cover.tif") %>%
  terra::rast() %>%
  terra::as.data.frame(xy = T) %>%
  as_tibble() %>%
  select(x, y, percent = Land_Cover_Type_1_Percent_10)

# # estimate grassland proportion
# lc_ras %>%
#   raster::as.data.frame(xy = T) %>%
#   drop_na() %>%
#   group_by(Majority_Land_Cover_Type_1) %>%
#   summarise(count = n()) %>%
#   ungroup() %>%
#   mutate(proportion = count / sum(count)) %>%
#   arrange(desc(proportion))

# make map
set.seed(618)
site_map_gg <-
  ggplot() +
  geom_sf(
    data = rnaturalearth::ne_states(
      country = c("Mexico", "United States of America"),
      returnclass = "sf"
    ),
    fill = NA,
    color = alpha("black", .1)
  ) +
  geom_sf(
    data = cfp_sf,
    color = alpha("black", .3),
    fill = alpha("white", .1)
  ) +
  geom_tile(
    data = grass_tbl,
    aes(x, y, alpha = percent),
    fill = "yellow green"
  ) +
  geom_sf(data = site_sf, color = "red") +
  ggrepel::geom_label_repel(
    data = site_sf,
    mapping = aes(
      label = lab, # name,
      geometry = geometry
    ),
    stat = "sf_coordinates",
    size = 3,
    color = "red",
    fill = NA,
    min.segment.length = 0,
    max.overlaps = Inf,
    label.padding = unit(.25, "lines"),
    label.size = NA
  ) +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(-126, -114), ylim = c(28, 44)) +
  scale_x_continuous(breaks = c(-125, -120, -115)) +
  scale_y_continuous(breaks = c(30, 35, 40)) +
  guides(alpha = "none")

# apply plotting function, make individual panels, combine
obs_gg <-
  site_map_gg +
  plot_cwm(obs_idx_tbl, "angelo", cti_lab = "CTI (°C)", cpi_lab = "CPI (mm)") +
  plot_cwm(obs_idx_tbl, "carrizo") +
  plot_cwm(obs_idx_tbl, "elkhorn", cti_lab = "CTI (°C)", cpi_lab = "CPI (mm)") +
  plot_cwm(obs_idx_tbl, "jasper") +
  plot_cwm(obs_idx_tbl, "mclann", cti_lab = "CTI (°C)", cpi_lab = "CPI (mm)") +
  plot_cwm(obs_idx_tbl, "mclserp") +
  plot_cwm(obs_idx_tbl, "morganterritory") +
  plot_cwm(obs_idx_tbl, "pleasantonridge") +
  plot_cwm(obs_idx_tbl, "sunol", cti_lab = "CTI (°C)", cpi_lab = "CPI (mm)", yr_axis = TRUE) +
  plot_cwm(obs_idx_tbl, "swanton", yr_axis = TRUE) +
  plot_cwm(obs_idx_tbl, "ucsc", yr_axis = TRUE) +
  plot_cwm(obs_idx_tbl, "vascocaves", yr_axis = TRUE) +
  plot_layout(
    design = "
  AABC
  AADE
  FGHI
  JKLM
"
  )

# save figure file
if (.fig_save) {
  ggsave(
    plot = obs_gg,
    filename = str_c(.path$out_fig, "fig-main-obs-comb.png"),
    width = 12,
    height = 12*1.5
  )
}
