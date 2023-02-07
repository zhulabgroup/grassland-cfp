# read site data
site_sf <- read_rds(.path$geo_site) %>%
  filter(abbr %in% c("angelo", "carrizo", "elkhorn", "jasper", "mclann", "morganterritory", "pleasantonridge", "sunol", "swanton", "ucsc", "vascocaves")) %>%
  arrange(abbr) %>%
  add_column(lab = c(LETTERS[1:4], "E/F", LETTERS[7:12]))

# make site table
site_tbl <- site_sf %>%
  extract(geometry, c("latitude", "longitude"), "\\((.*), (.*)\\)", convert = TRUE) %>%
  select(label = lab, name, latitude, longitude, grass_type, data_method) %>%
  mutate(
    name = if_else(label == "E/F", str_c(name, "/Serpentine"), name),
    grass_type = if_else(label == "E/F", str_c(grass_type, "/Serpentine"), grass_type)
  )

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

# save figure
if (.fig_save) {
  ggsave(
    plot = site_map_gg,
    filename = str_c(.path$out_fig, "fig-supp-site-map2.pdf"),
    width = 7,
    height = 10
  )
}
