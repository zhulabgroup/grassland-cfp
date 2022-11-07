# read CFP data
cfp_sf <- st_read(.path$geo_cfp) %>%
  filter(
    NAME == "California Floristic Province",
    Type == "hotspot area"
  )

# read GBIF-CHELSA data
gbif_chelsa_sf <- read_rds(.path$geo_clim) %>%
  dplyr::select(geometry, key, species, tmp = chelsa_tmp, ppt = chelsa_ppt, vpd = chelsa_vpd) %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")

# check species lists
sp_gbif_vec <- read_rds(.path$sum_niche) %>%
  filter(occ_n > 100) %>% # no dummy species
  pull(species)

# gbif summary
tmp_rng <- range(gbif_chelsa_sf$tmp)
ppt_rng <- range(gbif_chelsa_sf$ppt)
n_occ_tot <- nrow(gbif_chelsa_sf)

# multi-page plot
niche_gg <- vector(mode = "list")
for (i in seq_along(sp_gbif_vec)) {
  sp <- sp_gbif_vec[i]
  occ_sp_sf <- filter(gbif_chelsa_sf, species == sp)
  occ_sp_stat <- occ_sp_sf %>%
    as_tibble() %>%
    summarise(
      occ_n = n(),
      tmp_occ_median = median(tmp, na.rm = TRUE),
      tmp_occ_sd = sd(tmp, na.rm = TRUE),
      ppt_occ_median = median(ppt, na.rm = TRUE),
      ppt_occ_sd = sd(ppt, na.rm = TRUE)
    )

  occ_geog <-
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
    geom_sf(data = occ_sp_sf, alpha = .1, size = .5) +
    labs(x = "Longitude", y = "Latitude", title = sp) +
    coord_sf(xlim = c(-126, -114), ylim = c(28, 44)) +
    scale_x_continuous(breaks = c(-125, -120, -115)) +
    scale_y_continuous(breaks = c(30, 35, 40)) +
    theme(plot.title = element_text(face = "italic"))

  occ_clim <-
    ggplot(occ_sp_sf, aes(tmp, ppt)) +
    geom_point(alpha = .1, size = .5) +
    geom_rug(alpha = .1) +
    stat_ellipse(col = "red") +
    geom_point(
      data = occ_sp_stat,
      aes(x = tmp_occ_median, y = ppt_occ_median),
      shape = 3, col = "red", size = 10
    ) +
    lims(x = tmp_rng, y = ppt_rng) +
    labs(
      x = "Mean annual temperature (°C)", y = "Mean annual precipitation (mm)",
      title = bquote(italic("n") ~ "=" ~ .(format(occ_sp_stat$occ_n, big.mark = ",", trim = TRUE)))
    )

  # print(sp)
  niche_gg[[i]] <- occ_geog + occ_clim # no need to print(); will slow down
}
names(niche_gg) <- sp_gbif_vec
