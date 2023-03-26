# read CFP data
cfp_sf <- st_read(.path$geo_cfp, quiet = TRUE) %>%
  filter(
    NAME == "California Floristic Province",
    Type == "hotspot area"
  )

# read GBIF-CHELSA data
gbif_chelsa_sf <- read_rds(.path$geo_clim) %>%
  dplyr::select(geometry, key, species, tmp = chelsa_tmp, ppt = chelsa_ppt) %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")

# check species lists
sp_gbif_vec <- read_rds(.path$sum_niche) %>%
  filter(occ_n > 100) %>% # no dummy species
  pull(species)

# gbif summary
tmp_rng <- range(gbif_chelsa_sf$tmp)
ppt_rng <- range(gbif_chelsa_sf$ppt)
n_occ_tot <- nrow(gbif_chelsa_sf)
n_sp_tot <- length(sp_gbif_vec)

plot_sp_niche <- function(sp = "Danthonia californica") {
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

  occ_geog + occ_clim
}
