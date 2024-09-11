plot_individual_distribution <- function(data_occ,
                                         cool_species = "Danthonia californica",
                                         warm_species = "Stipa pulchra",
                                         frac = 1) {
  occ_sf <- dat_occ[["gbif"]] %>%
    select(species = consolidatedName, geometry) %>%
    mutate(species_type = case_when(
      species == cool_species ~ "cool",
      species == warm_species ~ "warm",
      TRUE ~ "other"
    ))

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
    ggrastr::rasterise(geom_sf(
      data = occ_sf %>% sample_frac(frac),
      color = "lightgray",
      shape = 16,
      alpha = 0.01
    ), dpi = 1200) +
    ggrastr::rasterise(geom_sf(
      data = occ_sf %>%
        filter(species_type == "cool") %>%
        st_buffer(1e4) %>% # buffer to better show overlapping points
        st_union(),
      fill = "blue",
      color = NA,
      alpha = 0.4
    ), dpi = 1200) +
    ggrastr::rasterise(geom_sf(
      data = occ_sf %>%
        filter(species_type == "warm") %>%
        st_buffer(1e4) %>% # buffer to better show overlapping points
        st_union(),
      fill = "red",
      color = NA,
      alpha = 0.4
    ), dpi = 1200) +
    coord_sf(xlim = c(-126, -114), ylim = c(28, 44)) +
    scale_x_continuous(breaks = c(-125, -120, -115)) +
    scale_y_continuous(breaks = c(30, 35, 40)) +
    labs(x = "Longitude", y = "Latitude") +
    guides(fill = "none", color = "none", shape = "none", alpha = "none")

  return(occ_geog_gg)
}

plot_individual_trait <- function(dat_trait,
                                  cool_species = "Danthonia californica",
                                  warm_species = "Stipa pulchra",
                                  frac = 1) {
  trait_tbl <- dat_trait %>%
    mutate(species_type = case_when(
      species == cool_species ~ "cool",
      species == warm_species ~ "warm",
      TRUE ~ "other"
    )) %>%
    filter(
      tmp > quantile(tmp, .0001), tmp < quantile(tmp, .9999), # remove extreme climate points for plotting
      ppt > quantile(ppt, .0001), ppt < quantile(ppt, .9999)
    ) %>%
    mutate(
      scaled_tmp = (tmp - min(tmp)) / (max(tmp) - min(tmp)),
      scaled_ppt = (ppt - min(ppt)) / (max(ppt) - min(ppt))
    )

  trait_sf <- trait_tbl %>%
    st_as_sf(coords = c("scaled_tmp", "scaled_ppt"), crs = 4326)

  ind_trait_gg <-
    ggplot() +
    ggrastr::rasterise(geom_sf(
      data = trait_sf %>% sample_frac(frac),
      color = "lightgray",
      shape = 16,
      alpha = 0.01
    ), dpi = 1200) +
    ggrastr::rasterise(geom_sf(
      data = trait_sf %>%
        filter(species_type == "cool") %>%
        st_buffer(1e3) %>% # buffer to better show overlapping points
        st_union(),
      fill = "blue",
      color = NA,
      alpha = 0.4
    ), dpi = 1200) +
    ggrastr::rasterise(geom_sf(
      data = trait_sf %>%
        filter(species_type == "warm") %>%
        st_buffer(1e3) %>% # buffer to better show overlapping points
        st_union(),
      fill = "red",
      color = NA,
      alpha = 0.4
    ), dpi = 1200) +
    labs(
      x = "Mean annual temperature (°C)",
      y = "Annual precipitation (mm)"
    ) +
    scale_x_continuous(
      breaks = (seq(0, 20, 5) - min(trait_tbl$tmp)) / (max(trait_tbl$tmp) - min(trait_tbl$tmp)),
      labels = seq(0, 20, 5)
    ) +
    scale_y_continuous(
      breaks = (seq(0, 3000, 1000) - min(trait_tbl$ppt)) / (max(trait_tbl$ppt) - min(trait_tbl$ppt)),
      labels = seq(0, 3000, 1000)
    ) +
    guides(color = "none", shape = "none", alpha = "none")

  return(ind_trait_gg)
}
