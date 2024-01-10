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
    geom_sf(
      data = occ_sf %>% sample_frac(frac),
      color = gray(0.5),
      shape = 20,
      alpha = 0.01
    ) +
    geom_sf(
      data = occ_sf %>%
        filter(species_type == "cool") %>%
        st_buffer(1e4) %>% # buffer to better show overlapping points
        st_union(),
      fill = "blue",
      color = NA,
      alpha = 0.4
    ) +
    geom_sf(
      data = occ_sf %>%
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
    )

  trait_tbl <- bind_rows(
    trait_tbl %>%
      filter(species_type == "other") %>%
      sample_frac(frac),
    trait_tbl %>%
      filter(species_type != "other")
  )

  ind_trait_gg <-
    ggplot(
      data = trait_tbl,
      mapping = aes(tmp, ppt)
    ) +
    geom_point(
      aes(color = species_type, shape = species_type, alpha = species_type)
    ) +
    scale_color_manual(values = c(cool = "blue", other = gray(.5), warm = "red")) +
    scale_shape_manual(values = c(cool = 19, other = 20, warm = 19)) +
    scale_alpha_manual(values = c(cool = .6, other = .01, warm = .6)) +
    labs(
      x = "Mean annual temperature (°C)",
      y = "Annual precipitation (mm)"
    ) +
    guides(color = "none", shape = "none", alpha = "none")

  return(ind_trait_gg)
}
