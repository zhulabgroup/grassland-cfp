print_ind_sp_niche <- function(dat_occ, dat_trait, dat_niche, cfp_sf, outfile = "output/figures/species-climate-niche.pdf") {
  sp_vec <- dat_niche %>%
    filter(occ_n > 100) %>% # no dummy species
    pull(species)

  niche_gg <- vector(mode = "list")
  for (sp in sp_vec) {
    niche_gg[[sp]] <- plot_sp_niche(dat_occ, dat_trait, cfp_sf, sp = sp) # no need to print(); will slow down
  }

  # runtime ~= 4 min
  pdf(outfile, width = 8, height = 8 * .618)
  print(niche_gg)
  dev.off()
}
plot_ind_sp_niche <- function(dat_occ, dat_trait, cfp_sf, sp = "Danthonia californica") {
  geo_sp <- dat_occ$gbif %>%
    rename(species = consolidatedName) %>%
    filter(species == sp)

  trait_sp <- dat_trait %>%
    filter(species == sp)

  niche_sp <- dat_trait %>%
    filter(species == sp) %>%
    summarise(
      occ_n = n(),
      tmp_occ_median = median(tmp, na.rm = TRUE),
      tmp_occ_sd = sd(tmp, na.rm = TRUE),
      ppt_occ_median = median(ppt, na.rm = TRUE),
      ppt_occ_sd = sd(ppt, na.rm = TRUE)
    )

  # gbif summary
  tmp_rng <- range(dat_trait$tmp)
  ppt_rng <- range(dat_trait$ppt)
  n_occ_tot <- nrow(trait_sp)

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
    geom_sf(data = geo_sp, alpha = .1, size = .5) +
    labs(x = "Longitude", y = "Latitude", title = sp) +
    coord_sf(xlim = c(-126, -114), ylim = c(28, 44)) +
    scale_x_continuous(breaks = c(-125, -120, -115)) +
    scale_y_continuous(breaks = c(30, 35, 40)) +
    theme(plot.title = element_text(face = "italic"))

  occ_clim <-
    ggplot(trait_sp, aes(tmp, ppt)) +
    geom_point(alpha = .1, size = .5) +
    geom_rug(alpha = .1) +
    stat_ellipse(col = "red") +
    geom_point(
      data = niche_sp,
      aes(x = tmp_occ_median, y = ppt_occ_median),
      shape = 3, col = "red", size = 10
    ) +
    lims(x = tmp_rng, y = ppt_rng) +
    labs(
      x = "Mean annual temperature (°C)",
      y = "Annual precipitation (mm)",
      title = bquote(italic("n") ~ "=" ~ .(format(n_occ_tot, big.mark = ",", trim = TRUE)))
    )

  out <- occ_geog + occ_clim

  return(out)
}

plot_all_niche <- function(dat_occ, dat_trait, dat_niche, cfp_sf,
                           cool_species = "Danthonia californica",
                           warm_species = "Stipa pulchra") {
  # example species
  # Michael: important native species in California
  # Susan: Stipa pulchra is the state grass, and is the subject of a lot of ecological research and restoration effort

  occ_geog_gg <- plot_occ_geo(dat_occ, cool_species, warm_species, cfp_sf)
  ind_trait_gg <- plot_ind_trait(dat_trait, cool_species, warm_species)
  sp_niche_gg <- plot_sp_niche(dat_niche, cool_species, warm_species)

  # combine panels
  niche_gg <- occ_geog_gg + ind_trait_gg + sp_niche_gg +
    plot_annotation(tag_levels = "A") +
    plot_layout(design = "
  AABB
  AABB
  CCCC
  CCCC
  CCCC
  ")

  out <- list(
    occ_geo = occ_geog_gg,
    ind_trait = ind_trait_gg,
    sp_niche = sp_niche_gg,
    combined = niche_gg
  )

  return(out)
}

plot_occ_geo <- function(data_occ,
                         cool_species = "Danthonia californica",
                         warm_species = "Stipa pulchra",
                         cfp_sf,
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

plot_ind_trait <- function(dat_trait,
                           cool_species = "Danthonia californica",
                           warm_species = "Stipa pulchra") {
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

plot_sp_niche <- function(dat_niche,
                          cool_species = "Danthonia californica",
                          warm_species = "Stipa pulchra") {
  niche_tbl <- dat_niche %>%
    mutate(species_type = case_when(
      species == cool_species ~ "cool",
      species == warm_species ~ "warm",
      TRUE ~ "other"
    )) %>%
    filter(occ_n > 100) # no dummy species
  sp_niche_gg <-
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
    labs(
      x = "Mean annual temperature (°C)",
      y = "Annual precipitation (mm)"
    ) +
    guides(color = "none")
  return(sp_niche_gg)
}
