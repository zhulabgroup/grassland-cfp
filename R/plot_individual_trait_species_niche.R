# docu

#' @export
plot_individual_trait_species_niche_ind_sp <- function(dat_occ, dat_trait, sf_cfp = NULL, sp = "Danthonia californica") {
  if (is.null(sf_cfp)) {
    sf_cfp <- read_cfp(path_cfp = system.file("extdata", "cfp", package = "grassland"))
  }

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
      data = sf_cfp,
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

#' @export
plot_individual_trait_species_niche_all <- function(dat_occ, dat_trait, dat_niche, # sf_cfp,
                                                    cool_species = "Danthonia californica",
                                                    warm_species = "Stipa pulchra",
                                                    frac = 1) {
  # example species
  # Michael: important native species in California
  # Susan: Stipa pulchra is the state grass, and is the subject of a lot of ecological research and restoration effort

  occ_geog_gg <- plot_individual_distribution(dat_occ, cool_species, warm_species, frac)
  ind_trait_gg <- plot_individual_trait(dat_trait, cool_species, warm_species, frac)
  sp_niche_gg <- plot_species_niche(dat_niche, cool_species, warm_species)

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
