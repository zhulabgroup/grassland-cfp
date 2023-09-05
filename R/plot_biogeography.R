plot_biogeography <- function(dat_occ) {
  dat_occ_all <- bind_rows(dat_occ) %>%
    mutate(dataset = factor(dataset,
      levels = c("gbif", "bien", "cch", "inat"),
      labels = c("GBIF", "BIEN", "CCH", "iNat")
    ))

  set.seed(618)
  prop_samp <- .1 # prop to sample and plot
  occ_comp_gg <- ggplot() +
    geom_sf(
      data = rnaturalearth::ne_states(
        country = c("Mexico", "United States of America"),
        returnclass = "sf"
      ),
      fill = NA,
      color = alpha("black", .1)
    ) +
    # geom_sf(data = cfp_sf, fill = "white", alpha = .5) +
    geom_sf(
      data = dat_occ_all %>% slice_sample(prop = prop_samp), # randomly sample certain proportion, otherwise too slow to render
      color = "black", alpha = .1, size = .1
    ) +
    labs(x = "Longitude", y = "Latitude") +
    coord_sf(xlim = c(-126, -114), ylim = c(28, 44)) +
    scale_x_continuous(breaks = c(-125, -120, -115)) +
    scale_y_continuous(breaks = c(30, 35, 40)) +
    facet_wrap(. ~ dataset,
      nrow = 1
    ) +
    theme(
      strip.background = element_blank(),
      # strip.text.x = element_text(hjust = 0)
    )
}
