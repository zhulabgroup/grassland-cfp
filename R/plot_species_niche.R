plot_species_niche <- function(dat_niche,
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
