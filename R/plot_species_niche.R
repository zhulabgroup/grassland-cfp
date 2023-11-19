#' @export
plot_species_niche <- function(dat_niche,
                               cool_species = "Danthonia californica", # Michael: important native species in California
                               warm_species = "Stipa pulchra" # Susan: Stipa pulchra is the state grass, and is the subject of a lot of ecological research and restoration effort
) {
  niche_tbl <- dat_niche %>%
    mutate(species_type = case_when(
      species == cool_species ~ "cool",
      species == warm_species ~ "warm",
      TRUE ~ "other"
    )) %>%
    filter(!is.na(occ_n)) # no dummy species

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
