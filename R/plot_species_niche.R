#' Plot Species Niche
#'
#' This function plots species in the climate niche space based on their temperature and precipitation optima.
#' Blue and red colors highlight two species associated with cool and warm conditions, respectively.
#'
#' @param dat_niche A data frame that contains the species climate niche.
#' @param cool_species A string representing a species associated with cool conditions (defaulted to "Danthonia californica").
#' @param warm_species A string representing a species associated with warm conditions (defaulted to "Stipa pulchra").
#'
#' @return A ggplot2 object showing species climate niche.
#' @examples
#' \dontrun{
#' p_niche <- plot_species_niche(dat_niche = my_data)
#' p_niche <- plot_species_niche(dat_niche = my_data, cool_species = "Cool Species", warm_species = "Warm Species")
#' }
#' @export
plot_species_niche <- function(dat_niche,
                               cool_species = "Danthonia californica",
                               warm_species = "Stipa pulchra") {
  # Danthonia californica: important native species in California (Michael)
  # Stipa pulchra: the state grass, and is the subject of a lot of ecological research and restoration effort (Susan)
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
      data = niche_tbl %>% filter(species_type == "cool"),
      fill = NA,
      fontface = "italic",
      label.padding = unit(.5, "lines"),
      label.size = 0,
      vjust = "bottom",
      hjust = 0.75
    ) +
    geom_label(
      data = niche_tbl %>% filter(species_type == "warm"),
      fill = NA,
      fontface = "italic",
      label.padding = unit(.5, "lines"),
      label.size = 0,
      vjust = "top",
      hjust = 0.25
    ) +
    scale_color_manual(values = c(cool = "blue", other = gray(.75), warm = "red")) +
    labs(
      x = "Mean annual temperature (°C)",
      y = "Annual precipitation (mm)"
    ) +
    guides(color = "none")

  return(sp_niche_gg)
}
