plot_site_map <- function(cfp_sf, grass_ras) {
  # read site data
  site_sf <- site_info() %>%
    filter(abbr %in% c("angelo", "carrizo", "elkhorn", "jasper", "mclann", "morganterritory", "pleasantonridge", "sunol", "swanton", "ucsc", "vascocaves")) %>%
    arrange(abbr) %>%
    add_column(lab = c(LETTERS[1:4], "E/F", LETTERS[7:12]))

  grass_tbl <- grass_ras %>%
    terra::as.data.frame(xy = T) %>%
    as_tibble() %>%
    select(x, y, percent = Land_Cover_Type_1_Percent_10)

  # make map
  set.seed(618)
  site_map_gg <-
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
    geom_tile(
      data = grass_tbl,
      aes(x, y, alpha = percent),
      fill = "yellow green"
    ) +
    geom_sf(data = site_sf, color = "black") +
    ggrepel::geom_label_repel(
      data = site_sf,
      mapping = aes(
        label = lab, # name,
        geometry = geometry
      ),
      stat = "sf_coordinates",
      size = 3,
      color = "black",
      fill = NA,
      min.segment.length = 0,
      max.overlaps = Inf,
      label.padding = unit(.25, "lines"),
      label.size = NA
    ) +
    labs(x = "Longitude", y = "Latitude", alpha = "Grassland\npercent\ncover") +
    coord_sf(xlim = c(-126, -114), ylim = c(28, 44)) +
    scale_x_continuous(breaks = c(-125, -120, -115)) +
    scale_y_continuous(breaks = c(30, 35, 40)) +
    theme(
      legend.position = c(0.2, 0.2),
      legend.title = element_text(size = 10)
    ) +
    guides(alpha = guide_legend(reverse = TRUE))

  # grass_map_gg <-
  #   ggplot() +
  #   geom_sf(
  #     data = rnaturalearth::ne_states(
  #       country = c("Mexico", "United States of America"),
  #       returnclass = "sf"
  #     ),
  #     fill = NA,
  #     color = alpha("black", .1)
  #   ) +
  #   geom_sf(
  #     data = cfp_sf,
  #     color = alpha("black", .3),
  #     fill = alpha("white", .1)
  #   ) +
  #   geom_tile(
  #     data = grass_tbl,
  #     aes(x, y, alpha = percent),
  #     fill = "yellow green"
  #   ) +
  #   labs(x = "Longitude", y = "Latitude", alpha = "Grassland\npercent\ncover") +
  #   coord_sf(xlim = c(-126, -114), ylim = c(28, 44)) +
  #   scale_x_continuous(breaks = c(-125, -120, -115)) +
  #   scale_y_continuous(breaks = c(30, 35, 40)) +
  #   theme(
  #     legend.position = c(0.2, 0.2),
  #     legend.title = element_text(size = 10)
  #   ) +
  #   guides(alpha = guide_legend(reverse = TRUE))

  return(site_map_gg)
}

site_info <- function() {
  # site info data
  site_sf <- tribble(
    ~abbr, ~name, ~latitude, ~longitude, ~grass_type, ~data_method,
    "angelo", "Angelo Coast", "39 43 02.5", "-123 39 11.1", "Valley", "Cover in 30x0.09-m2 quadrats",
    "carrizo", "Carrizo Plain", "35 11 20.6", "-119 51 45.2", "Valley", "9x9 point intercept in 1-m2 quadrats",
    "elkhorn", "Elkhorn Slough", "36 52 02.4", "-121 44 27.5", "Coastal prairie", "5x5 point intercept in 0.25-m2 quadrats",
    "jasper", "Jasper Ridge", "37 24 22.4", "-122 14 31.8", "Serpentine", "Cover in 1-m2 quadrats",
    "jrgce", "Jasper Ridge Global Change Experiment", "37 24 22.4", "-122 14 31.8", "???", "Point intercept (?)",
    "mclann", "McLaughlin Annual", "38 52 11.2", "-122 25 15.6", "Valley", "Cover in 1-m2 quadrats; first 6 years are richness only",
    "mclserp", "McLaughlin Serpentine", "38 52 11.2", "-122 25 15.6", "Serpentine", "Cover in 1-m2 quadrats; first 6 years are richness only",
    "morganterritory", "Morgan Territory", "37 49 5.92", "-121 47 45.18", "Valley", "70 point-intercepts along four 17-m transects; tallest species only",
    "pleasantonridge", "Pleasanton Ridge", "37 36 55.47384", "-121 53 4.4142", "Valley", "70 point-intercepts along four 17-m transects; tallest species only",
    "scide", "IDE Arboretum", "36 58 38.856", "-122 03 8.136", "???", "???",
    "scide", "IDE Marshall Field", "37 0 57", "-122 4 45", "???", "???",
    "scide", "IDE Younger Lagoon", "36 57 0", "-122 4 0", "???", "???",
    "sunol", "Sunol", "37 30 36.66", "-121 49 42.78", "Valley", "70 point-intercepts along four 17-m transects; tallest species only",
    "swanton", "Swanton Ranch", "37 02 43.1", "-122 13 16.1", "Coastal prairie", "5x5 point intercept in 0.25-m2 quadrats",
    "ucsc", "UC Santa Cruz", "36 59 11.1", "-122 03 09.2", "Coastal prairie", "5x5 point intercept in 0.25-m2 quadrats",
    "vascocaves", "Vasco Caves", "37 48 18", "-121 41 13.2", "???", "???"
  ) %>%
    mutate(
      latitude = measurements::conv_unit(latitude, from = "deg_min_sec", to = "dec_deg") %>% as.numeric(),
      longitude = measurements::conv_unit(longitude, from = "deg_min_sec", to = "dec_deg") %>% as.numeric()
    ) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

  return(site_sf)
}
