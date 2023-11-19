#' Plot Site Map
#'
#' This function reads and plots a map showing the grassland percentage cover,
#' and optionally plots the sites with observed data.
#'
#' @param sf_cfp A simple feature object that contains the California Floristic Province data.
#' By default, it reads the "cfp" file from the package's "extdata" directory.
#' @param ras_grass A raster object that contains the grassland percent cover data.
#' By default, it reads the "cfp-grassland-percent-cover.tif" file from the package's "extdata" directory.
#' @param plotsite A boolean variable that decides whether to include sites in the plot. Default is TRUE.
#'
#' @return A ggplot object of the site map.
#' @examples
#' \dontrun{
#' site_map <- plot_site_map()
#' site_map
#' }
#' @export
plot_site_map <- function(sf_cfp = NULL, ras_grass = NULL, plotsite = T) {
  if (is.null(sf_cfp)) {
    sf_cfp <- read_cfp(path_cfp = system.file("extdata", "cfp", package = "grassland"))
  }

  if (is.null(ras_grass)) {
    ras_grass <- read_grasscover(path_grass = system.file("extdata", "cfp-grassland-percent-cover.tif", package = "grassland"))
  }

  grass_tbl <- ras_grass %>%
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
      ) %>%
        sf::st_transform(sf::st_crs(sf_cfp)$proj4string),
      fill = NA,
      color = alpha("black", .1)
    ) +
    geom_sf(
      data = sf_cfp,
      color = alpha("black", .3),
      fill = alpha("white", .1)
    ) +
    geom_tile(
      data = grass_tbl,
      aes(x, y, alpha = percent),
      fill = "yellow green"
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

  if (plotsite) {
    # read site data
    site_sf <- read_site_info(subset = "obs") %>%
      arrange(site) %>%
      add_column(lab = c(LETTERS[1:4], "E/F", LETTERS[7:12]))

    site_map_gg <- site_map_gg +
      geom_point(
        data = site_sf,
        mapping = aes(
          geometry = geometry
        ),
        stat = "sf_coordinates",
        color = "black"
      ) +
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
      )
  }
  return(site_map_gg)
}
