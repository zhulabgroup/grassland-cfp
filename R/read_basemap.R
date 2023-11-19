read_cfp <- function(path_cfp = system.file("extdata", "cfp", package = "grassland")) {
  sf_cfp <- st_read(path_cfp, quiet = TRUE) %>%
    filter(
      NAME == "California Floristic Province",
      Type == "hotspot area"
    ) %>%
    select(-NAME, -Type)

  return(sf_cfp)
}

read_grasscover <- function(path_grass = system.file("extdata", "cfp-grassland-percent-cover.tif", package = "grassland")) {
  ras_grass <- terra::rast(path_grass)
  return(ras_grass)
}
