#' @export
summ_cfp_geography <- function() {
  sf_cfp <- read_cfp(path_cfp = system.file("extdata", "cfp", package = "grassland"))

  bbox_cfp <- sf::st_bbox(sf_cfp)

  return(bbox_cfp)
}

#' @export
summ_cfp_climate <- function(dat_clim, source = "chelsa") {
  sf_cfp <- read_cfp(path_cfp = system.file("extdata", "cfp", package = "grassland"))

  df_chelsa_cfp <- dat_clim[[source]] %>%
    terra::crop(sf_cfp) %>%
    terra::mask(sf_cfp) %>%
    as.data.frame() %>%
    summarise(
      t_min = min(tmp, na.rm = T),
      t_max = max(tmp, na.rm = T),
      p_min = min(ppt, na.rm = T),
      p_max = max(ppt, na.rm = T)
    )

  return(df_chelsa_cfp)
}
