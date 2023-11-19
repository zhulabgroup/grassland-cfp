# docu

#' @export
read_cfp <- function(path_cfp = NULL, indir = "alldata/input/basemap/cfp/") {
  if (is.null(path_cfp)) {
    path_cfp <- list.files(indir, pattern = ".shp$", full.names = T)
  }
  sf_cfp <- st_read(path_cfp, quiet = TRUE) %>%
    filter(
      NAME == "California Floristic Province",
      Type == "hotspot area"
    ) %>%
    select(-NAME, -Type)

  return(sf_cfp)
}

#' @export
read_grasscover <- function(path_grass = NULL, indir = "alldata/input/basemap/grass/") {
  if (is.null(path_grass)) {
    path_grass <- list.files(indir, pattern = ".tif", full.names = T)
  }
  ras_grass <- terra::rast(path_grass)
  return(ras_grass)
}
