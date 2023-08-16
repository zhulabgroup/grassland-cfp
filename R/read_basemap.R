
read_cfp <- function(cfp_file = NULL, indir = "alldata/input/basemap/cfp/") {
  if (is.null(cfp_file)) {
    cfp_file <- list.files(indir, pattern = ".shp$", full.names = T)
  }
  cfp_sf <- st_read(cfp_file, quiet = TRUE) %>%
    filter(
      NAME == "California Floristic Province",
      Type == "hotspot area"
    )

  return(cfp_sf)
}



read_grasscover <- function(grass_file = NULL, indir = "alldata/input/basemap/grass/") {
  if (is.null(grass_file)) {
    grass_file <- list.files(indir, pattern = ".tif", full.names = T)
  }
  grass_ras <- terra::rast(grass_file)
  return(grass_ras)
}
