#' @export
# get observational sites geographical and climatic space
summ_site_obs <- function(sf_site, dat_clim, type) {
  if (type == "latlon") {
    out <- sf_site %>%
      extract(geometry, c("lon", "lat"), "\\((.*), (.*)\\)", convert = TRUE) %>%
      select(site, lat, lon) %>%
      summarise(
        lat_max = max(lat, na.rm = T),
        lat_min = min(lat, na.rm = T),
        lon_max = max(lon, na.rm = T),
        lon_min = min(lon, na.rm = T)
      )
  }

  if (type == "climate") {
    out <- terra::extract(dat_clim, sf_site) %>%
      summarise(
        tmp_min = min(tmp, na.rm = T),
        tmp_max = max(tmp, na.rm = T),
        ppt_min = min(ppt, na.rm = T),
        ppt_max = max(ppt, na.rm = T)
      )
  }

  if (type == "hull") {
    out <- sf_site %>%
      extract(geometry, c("lon", "lat"), "\\((.*), (.*)\\)", convert = TRUE) %>%
      select(site, lat, lon) %>%
      terra::vect(crs = terra::crs(read_site_info(), proj = T)) %>%
      terra::convHull() %>%
      terra::expanse(unit = "km")
  }
  return(out)
}
