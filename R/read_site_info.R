#' Import Site Information
#'
#' This function imports site information data and optionally subsets it to only observational sites.
#'
#' @param subset A character value that suggests if a subset of the data is needed.
#' If `subset` equals "obs", the function returns information of only observational sites.
#'
#' @return A spatial tibble (sf object) with site information.
#' @examples
#' \dontrun{
#' site_info <- read_site_info()
#' site_info <- read_site_info(subset = "obs")
#' }
#' @export
read_site_info <- function(subset = NULL) {
  path <- system.file("extdata", "site_info.csv", package = "grassland")

  site_sf <- read_csv(path) %>%
    mutate(data_method = str_replace(data_method, "\xd7", "\u00D7")) %>% # print multiple sign in unicode
    mutate(
      latitude = measurements::conv_unit(latitude, from = "deg_min_sec", to = "dec_deg") %>% as.numeric(),
      longitude = measurements::conv_unit(longitude, from = "deg_min_sec", to = "dec_deg") %>% as.numeric()
    ) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

  if (!is.null(subset)) {
    if (subset == "obs") {
      site_sf <- site_sf %>%
        filter(site %in% c("angelo", "carrizo", "elkhorn", "jasper", "mclann", "morganterritory", "pleasantonridge", "sunol", "swanton", "ucsc", "vascocaves"))
    }
  }

  return(site_sf)
}

read_site_name <- function() {
  site_vec <- c(
    angelo = "Angelo Coast",
    carrizo = "Carrizo Plain",
    elkhorn = "Elkhorn Slough",
    jasper = "Jasper Ridge Serpentine",
    mclann = "McLaughlin Annual",
    mclserp = "McLaughlin Serpentine",
    morganterritory = "Morgan Territory",
    pleasantonridge = "Pleasanton Ridge",
    sunol = "Sunol",
    swanton = "Swanton Ranch",
    ucsc = "UC Santa Cruz",
    vascocaves = "Vasco Caves",
    jrgce = "Jasper Ridge\nGlobal Change Experiment",
    mclexp = "McLaughlin Water Experiment",
    scide = "Santa Cruz\nInternational Drought Experiment"
  )
  return(site_vec)
}
