#' @export
read_site_info <- function(path = NULL, subset = NULL) {
  if (is.null(path)) {
    # path <- "alldata/input/basemap/site_info.csv"
    path <- system.file("extdata", "site_info.csv", package = "grassland")
  }

  # site info data
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

#' @export
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

#' @export
read_jrgce_env <- function(path = "alldata/input/climate/Environment.csv") {
  env_df <- read_csv(path) %>%
    filter(yr != 1998)
  return(env_df)
}
