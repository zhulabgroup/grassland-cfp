# basemap -----------------------------------------------------------------

download_cfp <- function() {
  # Biodiversity Hotspots (version 2016.1)
  # Michael Hoffman;  Kellee Koenig; Gill Bunting;  Jennifer Costanza;  Williams, Kristen J.
  # https://doi.org/10.5281/zenodo.3261807
  download.file(
    url = "https://zenodo.org/record/3261807/files/hotspots_2016_1.zip?download=1",
    destfile = "input/basemap/cfp.zip"
  )
  unzip(
    zipfile = "input/basemap/cfp.zip",
    exdir = "input/basemap/cfp"
  )
}

download_grasscover <- function() {
  # MCD12Q1 MODIS/Terra+Aqua Land Cover Type Yearly L3 Global 500m SIN Grid V006
  # Friedl, M., Sulla-Menashe, D.
  # https://doi.org/10.5067/MODIS/MCD12Q1.006
  download.file(
    url = "https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOTA/MCD12C1.006/2020.01.01/MCD12C1.A2020001.006.2021362215328.hdf",
    destfile = "input/basemap/grass/MCD12C1.A2020001.006.2021362215328.hdf",
    method = "wget",
    extra = str_c(
      "-c",
      " --user=", readline(prompt = "Earthdata login user name: "),
      " --password=", readline(prompt = "Earthdata login password: ")
    )
  )

  # read cfp data
  cfp_sf <- st_read("input/basemap/cfp/hotspots_2016_1.shp", quiet = TRUE) %>%
    filter(
      NAME == "California Floristic Province",
      Type == "hotspot area"
    )
  # get grass percentage cover
  grass_ras <- terra::rast("input/basemap/grass/MCD12C1.A2020001.006.2021362215328.hdf") %>%
    terra::crop(terra::ext(cfp_sf)) %>%
    terra::mask(cfp_sf) %>%
    terra::subset("Land_Cover_Type_1_Percent_10") # User guide at https://lpdaac.usgs.gov/documents/101/MCD12_User_Guide_V6.pdf
  terra::writeRaster(grass_ras, "input/basemap/grass/cfp-grassland-percent-cover.tif", overwrite = T)
}

# biogeography ------------------------------------------------------------

download_gbif <- function() {

}

download_bien <- function() {

}

download_cch <- function() {

}

download_inat <- function() {

}

# climate -----------------------------------------------------------------

download_chelsa <- function() {

}

download_prism <- function() {

}

download_terraclim <- function() {

}
