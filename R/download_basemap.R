# basemap -----------------------------------------------------------------

download_cfp <- function(cfp_sf, outdir = "input/basemap/cfp/") {
  # Biodiversity Hotspots (version 2016.1)
  # Michael Hoffman;  Kellee Koenig; Gill Bunting;  Jennifer Costanza;  Williams, Kristen J.
  # https://doi.org/10.5281/zenodo.3261807
  download.file(
    url = "https://zenodo.org/record/3261807/files/hotspots_2016_1.zip?download=1",
    destfile = str_c(dirname(outdir), "/cfp.zip")
  )
  unzip(
    zipfile = str_c(dirname(outdir), "/cfp.zip"),
    exdir = str_c(outdir)
  )

  outfile <- list.files(outdir, pattern = ".shp$", full.names = T)
  return(outfile)
}

read_cfp <- function(cfp_file = NULL, indir = "input/basemap/cfp/") {
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

download_grasscover <- function(outdir = "input/basemap/grass/") {
  # MCD12Q1 MODIS/Terra+Aqua Land Cover Type Yearly L3 Global 500m SIN Grid V006
  # Friedl, M., Sulla-Menashe, D.
  # https://doi.org/10.5067/MODIS/MCD12Q1.006
  download.file(
    url = "https://e4ftl01.cr.usgs.gov//MODV6_Cmp_C/MOTA/MCD12C1.006/2020.01.01/MCD12C1.A2020001.006.2021362215328.hdf",
    destfile = str_c(outdir, "MCD12C1.A2020001.006.2021362215328.hdf"),
    method = "wget",
    extra = str_c(
      "-c",
      " --user=", readline(prompt = "Earthdata login user name: "),
      " --password=", readline(prompt = "Earthdata login password: ")
    )
  )

  # get grass percentage cover
  grass_ras <- terra::rast(str_c(outdir, "MCD12C1.A2020001.006.2021362215328.hdf")) %>%
    terra::crop(terra::ext(cfp_sf)) %>%
    terra::mask(cfp_sf) %>%
    terra::subset("Land_Cover_Type_1_Percent_10") # User guide at https://lpdaac.usgs.gov/documents/101/MCD12_User_Guide_V6.pdf

  outfile <- str_c(outdir, "cfp-grassland-percent-cover.tif")
  terra::writeRaster(grass_ras, outfile, overwrite = T)

  return(outfile)
}

read_grasscover <- function(grass_file = NULL, indir = "input/basemap/grass/") {
  if (is.null(grass_file)) {
    grass_file <- list.files(indir, pattern = ".tif", full.names = T)
  }
  grass_ras <- terra::rast(grass_file)
  return(grass_ras)
}
