#' @export
down_cfp <- function(sf_cfp, outdir = "alldata/input/basemap/cfp/") {
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

  # outfile <- list.files(outdir, pattern = ".shp$", full.names = T)
  return(outdir)
}

#' @export
down_grasscover <- function(sf_cfp, outdir = "alldata/input/basemap/grass/") {
  # MCD12Q1 MODIS/Terra+Aqua Land Cover Type Yearly L3 Global 500m SIN Grid V006
  # Friedl, M., Sulla-Menashe, D.
  # https://doi.org/10.5067/MODIS/MCD12Q1.006
  download.file(
    url = "https://e4ftl01.cr.usgs.gov/MOTA/MCD12C1.061/2021.01.01/MCD12C1.A2021001.061.2022217040006.hdf",
    destfile = str_c(outdir, "MCD12C1.A2021001.061.2022217040006.hdf"),
    method = "wget",
    extra = str_c(
      "-c",
      " --user=", readline(prompt = "Earthdata login user name: "),
      " --password=", readline(prompt = "Earthdata login password: ")
    )
  )

  # get grass percentage cover
  ras_grass <- terra::rast(str_c(outdir, "MCD12C1.A2021001.061.2022217040006.hdf")) %>%
    terra::crop(terra::ext(sf_cfp)) %>%
    terra::mask(sf_cfp) %>%
    terra::subset("Land_Cover_Type_1_Percent_10") # User guide at https://lpdaac.usgs.gov/documents/101/MCD12_User_Guide_V6.pdf

  outfile <- str_c(outdir, "cfp-grassland-percent-cover.tif")
  terra::writeRaster(ras_grass, outfile, overwrite = T)

  return(outfile)
}
