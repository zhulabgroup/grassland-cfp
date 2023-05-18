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

}

download_gbif <- function() {

}

download_bien <- function() {

}

download_cch <- function() {

}

download_inat <- function() {

}
