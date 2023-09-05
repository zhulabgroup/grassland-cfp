read_climate <- function(path_clim = NULL, indir = "alldata/input/climate/") {
  if (is.null(path_clim)) {
    path_clim <- list(
      chelsa = str_c(indir, "chelsa/"),
      prism = str_c(indir, "prism/"),
      terraclim = str_c(indir, "terraclim/")
    )
  }
  # chelsa
  chelsa_ras <- terra::rast(c(
    str_c(path_clim$chelsa, "bio1.tif"),
    str_c(path_clim$chelsa, "bio12.tif"),
    str_c(path_clim$chelsa, "vpd_max.tif"),
    str_c(path_clim$chelsa, "bio1.tif"),
    str_c(path_clim$chelsa, "bio8.tif"),
    str_c(path_clim$chelsa, "bio9.tif"),
    str_c(path_clim$chelsa, "bio10.tif"),
    str_c(path_clim$chelsa, "bio11.tif"),
    str_c(path_clim$chelsa, "bio12.tif"),
    str_c(path_clim$chelsa, "bio13.tif"),
    str_c(path_clim$chelsa, "bio16.tif"),
    str_c(path_clim$chelsa, "bio19.tif")
  ))
  names(chelsa_ras) <- c(
    "tmp", "ppt", "vpd",
    "bio1", # Annual Mean Temperature
    "bio8", # Mean Temperature of Wettest Quarter
    "bio9", # Mean Temperature of Driest Quarter
    "bio10", # Mean Temperature of Warmest Quarter
    "bio11", # Mean Temperature of Coldest Quarter
    "bio12", # Annual Precipitation
    "bio13", # Precipitation of Wettest Month
    "bio16", # Precipitation of Wettest Quarter
    "bio19" # Precipitation of Coldest Quarter
  )
  # terra::crs(chelsa_ras, proj = TRUE) # WGS84

  # prism
  prism::prism_set_dl_dir(path_clim$prism)
  prism_ras <- terra::rast(list(
    prism::prism_archive_subset("tmean", "annual normals", resolution = "800m") %>%
      prism::pd_to_file() %>%
      terra::rast(),
    prism::prism_archive_subset("ppt", "annual normals", resolution = "800m") %>%
      prism::pd_to_file() %>%
      terra::rast(),
    prism::prism_archive_subset("vpdmax", "annual normals", resolution = "800m") %>%
      prism::pd_to_file() %>%
      terra::rast()
  ))
  names(prism_ras) <- c("tmp", "ppt", "vpd")
  # terra::crs(prism_ras, proj = TRUE) # NAD83

  # terraclimate
  terraclim_ras <- terra::rast(c(
    str_c(path_clim$terraclim, "tmp.tif"),
    str_c(path_clim$terraclim, "ppt.tif"),
    str_c(path_clim$terraclim, "vpd.tif"),
    str_c(path_clim$terraclim, "def.tif")
  ))
  names(terraclim_ras) <- c("tmp", "ppt", "vpd", "cwd")
  # terra::crs(terraclim_ras, proj = TRUE) # WGS84

  out <- list(
    chelsa = chelsa_ras,
    prism = prism_ras,
    terraclim = terraclim_ras
  )
  return(out)
}
