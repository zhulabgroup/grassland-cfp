# load all climate raster files

# chelsa
chelsa_ras <- terra::rast(c(
  str_c(.path$cli_chelsa, "bio1.tif"),
  str_c(.path$cli_chelsa, "bio12.tif"),
  str_c(.path$cli_chelsa, "vpd_max.tif")
))
names(chelsa_ras) <- c("tmp", "ppt", "vpd")
terra::crs(chelsa_ras, proj = TRUE) # WGS84

# prism
prism::prism_set_dl_dir(.path$cli_prism)
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
terra::crs(prism_ras, proj = TRUE) # NAD83

# terraclimate
terraclim_ras <- terra::rast(c(
  str_c(.path$cli_terraclimate, "tmp.tif"),
  str_c(.path$cli_terraclimate, "ppt.tif"),
  str_c(.path$cli_terraclimate, "vpd.tif"),
  str_c(.path$cli_terraclimate, "def.tif")
), )
names(terraclim_ras) <- c("tmp", "ppt", "vpd", "cwd")
terra::crs(terraclim_ras, proj = TRUE) # WGS84
