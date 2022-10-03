# load all climate raster files

# chelsa
chelsa_ras <- raster::stack(
  str_c(.path$cli_chelsa, "bio1.tif"),
  str_c(.path$cli_chelsa, "bio12.tif"),
  str_c(.path$cli_chelsa, "vpd_max.tif")
)
names(chelsa_ras) <- c("tmp", "ppt", "vpd")
raster::proj4string(chelsa_ras) # WGS84

# prism
prism::prism_set_dl_dir(.path$cli_prism)
prism_ras <- raster::stack(
  prism::prism_archive_subset("tmean", "annual normals", resolution = "800m") %>%
    prism::pd_to_file() %>%
    raster::raster(),
  prism::prism_archive_subset("ppt", "annual normals", resolution = "800m") %>%
    prism::pd_to_file() %>%
    raster::raster(),
  prism::prism_archive_subset("vpdmax", "annual normals", resolution = "800m") %>%
    prism::pd_to_file() %>%
    raster::raster()
)
names(prism_ras) <- c("tmp", "ppt", "vpd")
raster::proj4string(prism_ras) # NAD83

# terraclimate
terraclim_ras <- raster::stack(
  str_c(.path$cli_terraclimate, "tmp.tif"),
  str_c(.path$cli_terraclimate, "ppt.tif"),
  str_c(.path$cli_terraclimate, "vpd.tif")
)
names(terraclim_ras) <- c("tmp", "ppt", "vpd")
raster::proj4string(terraclim_ras) # WGS84
