#' @export
down_climate <- function(outdir = "input/climate/") {
  path_chelsa <- down_chelsa(outdir)
  path_prism <- down_prism(outdir)
  path_terraclim <- down_terraclim(outdir)

  out <- list(
    chelsa = path_chelsa,
    prism = path_prism,
    terraclim = path_terraclim
  )
  return(out)
}

down_chelsa <- function(outdir) {
  dir_chelsa <- str_c(outdir, "chelsa/")
  # download CHELSA climatology data, 1981-2010
  # https://chelsa-climate.org/wp-admin/download-page/CHELSA_tech_specification_V2.pdf
  param_list <- c(str_c("bio", 1:19), str_c("vpd_", c("max", "mean", "min", "range")))

  for (param in param_list) {
    url <- str_c(
      "https://envicloud.wsl.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/CHELSA_",
      param,
      "_1981-2010_V.2.1.tif"
    )
    download.file(
      url = url,
      destfile = str_c(dir_chelsa, param, ".tif")
    )
  }

  return(dir_chelsa)
}

down_prism <- function(outdir) {
  dir_prism <- str_c(outdir, "prism/")
  # download PRISM data
  prism::prism_set_dl_dir(dir_prism)
  prism::get_prism_normals("tmean", "800m", annual = TRUE, keepZip = FALSE)
  prism::get_prism_normals("ppt", "800m", annual = TRUE, keepZip = FALSE)
  prism::get_prism_normals("vpdmax", "800m", annual = TRUE, keepZip = FALSE)

  return(dir_prism)
}

down_terraclim <- function(outdir) {
  dir_terraclim <- str_c(outdir, "terraclim/")

  # http://thredds.northwestknowledge.net:8080/thredds/catalog/TERRACLIMATE_ALL/summaries/catalog.html
  param_down_ls <- c("tmax", "tmin", "ppt", "vpd", "def")
  for (param in param_down_ls) {
    url <- str_c(
      "http://thredds.northwestknowledge.net:8080/thredds/fileServer/TERRACLIMATE_ALL/summaries/TerraClimate19812010_", param, ".nc"
    )
    download.file(
      url = url,
      destfile = str_c(dir_terraclim, "TerraClimate19812010_", param, ".nc")
    )
  }

  # assemble into rasters
  param_ls <- c("tmp", "ppt", "vpd", "def")
  for (param in param_ls) {
    if (param == "tmp") { # average of max and min temp
      tmax_stk <- terra::rast(
        list.files(dir_terraclim,
          pattern = "tmax.nc",
          full.names = T
        )
      )
      tmin_stk <- terra::rast(
        list.files(dir_terraclim,
          pattern = "tmin.nc",
          full.names = T
        )
      )
      tmp_ras_ls <- vector(mode = "list", length = 12)
      for (m in 1:12) {
        tmp_ras_ls[[m]] <- terra::mean(tmax_stk[[m]], tmin_stk[[m]])
      }
      tmp_ras <- tmp_ras_ls %>%
        terra::rast() %>%
        mean()
      terra::writeRaster(
        tmp_ras,
        str_c(
          dir_terraclim, "tmp.tif"
        ),
        overwrite = TRUE
      )
    }

    if (param == "ppt") { # sum of monthly ppt
      ppt_stk <- terra::rast(
        list.files(dir_terraclim,
          pattern = "ppt.nc",
          full.names = T
        )
      )
      ppt_ras <- sum(ppt_stk)
      terra::writeRaster(
        ppt_ras,
        str_c(
          dir_terraclim, "ppt.tif"
        ),
        overwrite = TRUE
      )
    }

    if (param == "vpd") { # max of monthly vpd?
      vpd_stk <- terra::rast(
        list.files(dir_terraclim,
          pattern = "vpd.nc",
          full.names = T
        )
      )
      vpd_ras <- max(vpd_stk)
      writeRaster(
        vpd_ras,
        str_c(
          dir_terraclim, "vpd.tif"
        ),
        overwrite = TRUE
      )
    }

    if (param == "def") { # mean of monthly climate water deficit?
      def_stk <- terra::rast(
        list.files(dir_terraclim,
          pattern = "def.nc",
          full.names = T
        )
      )
      def_ras <- mean(def_stk)
      writeRaster(
        def_ras,
        str_c(
          dir_terraclim, "def.tif"
        ),
        overwrite = TRUE
      )
    }
  }
  return(dir_terraclim)
}
