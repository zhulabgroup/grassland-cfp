# manually download TerraClimate NC files
# system ("ln -s /nfs/turbo/seas-zhukai/climate/TerraClimate/climatologies ./data/climate/terraclimate/2023-02-08/raw")

# assemble into rasters
param_ls <- c("tmp", "ppt", "vpd", "def")

for (param in param_ls) {
  if (param == "tmp") { # average of max and min temp
    tmax_stk <- terra::rast(
      str_c(
        .path$cli_terraclimate,
        "raw/TerraClimate19812010_tmax.nc"
      )
    )
    tmin_stk <- terra::rast(
      str_c(
        .path$cli_terraclimate,
        "raw/TerraClimate19812010_tmin.nc"
      )
    )
    tmp_ras_ls <- vector(mode = "list", length = 12)
    for (m in 1:12) {
      tmp_ras_ls[[m]] <- mean(tmax_stk[[m]], tmin_stk[[m]])
    }
    tmp_ras <- tmp_ras_ls %>%
      terra::rast() %>%
      mean()
    terra::writeRaster(
      tmp_ras,
      str_c(
        .path$cli_terraclimate, "tmp.tif"
      ),
      overwrite = TRUE
    )
  }

  if (param == "ppt") { # sum of monthly ppt
    ppt_stk <- terra::rast(
      str_c(
        .path$cli_terraclimate,
        "raw/TerraClimate19812010_ppt.nc"
      )
    )
    ppt_ras <- sum(ppt_stk)
    terra::writeRaster(
      ppt_ras,
      str_c(
        .path$cli_terraclimate, "ppt.tif"
      ),
      overwrite = TRUE
    )
  }

  if (param == "vpd") { # max of monthly vpd?
    vpd_stk <- terra::rast(
      str_c(
        .path$cli_terraclimate,
        "raw/TerraClimate19812010_vpd.nc"
      )
    )
    vpd_ras <- max(vpd_stk)
    writeRaster(
      vpd_ras,
      str_c(
        .path$cli_terraclimate, "vpd.tif"
      ),
      overwrite = TRUE
    )
  }

  if (param == "def") { # mean of monthly climate water deficit?
    def_stk <- terra::rast(
      str_c(
        .path$cli_terraclimate,
        "raw/TerraClimate19812010_def.nc"
      )
    )
    def_ras <- mean(def_stk)
    writeRaster(
      def_ras,
      str_c(
        .path$cli_terraclimate, "def.tif"
      ),
      overwrite = TRUE
    )
  }
}
