# manually download TerraClimate NC files
# assemble into rasters
library(raster)
param_ls <- c("tmp", "ppt", "vpd")

for (param in param_ls) {
  if (param == "tmp") { # average of max and min temp
    tmax_stk <- stack(
      str_c(
        .path$cli_terraclimate,
        "nc/TerraClimate19812010_tmax.nc"
      )
    )
    tmin_stk <- stack(
      str_c(
        .path$cli_terraclimate,
        "nc/TerraClimate19812010_tmin.nc"
      )
    )
    tmp_ras_ls <- vector(mode = "list", length = 12)
    for (m in 1:12) {
      tmp_ras_ls[[m]] <- mean(tmax_stk[[m]], tmin_stk[[m]])
    }
    tmp_ras <- tmp_ras_ls %>%
      stack() %>%
      mean()
    writeRaster(
      tmp_ras,
      str_c(
        .path$cli_terraclimate, "tmp.tif"
      ),
      format = "GTiff", overwrite = TRUE
    )
  }

  if (param == "ppt") { # sum of monthly ppt
    ppt_stk <- stack(
      str_c(
        .path$cli_terraclimate,
        "nc/TerraClimate19812010_ppt.nc"
      )
    )
    ppt_ras <- sum(ppt_stk)
    writeRaster(
      ppt_ras,
      str_c(
        .path$cli_terraclimate, "ppt.tif"
      ),
      format = "GTiff", overwrite = TRUE
    )
  }

  if (param == "vpd") { # max of monthly vpd?
    vpd_stk <- stack(
      str_c(
        .path$cli_terraclimate,
        "nc/TerraClimate19812010_vpd.nc"
      )
    )
    vpd_ras <- max(vpd_stk)
    writeRaster(
      vpd_ras,
      str_c(
        .path$cli_terraclimate, "vpd.tif"
      ),
      format = "GTiff", overwrite = TRUE
    )
  }
}

detach("package:raster", unload = TRUE) # unload raster to avoid conflict with tidyverse
