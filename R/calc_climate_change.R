# need to separate into two functions calculating annual metrics andchange respectively
calc_climate_change <- function(indir = "alldata/input/climate/monthly/",
                                v_param = c("tas", "pr", "vpd"),
                                outdir = "alldata/intermediate/background/cfp_annual") {
  # read cfp data
  sf_cfp <- read_cfp(path_cfp = system.file("extdata", "cfp", package = "grassland"))

  # annual metric
  for (param in v_param) {
    annual_list <- vector(mode = "list")
    for (year in 1980:2019) {
      files <- list.files(str_c(indir, param, "/raw/"), pattern = year %>% as.character(), full.names = T)
      sta <- raster::stack(files)
      sta <- raster::crop(sta, raster::extent(cfp_sf))
      sta <- raster::mask(sta, cfp_sf)

      if (param == "tas") {
        sta <- sta / 10 - 273.15
        annual <- raster::calc(sta, fun = mean)
      }
      if (param == "pr") {
        sta <- sta / 100
        annual <- raster::calc(sta, fun = sum)
      }
      if (param == "vpd") {
        annual <- raster::calc(sta, fun = max)
      }
      annual_list[[year %>% as.character()]] <- annual
      print(year)
    }
    annual_allyears <- raster::stack(annual_list)
    raster::writeRaster(annual_allyears, str_c(outdir, param, ".nc"), format = "CDF")

    pacman::p_load(raster)
    trend <- VoCC::tempTrend(annual_allyears, th = 10)
    pacman::p_unload(raster)
    raster::writeRaster(trend, str_c(outdir, param, "_trend.nc"), format = "CDF")
  }

  return(outdir)
}
