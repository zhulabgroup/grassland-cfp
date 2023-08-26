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
#
# calc_climate_change_site <- function(path, v_param= c("tas", "pr", "vpd"), outdir = "alldata/intermediate/background/") {
#
#   if (is.null(path)) {
#     indir = "alldata/input/climate/monthly/"
#   } else {
#     indir = path
#   }
#
#   ## climate change maps at observational sites CFP
#   # sites <- read_rds(.path$geo_site)
#
#   # Extract monthly data at sites by year and get annual summaries
#   cl <- makeCluster(length(1980:2019))
#   registerDoSNOW(cl)
#
#   val_list_allparam <- vector(mode = "list")
#   for (param in v_param) {
#     val_list <- foreach(
#       year = 1980:2019,
#       .packages = c("tidyverse", "sf", "raster", "matrixStats")
#     ) %dopar% {
#       files <- list.files(str_c(indir, param), pattern = year %>% as.character(), full.names = T)
#       sta <- raster::stack(files)
#       monthly_dat <- raster::extract(sta, sites)
#
#       if (param == "tas") {
#         monthly_dat_tr <- monthly_dat / 10 - 273.15
#         val_year <- data.frame(id = 1:nrow(sites), year = year, value = monthly_dat_tr %>% rowMeans())
#       }
#       if (param == "pr") {
#         monthly_dat_tr <- monthly_dat / 100
#         val_year <- data.frame(id = 1:nrow(sites), year = year, value = monthly_dat_tr %>% rowSums())
#       }
#       if (param == "vpd") {
#         monthly_dat_tr <- monthly_dat
#         val_year <- data.frame(id = 1:nrow(sites), year = year, value = monthly_dat_tr %>% rowMaxs())
#       }
#       val_year
#     }
#     val_list_allparam[[param]] <- bind_rows(val_list) %>%
#       left_join(sites %>% mutate(id = row_number()), by = "id") %>%
#       mutate(param = param)
#   }
#   val_df <- bind_rows(val_list_allparam) %>%
#     as_tibble() %>%
#     select(abbr, name, year, param, value) %>%
#     pivot_wider(names_from = param, values_from = value) %>%
#     rename(tmp = tas, ppt = pr) %>%
#     arrange(abbr)
#
#   outfile <- str_c(outdir, "site-annual.rds")
#   write_rds(val_df, outfile)
#
#   stopcluster(cl)
#
#   return(outfile)
# }
