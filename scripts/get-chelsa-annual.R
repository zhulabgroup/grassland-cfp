# download CHELSA monthly data
# extract at site locations
# Get download links from https://envicloud.wsl.ch/#/

param_list <- c("tas", "pr", "vpd")
for (param in param_list) {
  dir.create(paste0(.path$cli_chelsa_monthly, param, "/raw/"))
  system(paste0("cat ",.path$cli_chelsa_monthly, param,"/paths.txt | xargs -n 1 -P 40 wget --no-verbose --no-clobber --no-host-directories --no-directories --continue --directory-prefix=",.path$cli_chelsa_monthly, param,"/raw"))
}

# read cfp data
cfp_sf <- st_read(.path$geo_cfp, quiet = TRUE) %>%
  filter(
    NAME == "California Floristic Province",
    Type == "hotspot area"
  )

# annual metric
for (param in param_list) {
  annual_list<-vector(mode="list")
  for (year in 1980:2019)  {
    files <- list.files(paste0(.path$cli_chelsa_monthly, param, "/raw/"), pattern = year %>% as.character(), full.names = T)
    sta <- raster::stack(files)
    sta <- raster::crop(sta, raster::extent(cfp_sf))
    sta <- raster::mask(sta, cfp_sf)
    
    if (param == "tas") {
      sta<-sta/10-273.15
      annual <- raster::calc(sta, fun = mean)
    }
    if (param == "pr") {
      sta<-sta/100
      annual <- raster::calc(sta, fun = sum)
    }
    if (param == "vpd") {
      annual <- raster::calc(sta, fun = max)
    }
    annual_list[[year %>% as.character()]]<-annual
    print(year)
           }
  annual_allyears<-raster::stack(annual_list)
  raster::writeRaster(annual_allyears,paste0(.path$cli_chelsa_cfp_annual, param, ".nc"), format="CDF")
  
  pacman::p_load(raster)
  trend <- VoCC::tempTrend(annual_allyears, th = 10)
  pacman::p_unload(raster)
  raster::writeRaster(trend,paste0(.path$cli_chelsa_cfp_annual, param, "_trend.nc"), format="CDF")
  
}


## climate change maps in CFP
sites <- read_rds(.path$geo_site)

# Extract monthly data at sites by year and get annual summaries
cl <- makeCluster(length(1980:2019))
registerDoSNOW(cl)

val_list_allparam <- vector(mode = "list")
for (param in param_list) {
  val_list <- foreach(
    year = 1980:2019,
    .packages = c("tidyverse", "sf", "raster", "matrixStats")
  ) %dopar% {
    files <- list.files(paste0(.path$cli_chelsa_monthly, param), pattern = year %>% as.character(), full.names = T)
    sta <- raster::stack(files)
    monthly_dat <- raster::extract(sta, sites)

    if (param == "tas") {
      monthly_dat_tr <- monthly_dat / 10 - 273.15
      val_year <- data.frame(id = 1:nrow(sites), year = year, value = monthly_dat_tr %>% rowMeans())
    }
    if (param == "pr") {
      monthly_dat_tr <- monthly_dat / 100
      val_year <- data.frame(id = 1:nrow(sites), year = year, value = monthly_dat_tr %>% rowSums())
    }
    if (param == "vpd") {
      monthly_dat_tr <- monthly_dat
      val_year <- data.frame(id = 1:nrow(sites), year = year, value = monthly_dat_tr %>% rowMaxs())
    }
    val_year
  }
  val_list_allparam[[param]] <- bind_rows(val_list) %>%
    left_join(sites %>% mutate(id = row_number()), by = "id") %>%
    mutate(param = param)
}
val_df <- bind_rows(val_list_allparam)

val_df %>%
  as_tibble() %>%
  select(abbr, name, year, param, value) %>%
  pivot_wider(names_from = param, values_from = value) %>%
  rename(tmp = tas, ppt = pr) %>%
  arrange(abbr) %>%
  write_rds(.path$cli_chelsa_annual)

stopcluster(cl)
