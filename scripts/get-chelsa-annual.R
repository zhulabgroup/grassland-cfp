# download CHELSA monthly data
# Get download links from https://envicloud.wsl.ch/#/

param_list <- c( # "tas",
  "pr", "vpd"
)
for (param in param_list) {
  dir.create(paste0("/data/ZHULAB/grassland/climate/chelsa_monthly/", param))
  system(paste0("wget --no-host-directories --no-directories --input-file=/data/ZHULAB/grassland/climate/chelsa_monthly/", param, "_paths.txt --continue --directory-prefix=/data/ZHULAB/grassland/climate/chelsa_monthly/", param, "/"))
}
# can do this in parallel:
#   cat [xxx.txt] | xargs -n 1 -P 40 wget --no-host-directories --no-directories --continue --directory-prefix=[xxx]

sites <- read_rds("data/community/site-info.rds")

# Annual summaries
cl <- makeCluster(length(1980:2019))
for (param in param_list) {
  val_list <- foreach(
    year = 1980:2019,
    .packages("tidyverse", "raster", "matrixStats")
  ) %dopar% {
    files <- list.files(paste0("/data/ZHULAB/grassland/climate/chelsa_monthly/", param), pattern = year %>% as.character(), full.names = T)
    sta <- raster::stack(files)
    monthly_dat <- raster::extract(sta, sites)

    if (param == "tas") {
      monthly_dat_tr <- monthly_dat / 10 - 273
      val_year <- data.frame(id = 1:nrow(sites), year = year, value = monthly_dat_tr %>% rowMeans())
    }
    if (param == "pr") {
      monthly_dat_tr <- monthly_dat / 10
      val_year <- data.frame(id = 1:nrow(sites), year = year, value = monthly_dat_tr %>% rowSums())
    }
    if (param == "vpd") {
      monthly_dat_tr <- monthly_dat / 10
      val_year <- data.frame(id = 1:nrow(sites), year = year, value = monthly_dat_tr %>% rowMaxs())
    }
    val_year
  }
  val_df <- bind_rows(val_list) %>%
    left_join(sites %>% mutate(id = row_number()), by = "id")

  write_rds(val_df, file = paste0("./data/climate/chelsa_monthly/", param, "_annual.rds"))
}
stopcluster(cl)

ggplot(val_df %>% filter(id == 1)) +
  geom_line(aes(x = year, y = value))
