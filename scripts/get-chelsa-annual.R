# download CHELSA monthly data
# Get download links from https://envicloud.wsl.ch/#/
# data files deleted because of the large file sizes

param_list <- c("tas", "pr", "vpd")
for (param in param_list) {
  dir.create(paste0(.path$cli_chelsa_monthly, param))
  system(paste0("wget --no-host-directories --no-directories --input-file=",.path$cli_chelsa_monthly, param, "_paths.txt --continue --directory-prefix=",.path$cli_chelsa_monthly, param, "/"))
}
# can do this in parallel:
#   cat [xxx.txt] | xargs -n 1 -P 40 wget --no-host-directories --no-directories --continue --directory-prefix=[xxx]

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
      monthly_dat_tr <- monthly_dat / 10 - 273
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

write_rds(val_df, file = paste0(.path$cli_chelsa_monthly,"annual.rds"))
stopcluster(cl)

# Plot
ggplot(val_df) +
  geom_line(aes(x = year, y = value, group = name, col = name)) +
  geom_smooth(aes(x = year, y = value, group = name, col = name), method = "lm", se = F) +
  theme_classic() +
  facet_wrap(. ~ param, scales = "free_y", ncol = 3)

