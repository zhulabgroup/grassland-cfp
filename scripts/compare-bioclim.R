# compare CHELSA bioclim variables (https://www.worldclim.org/data/bioclim.html)
bioclim_sf <- read_rds(.path$cli_bioclim_gbif)

# temperature variables
bioclim_sf %>%
  st_drop_geometry() %>%
  select(str_c("bio", c(1, 5, 6, 8:11))) %>%
  sample_n(1e3) %>%
  PerformanceAnalytics::chart.Correlation(histogram = TRUE, pch = 19)

# precipitation variables
bioclim_sf %>%
  st_drop_geometry() %>%
  select(str_c("bio", c(12:14, 16:19))) %>%
  sample_n(1e3) %>%
  PerformanceAnalytics::chart.Correlation(histogram = TRUE, pch = 19)

# vpd variables
bioclim_sf %>%
  st_drop_geometry() %>%
  select(str_c("vpd_", c("max", "mean", "min"))) %>%
  sample_n(1e3) %>%
  PerformanceAnalytics::chart.Correlation(histogram = TRUE, pch = 19)
