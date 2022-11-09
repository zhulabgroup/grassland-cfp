# compare CHELSA bioclim variables (https://www.worldclim.org/data/bioclim.html)
bioclim_tbl <- read_rds(.path$geo_bioclim) %>%
  st_drop_geometry()

tmp_cor <- bioclim_tbl %>%
  select(
    bio1, # Annual Mean Temperature
    bio8, # Mean Temperature of Wettest Quarter
    bio9, # Mean Temperature of Driest Quarter
    bio10, # Mean Temperature of Warmest Quarter
    bio11 # Mean Temperature of Coldest Quarter
  ) %>%
  cor()

ppt_cor <- bioclim_tbl %>%
  select(
    bio12, # Annual Precipitation
    bio13, # Precipitation of Wettest Month
    bio16, # Precipitation of Wettest Quarter
    bio19 # Precipitation of Coldest Quarter
  ) %>%
  cor()
