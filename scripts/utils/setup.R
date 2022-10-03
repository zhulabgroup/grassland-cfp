# load packages
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(tidyverse, sf, patchwork)

# set parameters
knitr::opts_chunk$set(
  echo = FALSE,
  message = FALSE,
  eval = TRUE,
  warning = FALSE
)

# link paths
if (!dir.exists("data")) { # create symlink; final release needs to copy relevant files
  system("ln -s /data/ZHULAB/grassland data")
}

# data path tags are
# - `com`: grassland community data
# - `geo`: geography data, including GIS and climate
# - `occ`: occurrence data
# - `sum`: summary data, e.g., niche estimates
.path <- list( # hidden variable won't be removed
  com_exp = "data/community/all-experimental-data.rds",
  com_obs = "data/community/all-observational-data.rds",
  com_raw = "data/community/raw/",
  com_spp = "data/community/species/",
  com_tidy = "data/community/tidy/",
  geo_cfp = "data/occurrence/hotspots/hotspots_2016_1.shp",
  geo_clim = "data/climate/climate-gbif-2022-09-18.rds",
  geo_grass = "data/occurrence/grassland/",
  geo_site = "data/community/site-info.rds",
  occ_bien = "data/occurrence/bien/bien-cfp-2022-04-27.rds",
  occ_cch = "data/occurrence/cch/cch-cfp-2022-07-12.rds",
  occ_gbif = "data/occurrence/gbif/gbif-consolidated-2022-10-03.rds",
  occ_inat = "data/occurrence/inat/inat-cfp-2022-10-03.rds",
  sum_niche = "data/occurrence/niche-estimates-cfp-2022-09-18.rds",
  sum_thin = "data/occurrence/niche-estimates-cfp-2022-09-18-thin.rds"
)
