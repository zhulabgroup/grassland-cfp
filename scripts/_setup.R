# load packages
if (!require("pacman")) {
  install.packages("pacman")
}
if (!require("VoCC")) {
  devtools::install_github("JorGarMol/VoCC", dependencies = TRUE, build_vignettes = FALSE)
}
pacman::p_load(tidyverse, sf, patchwork, rgdal)

# set parameters
knitr::opts_chunk$set(
  echo = FALSE,
  message = FALSE,
  eval = TRUE,
  warning = FALSE
)

# link paths
if (!dir.exists("data")) { # create symlink; final release needs to copy relevant files
  system("ln -s ~/.data/grassland data")
}

# data path tags are
# - `cli`: climate data
# - `com`: grassland community data
# - `geo`: geography data, including GIS and climate
# - `occ`: occurrence data
# - `sum`: summary data, e.g., niche estimates
.path <- list( # hidden variable won't be removed
  cli_all_gbif = "data/climate/all/climate-gbif-2023-02-08.rds",
  cli_bioclim_gbif = "data/climate/all/bioclim-gbif-2022-11-09.rds",
  cli_chelsa = "data/climate/chelsa/2022-11-09/",
  cli_chelsa_annual = "data/climate/chelsa/chelsa-annual-2022-11-10.rds",
  cli_chelsa_monthly = "data/climate/chelsa/monthly/",
  cli_chelsa_cfp_annual = "data/climate/chelsa/cfp_annual/",
  cli_prism = "data/climate/prism/2022-10-03/",
  cli_terraclimate = "data/climate/terraclimate/2023-02-08/",
  com_exp = "data/community/all-experimental-data.rds",
  com_obs = "data/community/all-observational-data.rds",
  com_raw = "data/community/raw/",
  com_spp = "data/community/species/",
  com_tidy = "data/community/tidy/",
  geo_cfp = "data/occurrence/hotspots/hotspots_2016_1.shp",
  geo_clim = "data/climate/climate-gbif-2022-09-18.rds",
  geo_grass = "data/occurrence/grassland/",
  geo_site = "data/community/site-info.rds",
  occ_bien = "data/occurrence/bien/bien-cfp-2022-11-08.rds",
  occ_cch = "data/occurrence/cch/cch-cfp-2022-11-08.rds",
  occ_gbif = "data/occurrence/gbif/gbif-consolidated-2022-11-01.rds",
  occ_inat = "data/occurrence/inat/inat-cfp-2022-11-01.rds",
  sum_niche = "data/occurrence/niche-estimates-cfp-2022-11-01.rds",
  sum_niche_fig = "data/output/master/species-climate-niche-2022-11-01.pdf",
  sum_thin = "data/occurrence/niche-estimates-cfp-2022-11-01-thin.rds"
)
