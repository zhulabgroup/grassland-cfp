# load packages
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(bookdown, flextable, GGally, ggpubr, ggtext, ggthemes, googledrive, markdown, raster, rmarkdown, rnaturalearth, tidyverse, viridis, sf, patchwork)
pacman::p_load_gh("ropenscilabs/rnaturalearthhires")
pacman::p_unload("all")
pacman::p_load(tidyverse, sf, patchwork)

# set parameters
theme_set(ggthemes::theme_few())
knitr::opts_chunk$set(
  echo = FALSE,
  message = FALSE,
  eval = TRUE,
  warning = FALSE
)

# link paths
if (!dir.exists("data")) { # create symlink; final release needs to copy relevant files
  system("ln -s ~/.turbo/grassland data")
}

# data path tags are
# - `cli`: climate data
# - `com`: grassland community data
# - `geo`: geography data, including GIS and climate
# - `occ`: occurrence data
# - `out`: output folder
# - `sum`: summary data, e.g., niche estimates
.path <- list( # hidden variable won't be removed
  cli_chelsa = "data/climate/chelsa/2022-11-09/",
  cli_chelsa_annual = "data/climate/chelsa/chelsa-annual-2022-11-10.rds",
  cli_prism = "data/climate/prism/2022-10-03/",
  cli_terraclimate = "data/climate/terraclimate/2022-10-03/",
  com_exp = "data/community/all-experimental-data.rds",
  com_obs = "data/community/all-observational-data.rds",
  com_raw = "data/community/raw/",
  com_spp = "data/community/species/",
  com_tidy = "data/community/tidy/",
  geo_bioclim = "data/climate/all/bioclim-gbif-2022-11-09.rds",
  geo_cfp = "data/occurrence/hotspots/hotspots_2016_1.shp",
  geo_clim = "data/climate/all/climate-gbif-2022-11-01.rds",
  geo_grass = "data/occurrence/grassland/",
  geo_site = "data/community/site-info.rds",
  occ_bien = "data/occurrence/bien/bien-cfp-2022-11-08.rds",
  occ_cch = "data/occurrence/cch/cch-cfp-2022-11-08.rds",
  occ_gbif = "data/occurrence/gbif/gbif-consolidated-2022-11-01.rds",
  occ_inat = "data/occurrence/inat/inat-cfp-2022-11-01.rds",
  out_fig = "data/output/manuscript/",
  out_fig_niche = "data/output/manuscript/species-climate-niche.pdf",
  sum_niche = "data/occurrence/niche-estimates-cfp-2022-11-01.rds",
  sum_niche_fig = "figures/species-climate-niche-2022-11-01.pdf",
  sum_thin = "data/occurrence/niche-estimates-cfp-2022-11-01-thin.rds"
)

# save figures?
.fig_save <- FALSE
