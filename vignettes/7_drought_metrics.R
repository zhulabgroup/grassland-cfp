## ---- include = FALSE---------------------------------------------------------
knitr::opts_knit$set(root.dir = here::here()) # knit from project directory
knitr::opts_chunk$set(
  collapse = TRUE,
  out.width = "100%",
  cache = T,
  comment = "#>",
  fig.width = 8,
  fig.height = 8
)

## ----setup, cache = F---------------------------------------------------------
library(grassland)
theme_set(ggthemes::theme_few())

## -----------------------------------------------------------------------------
dat_trait_cwd <- read_individual_trait(path_trait = "alldata/intermediate/climate-niche/gbif-terraclim.rds")

## -----------------------------------------------------------------------------
gg_trait_cwd <- plot_trait_cwd(dat_trait_cwd)
gg_trait_cwd
test_trait_cwd_cor(dat_trait_cwd)

## -----------------------------------------------------------------------------
dat_niche_cwd <- calc_species_niche(dat_trait_cwd, add_dummy = T)
dat_index_cwd <- calc_community_index(dat_niche = dat_niche_cwd, dat_community = dat_community)

## -----------------------------------------------------------------------------
gg_community_index_obs_cwd <- plot_community_index(option = "obs", dat_index = dat_index_cwd)
gg_community_index_obs_cwd
gg_community_index_exp_cwd <- plot_community_index(option = "exp", dat_index = dat_index_cwd, experiment = "jrgce", treatment = "warming")
gg_community_index_exp_cwd

## ---- eval = F----------------------------------------------------------------
#  save_figure(out = list(
#    trait_cwd = gg_trait_cwd,
#    community_index_obs_cwd = gg_community_index_obs_cwd,
#    community_index_exp_cwd = gg_community_index_exp_cwd
#  ))

