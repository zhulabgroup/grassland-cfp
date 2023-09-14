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

## ----setup--------------------------------------------------------------------
library(grassland)
theme_set(ggthemes::theme_few())

## -----------------------------------------------------------------------------
gg_community_index_exp_jrgce_water <- plot_community_index(option = "exp", dat_index, experiment = "jrgce", treatment = "watering")
gg_community_index_exp_jrgce_water

## -----------------------------------------------------------------------------
gg_community_index_exp_mclexp <- plot_community_index(option = "exp", dat_index, experiment = "mclexp")
gg_community_index_exp_mclexp

## -----------------------------------------------------------------------------
gg_community_index_exp_scide <- plot_community_index(option = "exp", dat_index, experiment = "scide")
gg_community_index_exp_scide

## ---- eval = F----------------------------------------------------------------
#  save_figure(out = list(
#    community_index_exp_jrgce_water = gg_community_index_exp_jrgce_water,
#    community_index_exp_mclexp = gg_community_index_exp_mclexp,
#    community_index_exp_scide = gg_community_index_exp_scide
#  ))

