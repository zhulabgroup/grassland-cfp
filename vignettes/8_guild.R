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
dat_guild_niche <- calc_guild_niche(dat_community, dat_niche)
gg_guild_niche <- plot_guild_niche(dat_guild_niche = dat_guild_niche)
gg_guild_niche
res_guild_niche <- test_guild_niche(dat_guild_niche = dat_guild_niche)
res_guild_niche

## -----------------------------------------------------------------------------
gg_guild_percentage_obs <- plot_guild_percentage(option = "obs", dat_community = dat_community)
gg_guild_percentage_obs
gg_guild_percentage_exp <- plot_guild_percentage(option = "exp", dat_community = dat_community)
gg_guild_percentage_exp

## ---- eval = F----------------------------------------------------------------
#  save_figure(out = list(
#    guild_niche = gg_guild_niche,
#    guild_perc_obs = gg_guild_percentage_obs,
#    guild_perc_exp = gg_guild_percentage_exp
#  ))

