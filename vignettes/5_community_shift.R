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
gg_guild_percentage_obs <- plot_guild_percentage(option = "obs", dat_community = dat_community)
gg_guild_percentage_obs
gg_guild_percentage_exp <- plot_guild_percentage(option = "exp", dat_community = dat_community)
gg_guild_percentage_exp

## -----------------------------------------------------------------------------
df_trait_change_obs <- test_trait_change_all(dat_community, dat_niche, option = "obs")
df_trait_change_obs %>% knitr::kable()
df_trait_change_exp <- test_trait_change_all(dat_community, dat_niche, option = "exp")
df_trait_change_exp %>% knitr::kable()
df_summ_trait_change_obs <- summ_change(df_trait_change_obs, option = "obs")

## -----------------------------------------------------------------------------
gg_trait_change_obs <- plot_trait_change(option = "obs", dat_community, dat_niche)
gg_trait_change_obs
gg_trait_change_exp <- plot_trait_change(option = "exp", dat_community, dat_niche)
gg_trait_change_exp

## ---- eval = F----------------------------------------------------------------
#  dat_index <- calc_community_index(dat_niche = dat_niche, dat_community = dat_community)

## -----------------------------------------------------------------------------
df_index_change_obs <- test_index_change_all(dat_index, option = "obs")
df_index_change_obs %>% knitr::kable()
df_index_change_exp <- test_index_change_all(dat_index, option = "exp")
df_index_change_exp %>% knitr::kable()
df_summ_index_change_obs <- summ_change(df_index_change_obs, option = "obs")

## -----------------------------------------------------------------------------
gg_community_index_obs <- plot_community_index(option = "obs", dat_index = dat_index)
gg_community_index_obs
gg_community_index_exp <- plot_community_index(option = "exp", dat_index = dat_index, experiment = "jrgce", treatment = "warming")
gg_community_index_exp

## ---- eval = F----------------------------------------------------------------
#  dat_shift <- calc_community_shift(dat_index = dat_index)

## -----------------------------------------------------------------------------
gg_community_shift <- plot_community_shift(dat_shift, dat_niche)
gg_community_shift$combined

## -----------------------------------------------------------------------------
df_summ_shift_coupling <- summ_shift_coupling(dat_shift)
df_summ_shift_coupling

## ---- eval = F----------------------------------------------------------------
#  save_figure(out = list(
#    guild_perc_obs = gg_guild_percentage_obs,
#    guild_perc_exp = gg_guild_percentage_exp,
#    trait_change_obs = gg_trait_change_obs,
#    trait_change_exp = gg_trait_change_exp,
#    community_index_obs = gg_community_index_obs,
#    community_index_exp = gg_community_index_exp,
#    community_shift = gg_community_shift$combined
#  ))
#  save_rda(out = list(dat_index, dat_shift))

