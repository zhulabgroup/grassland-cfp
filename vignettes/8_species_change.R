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

## ---- eval=F------------------------------------------------------------------
#  dat_gainloss <- calc_species_gainloss(dat_community = dat_community, dat_niche = dat_niche)
#  # need to output to csv

## -----------------------------------------------------------------------------
gg_species_change <- plot_species_gainloss(dat_niche = dat_niche, dat_gainloss = dat_gainloss)
gg_species_change$combined

## -----------------------------------------------------------------------------
df_summ_species_change_obs <- summ_species_change(dat_gainloss, option = "obs")
df_summ_species_change_exp <- summ_species_change(dat_gainloss, option = "exp")

## -----------------------------------------------------------------------------
dat_rank <- calc_rank_abundance(dat_community = dat_community)
gg_rank_abundance <- plot_rank_abundance(dat_rank = dat_rank)
gg_rank_abundance$obs
gg_rank_abundance$exp

## -----------------------------------------------------------------------------
dat_evenness <- calc_evenness(dat_community = dat_community)
test_evenness_change(dat_evenness = dat_evenness)

## ---- eval = F----------------------------------------------------------------
#  save_figure(out = list(
#    species_change = gg_species_change$combined,
#    species_change_obs = gg_species_change$obs$circle_detail,
#    species_change_exp = gg_species_change$exp$circle_detail,
#    rank_abund_obs = gg_rank_abundance$obs,
#    rank_abund_exp = gg_rank_abundance$exp
#  ))
#  save_rda(out = list(dat_gainloss))

