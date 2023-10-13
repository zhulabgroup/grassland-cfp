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

dat_evenness <- calc_evenness(dat_community = dat_community)
df_evenness_summ <- test_evenness_change(dat_evenness = dat_evenness)
df_evenness_summ

gg_rank_abundance <- plot_rank_abundance(dat_rank = dat_rank, df_evenness_summ = df_evenness_summ)
gg_rank_abundance$obs
gg_rank_abundance$exp

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
#    species_change = gg_species_change$combined,
#    species_change_obs = gg_species_change$obs$circle_detail,
#    species_change_exp = gg_species_change$exp$circle_detail,
#    rank_abund_obs = gg_rank_abundance$obs,
#    rank_abund_exp = gg_rank_abundance$exp,
#    community_shift = gg_community_shift$combined
#  ))
#  
#  save_table(out = list(
#    gainloss = dat_gainloss %>% tidy_table_gainloss()
#  ))
#  
#  save_rda(out = list(dat_gainloss, dat_shift))

