## ---- include = FALSE---------------------------------------------------------
knitr::opts_knit$set(root.dir = here::here()) # knit from project directory
knitr::opts_chunk$set(
  collapse = TRUE,
  out.width = "100%",
  fig.width = 10,
  fig.height = 10,
  cache = T,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(grassland)
theme_set(ggthemes::theme_few())

## -----------------------------------------------------------------------------
dat_community

## -----------------------------------------------------------------------------
dat_occ$gbif
# dat_clim
dat_trait

## -----------------------------------------------------------------------------
dat_niche <- calc_species_niche(dat_trait, add_dummy = T)

gg_individual_trait_species_niche_cool <- plot_individual_trait_species_niche_ind_sp(dat_occ, dat_trait, sp = "Danthonia californica")
gg_individual_trait_species_niche_warm <- plot_individual_trait_species_niche_ind_sp(dat_occ, dat_trait, sp = "Stipa pulchra")
gg_individual_trait_species_niche_cool
gg_individual_trait_species_niche_warm

gg_species_niche <- plot_species_niche(dat_niche,
  cool_species = "Danthonia californica",
  warm_species = "Stipa pulchra"
)
gg_species_niche

## -----------------------------------------------------------------------------
dat_index <- calc_community_index(dat_niche = dat_niche, dat_community = dat_community)
gg_community_index_obs <- plot_community_index(option = "obs", dat_index = dat_index)
gg_community_index_obs
gg_community_index_exp <- plot_community_index(option = "exp", dat_index = dat_index, experiment = "jrgce", treatment = "warming")
gg_community_index_exp

## -----------------------------------------------------------------------------
df_trait_change_obs <- test_trait_change_all(dat_community, dat_niche, option = "obs")
df_trait_change_obs %>% knitr::kable()
df_trait_change_exp <- test_trait_change_all(dat_community, dat_niche, option = "exp")
df_trait_change_exp %>% knitr::kable()

## -----------------------------------------------------------------------------
df_index_change_obs <- test_index_change_all(dat_index, option = "obs")
df_index_change_obs %>% knitr::kable()
df_index_change_exp <- test_index_change_all(dat_index, option = "exp")
df_index_change_exp %>% knitr::kable()

## -----------------------------------------------------------------------------
dat_shift <- calc_community_shift(dat_index = dat_index)
gg_community_shift <- plot_community_shift(dat_shift, dat_niche)
gg_community_shift$obs
gg_community_shift$exp
gg_community_shift$compare

