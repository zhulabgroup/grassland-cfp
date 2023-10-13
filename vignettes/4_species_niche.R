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
df_summ_occ <- summ_biogeography(dat_occ = dat_occ_full)
df_summ_occ %>% knitr::kable()

## -----------------------------------------------------------------------------
gg_individual_trait_species_niche_cool <- plot_individual_trait_species_niche_ind_sp(dat_occ, dat_trait, sp = "Danthonia californica")
gg_individual_trait_species_niche_cool
gg_individual_trait_species_niche_warm <- plot_individual_trait_species_niche_ind_sp(dat_occ, dat_trait, sp = "Stipa pulchra")
gg_individual_trait_species_niche_warm

## -----------------------------------------------------------------------------
df_summ_niche_cool <- summ_species_niche(dat_niche = dat_niche, sp = "Danthonia californica")
df_summ_niche_warm <- summ_species_niche(dat_niche = dat_niche, sp = "Stipa pulchra")

## ---- eval = F----------------------------------------------------------------
#  path_gg_trait <- plot_individual_trait_species_niche_ind_sp_all(dat_occ, dat_trait, dat_niche)

## ---- eval = F----------------------------------------------------------------
#  gg_niche <- plot_individual_trait_species_niche_all(dat_occ, dat_trait, dat_niche, frac = 1)
#  gg_niche$sp_niche

## ---- eval = F----------------------------------------------------------------
#  path_occ_thin <- cal_thin_occ(dat_occ = dat_occ$gbif, dat_trait = dat_trait, option = "spatial", num_cores = 4) # slow

## -----------------------------------------------------------------------------
dat_trait_thin <- calc_trait_thin(path_occ_thin = NULL, dat_trait, option = "spatial")

## -----------------------------------------------------------------------------
dat_niche_thin <- calc_species_niche(dat_trait_thin, add_dummy = T)
gg_niche_thin <- plot_niche_subset(dat_niche, dat_niche_thin)
gg_niche_thin

## ---- eval = F----------------------------------------------------------------
#  path_occ_thin_clim <- cal_thin_occ(dat_occ = dat_occ$gbif, dat_trait = dat_trait, option = "climate", num_cores = 4) # slow

## -----------------------------------------------------------------------------
dat_trait_thin_clim <- calc_trait_thin(path_occ_thin = NULL, dat_trait, option = "climate")

## -----------------------------------------------------------------------------
dat_niche_thin_clim <- calc_species_niche(dat_trait_thin_clim, add_dummy = T)
gg_niche_thin_clim <- plot_niche_subset(dat_niche, dat_niche_thin_clim)
gg_niche_thin_clim

## ---- eval=F------------------------------------------------------------------
#  spp_tbl <- tidy_taxonomy_consolidate(dat_community = dat_community)
#  path_occ_early <- down_biogeography(species_table = spp_tbl, start_year = 1961, end_year = 1990, postfix = "early", gbif_only = T) # needs parallel computation
#  dat_occ_early <- read_biogeography(path_occ_early, gbif_only = T)

## ---- eval = F----------------------------------------------------------------
#  dat_clim <- read_climate()
#  path_trait_early <- calc_individual_trait(dat_occ_early, dat_clim, occ_source = "gbif", clim_source = "chelsa", postfix = "early")
#  dat_trait_early <- read_individual_trait(path_trait = path_trait_early)

