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
#  spp_tbl <- tidy_taxonomy_consolidate(dat_community = dat_community)
#  path_occ <- down_biogeography(species_table = spp_tbl) # needs parallel computation
#  dat_occ_full <- read_biogeography(path_occ = path_occ, gbif_only = F)
#  dat_occ <- read_biogeography(path_occ = path_occ, gbif_only = T)

## -----------------------------------------------------------------------------
dat_occ_full <- read_biogeography(gbif_only = F)
gg_biogeography <- plot_biogeography(dat_occ_full)
gg_biogeography

## -----------------------------------------------------------------------------
df_summ_occ <- summ_biogeography(dat_occ = dat_occ_full)
df_summ_occ

## ---- eval = F----------------------------------------------------------------
#  dat_clim <- read_climate()
#  path_trait <- calc_individual_trait(dat_occ, dat_clim, occ_source = "gbif", clim_source = "chelsa")
#  dat_trait <- read_individual_trait(path_trait = path_trait)

## -----------------------------------------------------------------------------
dat_niche <- calc_species_niche(dat_trait, add_dummy = T)
df_summ_niche <- summ_species_niche(dat_niche = dat_niche)

## -----------------------------------------------------------------------------
dat_guild_niche <- calc_guild_niche(dat_community, dat_niche)
gg_guild_niche <- plot_guild_niche(dat_guild_niche = dat_guild_niche)
gg_guild_niche
res_guild_niche <- test_guild_niche(dat_guild_niche = dat_guild_niche)
res_guild_niche

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

## -----------------------------------------------------------------------------
gg_niche <- plot_individual_trait_species_niche_all(dat_occ, dat_trait, dat_niche, frac = 0)
gg_niche$sp_niche

## -----------------------------------------------------------------------------
mod_species_niche_cor <- test_species_niche(dat_niche = dat_niche)
mod_species_niche_cor

## ---- eval = F----------------------------------------------------------------
#  path_trait_chelsa <- calc_individual_trait(dat_occ, dat_clim, occ_source = "gbif", clim_source = "chelsa")
#  path_trait_prism <- calc_individual_trait(dat_occ, dat_clim, occ_source = "gbif", clim_source = "prism")
#  path_trait_terraclim <- calc_individual_trait(dat_occ, dat_clim, occ_source = "gbif", clim_source = "terraclim")

## -----------------------------------------------------------------------------
dat_trait_chelsa <- read_individual_trait(path_trait = "alldata/intermediate/climate-niche/gbif-chelsa.rds")
dat_trait_prism <- read_individual_trait(path_trait = "alldata/intermediate/climate-niche/gbif-prism.rds")
dat_trait_terraclim <- read_individual_trait(path_trait = "alldata/intermediate/climate-niche/gbif-terraclim.rds")
gg_climate_source <- plot_climate_source(dat_trait_chelsa, dat_trait_prism, dat_trait_terraclim)
gg_climate_source

## ---- eval = F----------------------------------------------------------------
#  path_trait_bioclim <- calc_individual_trait(dat_occ, dat_clim, occ_source = "gbif", clim_source = "chelsa", allbioclim = T)

## -----------------------------------------------------------------------------
dat_trait_bioclim <- read_individual_trait(path_trait = "alldata/intermediate/climate-niche/gbif-chelsa_full.rds")
gg_bioclim_tmp <- plot_bioclim(dat_trait_bioclim, var = "tmp")
gg_bioclim_tmp
gg_bioclim_ppt <- plot_bioclim(dat_trait_bioclim, var = "ppt")
gg_bioclim_ppt

## -----------------------------------------------------------------------------
gg_niche_stat_tmp <- plot_niche_stat(dat_niche, var = "tmp")
gg_niche_stat_tmp
gg_niche_stat_ppt <- plot_niche_stat(dat_niche, var = "ppt")
gg_niche_stat_ppt

## ---- eval = F----------------------------------------------------------------
#  path_occ_thin <- cal_thin_occ(dat_occ = dat_occ$gbif) # slow

## -----------------------------------------------------------------------------
dat_trait_thin <- calc_trait_thin(path_occ_thin = NULL, dat_trait)
dat_niche_thin <- calc_species_niche(dat_trait_thin, add_dummy = T)
gg_niche_thin <- plot_niche_thin(dat_niche, dat_niche_thin)
gg_niche_thin

## ---- eval = F----------------------------------------------------------------
#  save_figure(out = list(
#    biogeography = gg_biogeography,
#    guild_niche = gg_guild_niche,
#    niche = gg_niche$combined,
#    niche_cool = gg_individual_trait_species_niche_cool,
#    niche_warm = gg_individual_trait_species_niche_warm,
#    clim_source = gg_climate_source,
#    bioclim_tmp = gg_bioclim_tmp,
#    bioclim_ppt = gg_bioclim_ppt,
#    niche_stat_tmp = gg_niche_stat_tmp,
#    niche_stat_ppt = gg_niche_stat_ppt,
#    niche_thin = gg_niche_thin
#  ))
#  save_rda(out = list(dat_occ, dat_trait, dat_niche))

