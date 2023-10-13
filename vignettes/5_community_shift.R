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
df_trait_change_obs <- test_trait_change_all(dat_community, dat_niche, option = "obs")
df_trait_change_obs %>% 
  mutate(
    estimate = estimate %>% signif(3),
    std.error = std.error %>% signif(3),
    p.value = p.value %>% round(4)
  ) %>% 
  knitr::kable()
df_trait_change_exp <- test_trait_change_all(dat_community, dat_niche, option = "exp")
df_trait_change_exp %>% 
   mutate(
    estimate = estimate %>% signif(3),
    std.error = std.error %>% signif(3),
    p.value = p.value %>% round(4)
  ) %>% 
  knitr::kable()
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
df_index_change_obs  %>% 
   mutate(
    estimate = estimate %>% signif(3),
    std.error = std.error %>% signif(3),
    p.value = p.value %>% round(4)
  ) %>% 
knitr::kable()
df_index_change_exp <- test_index_change_all(dat_index, option = "exp")
df_index_change_exp  %>% 
   mutate(
    estimate = estimate %>% signif(3),
    std.error = std.error %>% signif(3),
    p.value = p.value %>% round(4)
  ) %>%
  knitr::kable()
df_summ_index_change_obs <- summ_change(df_index_change_obs, option = "obs")

## -----------------------------------------------------------------------------
gg_community_index_obs <- plot_community_index(option = "obs", dat_index = dat_index)
gg_community_index_obs
gg_community_index_exp <- plot_community_index(option = "exp", dat_index = dat_index, experiment = "jrgce", treatment = "warming")
gg_community_index_exp

## ---- eval = F----------------------------------------------------------------
#  save_figure(out = list(
#    trait_change_obs = gg_trait_change_obs,
#    trait_change_exp = gg_trait_change_exp,
#    community_index_obs = gg_community_index_obs,
#    community_index_exp = gg_community_index_exp
#  ))
#  save_rda(out = list(dat_index))

