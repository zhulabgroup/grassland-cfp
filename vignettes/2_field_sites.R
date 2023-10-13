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
dat_clim_site <- read_climate_site()
df_cc_site <- calc_climate_change_site(dat_clim_site = dat_clim_site)
df_cc_site %>%
  mutate(
    estimate = estimate %>% signif(3),
    std.error = std.error %>% signif(3),
    p.value = p.value %>% round(4)
  ) %>%
  knitr::kable()
df_summ_site_cc <- summ_change(df_change = df_cc_site, option = "obs")
gg_site_cc <- plot_site_climate_change(dat_clim_site = dat_clim_site, dat_avail = df_data_avail)

## ---- eval = F----------------------------------------------------------------
#  save_figure(out = list(data_avail = gg_data_avail,
#                         site_cc = gg_site_cc))
#  
#  save_extdata(out = list("alldata/input/basemap/site_info.csv"))

