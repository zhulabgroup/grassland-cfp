# calculate CTI/CPI/other indexes
library(grassland)
dat_index <- calc_community_index(dat_niche = dat_niche, dat_community = dat_community)

# single site, plot residuals
df_site <-
  dat_index$obs %>%
  filter(site == "jasper") %>%
  select(site, year, plot, cti = tmp_com_mean, cpi = ppt_com_mean) %>%
  modelr::spread_residuals(
    resid_cti = lm(cti ~ year, data = .),
    resid_cpi = lm(cpi ~ year, data = .)
  )

df_site %>%
  pivot_longer(
    cols = cti:resid_cpi,
    names_to = "variable"
  ) %>%
  mutate(variable = factor(
    variable,
    levels = c("cti", "resid_cti", "cpi", "resid_cpi"),
    labels = c("CTI", "Residual of CTI", "CPI", "Residual of CPI")
  )) %>%
  ggplot(aes(year, value)) +
  geom_point() +
  facet_wrap(~variable, scales = "free_y")

lm(cti ~ year, data = df_site) %>%
  car::durbinWatsonTest()

lmtest::dwtest(cti ~ year, alternative = "two.sided", data = df_site)

# all sites, DW tests
dat_obs_cti_dwtest <-
  dat_index$obs %>%
  group_by(site) %>%
  nest() %>%
  mutate(
    dwtest = map(data, function(df) lmtest::dwtest(tmp_com_mean ~ year, alternative = "two.sided", data = df)),
    map_vec(dwtest, broom::tidy)
  )

dat_obs_cpi_dwtest <-
  dat_index$obs %>%
  group_by(site) %>%
  nest() %>%
  mutate(
    dwtest = map(data, function(df) lmtest::dwtest(ppt_com_mean ~ year, alternative = "two.sided", data = df)),
    map_vec(dwtest, broom::tidy)
  )

filter(dat_obs_cti_dwtest, p.value <= 0.01)
filter(dat_obs_cpi_dwtest, p.value <= 0.01)
