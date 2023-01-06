# get CTI/CPI for observational communities
source("scripts/calculate-cwm.R")

# get climate data
clim_tbl <- read_rds(.path$cli_chelsa_annual) %>%
  filter(abbr %in% c(
    "angelo", "carrizo", "elkhorn", "jasper", "mclann", "mclserp",
    "morganterritory", "pleasantonridge", "sunol",
    "swanton", "ucsc", "vascocaves"
  ))

# join
cwm_clim_tbl <- obs_tbl %>%
  group_by(site, year) %>%
  summarize(
    cti = mean(tmp_com_mean),
    cpi = mean(ppt_com_mean)
  ) %>%
  inner_join(clim_tbl, by = c("site" = "abbr", "year"))

# cti ~ tmp
cti_gg <-
  ggplot(cwm_clim_tbl, aes(tmp, cti, group = name, color = name)) +
  geom_point() +
  stat_ellipse() +
  labs(
    x = "Mean annual temperature (°C)",
    y = "Community Temperature Index (CTI, °C)",
    color = "Site"
  )

# cpi ~ ppt
cpi_gg <-
  ggplot(cwm_clim_tbl, aes(ppt, cpi, group = name, color = name)) +
  geom_point() +
  stat_ellipse() +
  labs(
    x = "Mean annual precipitation (mm)",
    y = "Community Precipitation Index (CPI, mm)",
    color = "Site"
  )
