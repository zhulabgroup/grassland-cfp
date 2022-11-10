# explore annual site climate change patterns
clim_tbl <- read_rds(.path$cli_chelsa_annual)

tmp_gg <- ggplot(clim_tbl, aes(year, tmp)) +
  geom_point() +
  geom_smooth(
    method = "lm", formula = y ~ x, se = FALSE,
    color = "red", lty = "dashed"
  ) +
  ggpubr::stat_cor(aes(label = ..p.label..),
    p.accuracy = 0.05,
    color = "red"
  ) +
  facet_wrap(~name, scales = "free_y") +
  labs(x = "Year", y = "Mean annual temperature (°C)")

ppt_gg <- ggplot(clim_tbl, aes(year, ppt)) +
  geom_point() +
  geom_smooth(
    method = "lm", formula = y ~ x, se = FALSE,
    color = "red", lty = "dashed"
  ) +
  ggpubr::stat_cor(aes(label = ..p.label..),
    p.accuracy = 0.05,
    color = "red"
  ) +
  facet_wrap(~name, scales = "free_y") +
  labs(x = "Year", y = "Mean annual precipitation (mm)")

vpd_gg <- ggplot(clim_tbl, aes(year, vpd)) +
  geom_point() +
  geom_smooth(
    method = "lm", formula = y ~ x, se = FALSE,
    color = "red", lty = "dashed"
  ) +
  ggpubr::stat_cor(aes(label = ..p.label..),
    p.accuracy = 0.05,
    color = "red"
  ) +
  facet_wrap(~name, scales = "free_y") +
  labs(x = "Year", y = "Mean VPDmax (hPa)")
