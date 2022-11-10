# explore annual site climate change patterns
plot_clim <- function(data, mapping, xlab = "Year", ylab) {
  # define function to plot climate change ~ year | site
  ggplot(data, mapping) +
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
    labs(x = xlab, y = ylab)
}

clim_tbl <- read_rds(.path$cli_chelsa_annual) %>%
  filter(abbr %in% c(
    "angelo", "carrizo", "elkhorn", "jasper", "mclann", "mclserp",
    "morganterritory", "pleasantonridge", "sunol",
    "swanton", "ucsc", "vascocaves"
  ))

tmp_gg <- plot_clim(clim_tbl, aes(x = year, y = tmp), ylab = "Mean annual temperature (°C)")
ppt_gg <- plot_clim(clim_tbl, aes(x = year, y = ppt), ylab = "Mean annual precipitation (mm)")
vpd_gg <- plot_clim(clim_tbl, aes(x = year, y = tmp), ylab = "Mean VPDmax (hPa)")
