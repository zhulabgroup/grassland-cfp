# read climate data
clim_wide_tbl <- read_rds(.path$cli_all_gbif) %>%
  as_tibble() %>%
  drop_na() # PRISM = NA in Mexico

# reshape
clim_long_tbl <- bind_rows(
  clim_wide_tbl %>%
    dplyr::select(
      tmp1 = chelsa_tmp,
      tmp2 = prism_tmp,
      ppt1 = chelsa_ppt,
      ppt2 = prism_ppt,
      vpd1 = chelsa_vpd,
      vpd2 = prism_vpd
    ) %>%
    mutate(compare = "chelsa-prism"),
  clim_wide_tbl %>%
    dplyr::select(
      tmp1 = chelsa_tmp,
      tmp2 = terraclim_tmp,
      ppt1 = chelsa_ppt,
      ppt2 = terraclim_ppt,
      vpd1 = chelsa_vpd,
      vpd2 = terraclim_vpd
    ) %>%
    mutate(compare = "chelsa-terraclim"),
  clim_wide_tbl %>%
    dplyr::select(
      tmp1 = prism_tmp,
      tmp2 = terraclim_tmp,
      ppt1 = prism_ppt,
      ppt2 = terraclim_ppt,
      vpd1 = prism_vpd,
      vpd2 = terraclim_vpd
    ) %>%
    mutate(compare = "prism-terraclim")
)

# temporarily remove CHELSA--need to debug
clim_long_tbl <- clim_long_tbl %>% 
  filter(compare == "prism-terraclim")

tmp_gg <- ggplot(clim_long_tbl, aes(tmp1, tmp2)) +
  geom_hex(bins = 100) +
  viridis::scale_fill_viridis() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", lty = "dashed") +
  ggpubr::stat_cor(
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),
    p.accuracy = 0.05,
    color = "red"
  ) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  coord_fixed() +
  labs(
    x = "Temperature 1 (°C)",
    y = "Temperature 2 (°C)",
    fill = "Occurrence site"
  ) +
  facet_wrap(. ~ compare)

ppt_gg <- ggplot(clim_long_tbl, aes(ppt1, ppt2)) +
  geom_hex(bins = 100) +
  viridis::scale_fill_viridis() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", lty = "dashed") +
  ggpubr::stat_cor(
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),
    p.accuracy = 0.05,
    color = "red"
  ) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  coord_fixed() +
  labs(
    x = "Precipitation 1 (mm)",
    y = "Precipitation 2 (mm)",
    fill = "Occurrence site"
  ) +
  facet_wrap(. ~ compare)

vpd_gg <- ggplot(clim_long_tbl, aes(vpd1, vpd2)) +
  geom_hex(bins = 100) +
  viridis::scale_fill_viridis() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", lty = "dashed") +
  ggpubr::stat_cor(
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),
    p.accuracy = 0.05,
    color = "red"
  ) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  coord_fixed() +
  labs(
    x = "VPD max 1 (Pa)",
    y = "VPD max 2 (Pa)",
    fill = "Occurrence site"
  ) +
  facet_wrap(. ~ compare)

# making maps is possible but too time consuming.
if (FALSE) {
  clim_long_sf <- read_rds(.path$cli_all_gbif) %>%
    pivot_longer(
      contains(match = c("chelsa", "prism")),
      names_to = "dataset_variable",
      values_to = "value"
    ) %>%
    separate(
      col = dataset_variable,
      into = c("dataset", "variable"),
      sep = "_"
    )

  ggplot() +
    geom_sf(
      data = rnaturalearth::ne_states(
        country = c("Mexico", "United States of America"),
        returnclass = "sf"
      ),
      fill = NA,
      color = alpha("black", .1)
    ) +
    geom_sf(data = cfp_sf, fill = "white", alpha = .5) +
    geom_sf(
      data = clim_long_sf,
      aes(geometry = geometry, color = value),
      alpha = .1, size = .1
    ) +
    facet_wrap(variable ~ dataset) +
    scale_color_viridis() +
    labs(x = "Longitude", y = "Latitude") +
    coord_sf(xlim = c(-125, -115), ylim = c(28, 44))
}
