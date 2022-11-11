# read climate datasets
clim_gbif_tbl <- read_rds(.path$geo_clim) %>%
  as_tibble()

chelsa_gbif_tbl <- clim_gbif_tbl %>%
  dplyr::select(
    geometry, key, species,
    chelsa_tmp, chelsa_ppt
  ) %>%
  pivot_longer(chelsa_tmp:chelsa_ppt,
               names_to = "variable",
               values_to = "CHELSA"
  ) %>%
  mutate(variable = str_sub(variable, start = -3L, end = -1L))

prism_gbif_tbl <- clim_gbif_tbl %>%
  dplyr::select(
    geometry, key, species,
    prism_tmp, prism_ppt
  ) %>%
  pivot_longer(prism_tmp:prism_ppt,
               names_to = "variable",
               values_to = "PRISM"
  ) %>%
  mutate(variable = str_sub(variable, start = -3L, end = -1L))

terraclim_gbif_tbl <- clim_gbif_tbl %>%
  select(
    geometry, key, species,
    terraclim_tmp, terraclim_ppt
  ) %>%
  pivot_longer(terraclim_tmp:terraclim_ppt,
               names_to = "variable",
               values_to = "TerraClim"
  ) %>%
  mutate(variable = str_sub(variable, start = -3L, end = -1L))

# combine
clim_comp_tbl <- chelsa_gbif_tbl %>%
  inner_join(
    prism_gbif_tbl,
    by = c("geometry", "key", "species", "variable")
  ) %>%
  inner_join(
    terraclim_gbif_tbl,
    by = c("geometry", "key", "species", "variable")
  )

chelsa_prism_gg <- ggplot(clim_comp_tbl, aes(CHELSA, PRISM)) +
  geom_hex(bins = 100) +
  viridis::scale_fill_viridis() +
  geom_smooth(method = "lm", formula = y ~ x - 1, se = FALSE, color = "red", lty = "dashed") +
  ggpubr::stat_cor(
    aes(label = paste(..r.label.., ..p.label.., sep = "*`,`~")),
    p.accuracy = 0.05,
    color = "red"
  ) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  facet_wrap(. ~ factor(
    variable,
    levels = c("tmp", "ppt"),
    labels = c(
      "Mean annual temperature (°C)",
      "Mean annual precipitation (mm)"
    )
  ),
  scales = "free"
  ) +
  labs(
    x = "",
    y = "PRISM",
    # fill = "Occurrence record"
  ) +
  guides(fill = "none") +
  theme(
    aspect.ratio = 1, # can't do + coord_fixed() with facet_wrap(scales = "free")
    strip.background = element_blank(),
    strip.text.x = element_text(size = 12)
  )

chelsa_terraclim_gg <- ggplot(clim_comp_tbl, aes(CHELSA, TerraClim)) +
  geom_hex(bins = 100) +
  viridis::scale_fill_viridis() +
  geom_smooth(method = "lm", formula = y ~ x - 1, se = FALSE, color = "red", lty = "dashed") +
  ggpubr::stat_cor(
    aes(label = paste(..r.label.., ..p.label.., sep = "*`,`~")),
    p.accuracy = 0.05,
    color = "red"
  ) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  facet_wrap(. ~ factor(
    variable,
    levels = c("tmp", "ppt"),
    labels = c(
      "Mean annual temperature (°C)",
      "Mean annual precipitation (mm)"
    )
  ),
  scales = "free"
  ) +
  labs(
    x = "CHELSA",
    y = "TerraClimate",
    # fill = "Occurrence record"
  ) +
  guides(fill = "none") +
  theme(
    aspect.ratio = 1, # can't do + coord_fixed() with facet_wrap(scales = "free")
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )
