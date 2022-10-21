# tmp niches
niche_tmp_gg <- niche_tbl %>%
  select(tmp_occ_mean, tmp_occ_median, tmp_occ_q05, tmp_occ_q95) %>%
  GGally::ggpairs(
    title = "Species temperature niche (°C)",
    columnLabels = c("Mean", "Median", "Lower limit (5%)", "Upper limit (95%)"),
  )

# ppt niches
niche_ppt_gg <- niche_tbl %>%
  select(ppt_occ_mean, ppt_occ_median, ppt_occ_q05, ppt_occ_q95) %>%
  GGally::ggpairs(
    title = "Species precipitation niche (mm)",
    columnLabels = c("Mean", "Median", "Lower limit (5%)", "Upper limit (95%)")
  )

# vpd niches
niche_vpd_gg <- niche_tbl %>%
  select(vpd_occ_mean, vpd_occ_median, vpd_occ_q05, vpd_occ_q95) %>%
  GGally::ggpairs(
    title = "Species VPD max niche (Pa)",
    columnLabels = c("Mean", "Median", "Lower limit (5%)", "Upper limit (95%)")
  )

# tmp, ppt, vpd niches
niche_dim_gg <- niche_tbl %>%
  select(tmp_occ_median, ppt_occ_median, vpd_occ_median) %>%
  GGally::ggpairs(
    title = "Species niche dimensions",
    columnLabels = c("Temperature (°C)", "Precipitation (mm)", "VPD max (Pa)")
  )

# mean tmp and ppt niches
niche_tp_gg <- niche_tbl %>%
  ggplot(aes(
    text = species,
    x = tmp_occ_mean,
    y = ppt_occ_mean,
    xmin = tmp_occ_mean - tmp_occ_sd / sqrt(occ_n),
    xmax = tmp_occ_mean + tmp_occ_sd / sqrt(occ_n),
    ymin = ppt_occ_mean - ppt_occ_sd / sqrt(occ_n),
    ymax = ppt_occ_mean + ppt_occ_sd / sqrt(occ_n)
  )) +
  geom_point() +
  geom_errorbarh(alpha = .5) +
  geom_errorbar(alpha = .5) +
  labs(
    x = "Species occurrence mean temperature (°C)",
    y = "Species occurrence mean precipitation (mm)"
  )
