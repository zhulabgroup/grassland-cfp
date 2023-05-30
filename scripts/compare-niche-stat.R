# read species climate niche estimates
niche_tbl <- read_rds(.path$sum_niche) %>%
  filter(occ_n > 100) # no dummy species

# temperature niche stats
niche_tmp_gg <-
  niche_tbl %>%
  dplyr::select(tmp_occ_mean, tmp_occ_median, tmp_occ_q05, tmp_occ_q95) %>%
  GGally::ggpairs(
    lower = list(continuous = GGally::wrap("points", alpha = 0.2)),
    # title = "Species temperature niche (°C)",
    columnLabels = c("Mean (°C)", "Median (°C)", "Lower limit (5%, °C)", "Upper limit (95%, °C)"),
    switch = "both"
  ) +
  theme(
    strip.background = element_blank(),
    strip.placement = "outside"
  )

# precipitation niche stats
niche_ppt_gg <-
  niche_tbl %>%
  dplyr::select(ppt_occ_mean, ppt_occ_median, ppt_occ_q05, ppt_occ_q95) %>%
  GGally::ggpairs(
    lower = list(continuous = GGally::wrap("points", alpha = 0.2)),
    # title = "Species precipitation niche (mm)",
    columnLabels = c("Mean (mm)", "Median (mm)", "Lower limit (5%, mm)", "Upper limit (95%, mm)"),
    switch = "both"
  ) +
  theme(
    strip.background = element_blank(),
    strip.placement = "outside"
  )

# save figure file
if (.fig_save) {
  ggsave(
    plot = niche_tmp_gg,
    filename = str_c(.path$out_fig, "fig-supp-niche-tmp.png"),
    width = 7,
    height = 7,
    device = png, type = "cairo"
  )
  ggsave(
    plot = niche_ppt_gg,
    filename = str_c(.path$out_fig, "fig-supp-niche-ppt.png"),
    width = 7,
    height = 7,
    device = png, type = "cairo"
  )
}
