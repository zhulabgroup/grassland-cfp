# read species climate niche estimates
niche_tbl <- read_rds(.path_ls$sum_niche) %>%
  filter(occ_n > 100) # no dummy species

# temperature niche stats
niche_tmp_gg <- niche_tbl %>%
  dplyr::select(tmp_occ_mean, tmp_occ_median, tmp_occ_q05, tmp_occ_q95) %>%
  GGally::ggpairs(
    lower = list(continuous = GGally::wrap("points", alpha = 0.2)),
    title = "Species temperature niche (°C)",
    columnLabels = c("Mean", "Median", "Lower limit (5%)", "Upper limit (95%)")
  ) +
  theme(strip.background = element_blank())

# precipitation niche stats
niche_ppt_gg <- niche_tbl %>%
  dplyr::select(ppt_occ_mean, ppt_occ_median, ppt_occ_q05, ppt_occ_q95) %>%
  GGally::ggpairs(
    lower = list(continuous = GGally::wrap("points", alpha = 0.2)),
    title = "Species precipitation niche (mm)",
    columnLabels = c("Mean", "Median", "Lower limit (5%)", "Upper limit (95%)")
  ) +
  theme(strip.background = element_blank())
