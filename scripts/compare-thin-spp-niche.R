# compare thin vs. full niche estimates
thin_tbl <- read_rds(.path$sum_thin)

thin_tmp_gg <- thin_tbl %>%
  select(
    tmp_mean_full, tmp_mean_thin,
    tmp_median_full, tmp_median_thin
  ) %>%
  GGally::ggpairs(
    title = "Species temperature niche (°C)",
    columnLabels = c("Full mean", "Thinned mean", "Full median", "Thinned median")
  )

thin_ppt_gg <- thin_tbl %>%
  select(
    ppt_mean_full, ppt_mean_thin,
    ppt_median_full, ppt_median_thin
  ) %>%
  GGally::ggpairs(
    title = "Species precipitation niche (mm)",
    columnLabels = c("Full mean", "Thinned mean", "Full median", "Thinned median")
  )
