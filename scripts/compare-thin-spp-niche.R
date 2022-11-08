# compare thin vs. full niche estimates
thin_tbl <- read_rds(.path$sum_thin)

thin_tmp_gg <- thin_tbl %>%
  select(
    full_mean_tmp, thin_mean_tmp,
    full_median_tmp, thin_median_tmp
  ) %>%
  GGally::ggpairs(
    lower = list(continuous = GGally::wrap("points", alpha = 0.2)),
    title = "Species temperature niche (°C)",
    columnLabels = c("Full mean", "Thinned mean", "Full median", "Thinned median")
  )

thin_ppt_gg <- thin_tbl %>%
  select(
    full_mean_ppt, thin_mean_ppt,
    full_median_ppt, thin_median_ppt
  ) %>%
  GGally::ggpairs(
    lower = list(continuous = GGally::wrap("points", alpha = 0.2)),
    title = "Species precipitation niche (mm)",
    columnLabels = c("Full mean", "Thinned mean", "Full median", "Thinned median")
  )
