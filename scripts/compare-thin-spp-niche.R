# compare thin vs. full niche estimates
thin_tbl <- read_rds(.path$sum_thin)

thin_tmp_gg <- thin_tbl %>%
  select(full_mean_tmp, thin_mean_tmp) %>%
  GGally::ggpairs(
    title = "Species occurrence mean temperature (°C)",
    columnLabels = c("Full data", "Thinned data")
  )

thin_ppt_gg <- thin_tbl %>%
  select(full_mean_ppt, thin_mean_ppt) %>%
  GGally::ggpairs(
    title = "Species occurrence mean precipitation (mm)",
    columnLabels = c("Full data", "Thinned data")
  )
