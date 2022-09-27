# read original and thinned niche tables
niche_tbl <- read_rds(.path$sum_niche) %>%
  filter(occ_n > 100) %>%
  select(species, occ_n)

thin_tbl <- read_rds(.path$sum_thin) %>%
  inner_join(niche_tbl, ., by = "species")

thin_tmp_gg <- thin_tbl %>%
  select(
    tmp_mean_full, tmp_mean_thin,
    tmp_median_full, tmp_median_thin
  ) %>%
  GGally::ggpairs(
    lower = list(continuous = GGally::wrap("points", alpha = 0.2)),
    title = "Species temperature niche (°C)",
    columnLabels = c("Full mean", "Thinned mean", "Full median", "Thinned median")
  )

thin_ppt_gg <- thin_tbl %>%
  select(
    ppt_mean_full, ppt_mean_thin,
    ppt_median_full, ppt_median_thin
  ) %>%
  GGally::ggpairs(
    lower = list(continuous = GGally::wrap("points", alpha = 0.2)),
    title = "Species precipitation niche (mm)",
    columnLabels = c("Full mean", "Thinned mean", "Full median", "Thinned median")
  )
