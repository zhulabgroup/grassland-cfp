# summary tables to report statistics
# niche data
niche_tbl <- read_rds(.path$sum_niche) %>%
  filter(occ_n > 100 | is.na(occ_n)) # species with many observations and dummy species

niche_tbl %>%
  filter(!is.na(occ_n)) %>%
  mutate(
    occ_n = occ_n %>% format(big.mark = ","),
    across(starts_with(c("tmp", "ppt")), round, 3)
  ) %>%
  select(
    Species = species, n = occ_n,
    `T median (°C)` = tmp_occ_mean, `T std dev (°C)` = tmp_occ_sd,
    `P median (mm)` = ppt_occ_mean, `P std dev (mm)` = ppt_occ_sd,
  ) %>%
  knitr::kable(caption = "Species climate niche summary")
