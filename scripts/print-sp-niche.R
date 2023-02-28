# print species niche table to CSV file
niche_tbl <- read_rds(.path$sum_niche) %>%
  filter(occ_n > 100) %>% # species with many observations and dummy species
  mutate(across(where(is.double), signif, 3)) %>%
  select(
    Species = species, n = occ_n,
    `Temperature mean` = tmp_occ_mean, `Temperature median` = tmp_occ_median,
    `Temperature lower limit (5%)` = tmp_occ_q05, `Temperature upper limit (95%)` = tmp_occ_q95,
    `Precipitation mean` = ppt_occ_mean, `Precipitation median` = ppt_occ_median,
    `Precipitation lower limit (5%)` = ppt_occ_q05, `Precipitation upper limit (95%)` = ppt_occ_q95
  )

write_csv(niche_tbl, .path$out_tab_niche)
