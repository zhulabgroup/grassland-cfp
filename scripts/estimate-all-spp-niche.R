# estimate/summarize all species niches
# temp using terraclim
niche_tbl <- read_rds(.path$cli_all_gbif) %>%
  select(key, species, tmp = terraclim_tmp, ppt = terraclim_ppt, vpd = terraclim_vpd) %>%
  group_by(species) %>%
  summarize(
    occ_n = n(),
    tmp_occ_mean = mean(tmp, na.rm = TRUE),
    tmp_occ_sd = sd(tmp, na.rm = TRUE),
    tmp_occ_median = median(tmp, na.rm = TRUE),
    tmp_occ_q05 = quantile(tmp, .05, na.rm = TRUE),
    tmp_occ_q25 = quantile(tmp, .25, na.rm = TRUE),
    tmp_occ_q75 = quantile(tmp, .75, na.rm = TRUE),
    tmp_occ_q95 = quantile(tmp, .95, na.rm = TRUE), # hot limit, suggested by Susan Harrison
    ppt_occ_mean = mean(ppt, na.rm = TRUE),
    ppt_occ_sd = sd(ppt, na.rm = TRUE),
    ppt_occ_median = median(ppt, na.rm = TRUE),
    ppt_occ_q05 = quantile(ppt, .05, na.rm = TRUE), # dry limit, suggested by Susan Harrison
    ppt_occ_q25 = quantile(ppt, .25, na.rm = TRUE),
    ppt_occ_q75 = quantile(ppt, .75, na.rm = TRUE),
    ppt_occ_q95 = quantile(ppt, .95, na.rm = TRUE),
    vpd_occ_mean = mean(vpd, na.rm = TRUE),
    vpd_occ_sd = sd(vpd, na.rm = TRUE),
    vpd_occ_median = median(vpd, na.rm = TRUE),
    vpd_occ_q05 = quantile(vpd, .05, na.rm = TRUE),
    vpd_occ_q25 = quantile(vpd, .25, na.rm = TRUE),
    vpd_occ_q75 = quantile(vpd, .75, na.rm = TRUE),
    vpd_occ_q95 = quantile(vpd, .95, na.rm = TRUE)
  )
