
# calculate CWM (community weighted mean, or sd, etc.) like CTI and CPI

calculate_community_index <- function(dat_niche, dat_community) {
  # load niche estimates.
  niche_tbl <- dat_niche %>%
    filter(occ_n > 100 | is.na(occ_n)) # species with many observations and dummy species

  # observational data and CWM
  obs_tbl <- dat_community$obs %>%
    inner_join(niche_tbl, by = "species") %>%
    group_by(site, year, plot) %>% # w/o treatment for obs
    sum_cwm() %>%
    ungroup()

  # experimental data and CWM
  exp_tbl <- dat_community$exp %>%
    inner_join(niche_tbl, by = "species") %>%
    group_by(site, year, plot, treat) %>% # w/ treatment for exp
    sum_cwm() %>%
    ungroup()

  out <- list(
    obs = obs_tbl,
    exp = exp_tbl
  )

  return(out)
}


# define summarize CWM function
sum_cwm <- function(.) {
  # CTI, CTI_sd, CPI, CPI_sd, CVI, CVI_sd
  summarize(.,
    tmp_com_mean = sum(abund * tmp_occ_mean) / sum(abund),
    tmp_com_var = sum(abund * tmp_occ_mean^2) / sum(abund) - tmp_com_mean^2,
    ppt_com_mean = sum(abund * ppt_occ_mean) / sum(abund),
    ppt_com_var = sum(abund * ppt_occ_mean^2) / sum(abund) - ppt_com_mean^2,
    vpd_com_mean = sum(abund * vpd_occ_mean) / sum(abund),
    vpd_com_var = sum(abund * vpd_occ_mean^2) / sum(abund) - vpd_com_mean^2
  ) %>%
    mutate(
      tmp_com_sd = sqrt(tmp_com_var),
      ppt_com_sd = sqrt(ppt_com_var),
      vpd_com_sd = sqrt(vpd_com_var)
    )
}
