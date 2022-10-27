# calculate CWM (community weighted mean, or sd, etc.) like CTI and CPI

# load niche estimates.
niche_tbl <- read_rds(.path$sum_niche)

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
    mutate(tmp_com_sd = sqrt(tmp_com_var),
           ppt_com_sd = sqrt(ppt_com_var),
           vpd_com_sd = sqrt(vpd_com_var))
}

# experimental data and CWM
exp_tbl <- read_rds(.path$com_exp) %>%
  inner_join(niche_tbl, by = "species") %>%
  group_by(site, year, plot, treat) %>% # w/ treatment for exp
  sum_cwm() %>% 
  ungroup()

# observational data and CWM
obs_tbl <- read_rds(.path$com_obs) %>%
  inner_join(niche_tbl, by = "species") %>%
  group_by(site, year, plot) %>% # w/o treatment for obs
  sum_cwm() %>% 
  ungroup()
