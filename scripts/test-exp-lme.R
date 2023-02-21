# import niche and experimental data, calculate CTI and CPI
# niche data
niche_tbl <- read_rds(.path$sum_niche) %>%
  filter(occ_n > 100 | is.na(occ_n)) # species with many observations and dummy species

# JRGCE data
jrgce_tbl <- read_rds(.path$com_exp) %>%
  filter(site == "jrgce", year >= 1999) %>%
  inner_join(niche_tbl, by = "species") %>%
  group_by(site, year, plot, treat) %>%
  summarize(
    tmp_com_mean = sum(abund * tmp_occ_median) / sum(abund),
    ppt_com_mean = sum(abund * ppt_occ_median) / sum(abund)
  ) %>%
  mutate(treat_T = str_sub(treat, start = 1L, end = 1L)) %>%
  mutate(treat_P = str_sub(treat, start = 2L, end = 2L)) %>%
  select(site, year, plot, treat_T, treat_P, tmp_com_mean, ppt_com_mean) %>%
  pivot_longer(cols = tmp_com_mean:ppt_com_mean, names_to = "com_idx_name", values_to = "com_idx_value") %>%
  mutate(com_idx_name = factor(com_idx_name,
    levels = c("tmp_com_mean", "ppt_com_mean"),
    labels = c("CTI", "CPI")
  ))

# statistical tests pooled across years
data_lme <- jrgce_tbl %>%
  mutate(phase = case_when(
    year <= 2002 ~ 1,
    year >= 2010 ~ 3,
    TRUE ~ 2
  ))

exp_lme_stat <- data.frame(
  treatment = character(0),
  metric = character(0),
  phase = numeric(0),
  estimate = numeric(0),
  p = numeric(0)
)
for (treatment in c("warm", "water")) {
  for (metric in c("CTI", "CPI")) {
    if (treatment == "warm") {
      for (p in 1:3) {
        data_sub <- data_lme %>%
          mutate(treat = treat_T) %>%
          filter(com_idx_name == metric) %>%
          filter(phase == p)
        m <- nlme::lme(com_idx_value ~ treat,
          random = ~ 1 | year,
          data = data_sub
        )
        m_sum <- summary(m)
        exp_lme_stat <- exp_lme_stat %>%
          bind_rows(
            data.frame(
              treatment = treatment,
              metric = metric,
              phase = p,
              estimate = m_sum$tTable[2, 1],
              p = m_sum$tTable[2, 5]
            )
          )
      }
    }
    if (treatment == "water") {
      data_sub <- data_lme %>%
        mutate(treat = treat_P) %>%
        filter(com_idx_name == metric)
      m <- nlme::lme(com_idx_value ~ treat,
        random = ~ 1 | year,
        data = data_sub
      )
      m_sum <- summary(m)

      exp_lme_stat <- exp_lme_stat %>%
        bind_rows(
          data.frame(
            treatment = treatment,
            metric = metric,
            phase = NA,
            estimate = m_sum$tTable[2, 1],
            p = m_sum$tTable[2, 5]
          )
        )
    }
  }
}

exp_lme_stat %>%
  mutate(sig = case_when(
    p <= 0.05 ~ "sig",
    TRUE ~ "ns"
  )) %>%
  mutate(across(estimate, signif, 3)) %>%
  knitr::kable()
