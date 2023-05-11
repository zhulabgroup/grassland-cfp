# import niche and experimental data, calculate CTI and CPI
# niche data
niche_tbl <- read_rds(.path$sum_niche) %>%
  filter(occ_n > 100 | is.na(occ_n)) # species with many observations and dummy species

# experiment data
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

mclexp_tbl <- read_rds(.path$com_exp) %>%
  filter(site == "mclexp", year > 2015) %>%
  inner_join(niche_tbl, by = "species") %>%
  group_by(site, year, plot, treat) %>%
  summarize(
    tmp_com_mean = sum(abund * tmp_occ_median) / sum(abund),
    ppt_com_mean = sum(abund * ppt_occ_median) / sum(abund)
  ) %>%
  separate(treat, c("treat", "soil"), sep = 2) %>%
  pivot_longer(cols = tmp_com_mean:ppt_com_mean, names_to = "com_idx_name", values_to = "com_idx_value") %>%
  mutate(com_idx_name = factor(com_idx_name,
    levels = c("tmp_com_mean", "ppt_com_mean"),
    labels = c("CTI", "CPI")
  ))

scide_tbl <- read_rds(.path$com_exp) %>%
  filter(str_detect(site, "scide")) %>%
  mutate(site = str_replace(site, "scide_", "")) %>%
  mutate(site = factor(site,
    levels = c("arboretum", "marshallfield", "ylr"),
    labels = c("Arboretum", "Marshall Field", "Younger Lagoon")
  )) %>%
  inner_join(niche_tbl, by = "species") %>%
  group_by(site, year, plot, treat) %>%
  summarize(
    tmp_com_mean = sum(abund * tmp_occ_median) / sum(abund),
    ppt_com_mean = sum(abund * ppt_occ_median) / sum(abund)
  ) %>%
  pivot_longer(cols = tmp_com_mean:ppt_com_mean, names_to = "com_idx_name", values_to = "com_idx_value") %>%
  mutate(com_idx_name = factor(com_idx_name,
    levels = c("tmp_com_mean", "ppt_com_mean"),
    labels = c("CTI", "CPI")
  ))

# statistical tests pooled across years
lme_calc <- function(exp, trt) {
  exp_lme_stat <- data.frame(
    metric = character(0),
    estimate = numeric(0),
    p_val = numeric(0)
  )

  if (exp == "scide") {
    data_lme <- scide_tbl %>%
      mutate(treat_D = case_when(
        treat == "_" ~ "ambient",
        treat == "D" ~ "drought"
      )) %>%
      mutate(treat_D = factor(treat_D, levels = c("ambient", "drought")))
    for (metric in c("CTI", "CPI")) {
      if (trt == "drought") {
        s_list <- scide_tbl %>%
          pull(site) %>%
          unique()
        for (s in s_list) {
          data_sub <- data_lme %>%
            filter(site == s) %>%
            mutate(treat = treat_D) %>%
            filter(com_idx_name == metric)
          m <- nlme::lme(com_idx_value ~ treat,
            random = ~ 1 | year,
            data = data_sub
          )
          m_sum <- summary(m)
          exp_lme_stat <- exp_lme_stat %>%
            bind_rows(
              data.frame(
                Site = s,
                metric = metric,
                estimate = m_sum$tTable[2, 1],
                p_val = m_sum$tTable[2, 5]
              )
            ) %>%
            select(Site, everything())
        }
      }
    }
  }

  if (exp == "mclexp") {
    data_lme <- mclexp_tbl %>%
      mutate(treat_W = case_when(
        treat == "_X" ~ "ambient",
        treat == "WX" ~ "water"
      )) %>%
      mutate(treat_W = factor(treat_W, levels = c("ambient", "water"))) %>%
      mutate(treat_D = case_when(
        treat == "X_" ~ "ambient",
        treat == "XD" ~ "drought"
      )) %>%
      mutate(treat_D = factor(treat_D, levels = c("ambient", "drought")))

    for (metric in c("CTI", "CPI")) {
      if (trt == "water") {
        s_list <- c("S", "N")
      }
      if (trt == "drought") {
        s_list <- c("S")
      }
      for (s in s_list) {
        if (trt == "water") {
          data_sub <- data_lme %>%
            mutate(treat = treat_W) %>%
            filter(com_idx_name == metric) %>%
            filter(soil == s) %>%
            select(-treat_W, -treat_D) %>%
            drop_na()
        }
        if (trt == "drought") {
          data_sub <- data_lme %>%
            mutate(treat = treat_D) %>%
            filter(com_idx_name == metric) %>%
            filter(soil == s) %>%
            select(-treat_W, -treat_D) %>%
            drop_na()
        }
        m <- nlme::lme(com_idx_value ~ treat,
          random = ~ 1 | year,
          data = data_sub
        )
        m_sum <- summary(m)
        exp_lme_stat <- exp_lme_stat %>%
          bind_rows(
            data.frame(
              Site = s,
              metric = metric,
              estimate = m_sum$tTable[2, 1],
              p_val = m_sum$tTable[2, 5]
            ) %>%
              mutate(Site = case_when(
                Site == "S" ~ "Serpentine",
                Site == "N" ~ "Non-serpentine"
              )) %>%
              mutate(Site = factor(Site, levels = c("Serpentine", "Non-serpentine")))
          ) %>%
          select(Site, everything())
      }
    }
  }

  if (exp == "jrgce") {
    data_lme <- jrgce_tbl %>%
      mutate(phase = case_when(
        year <= 2002 ~ "I",
        year >= 2010 ~ "III",
        TRUE ~ "II"
      ))
    for (metric in c("CTI", "CPI")) {
      if (trt == "warm") {
        for (p in c("I", "II", "III")) {
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
                Phase = p,
                metric = metric,
                estimate = m_sum$tTable[2, 1],
                p_val = m_sum$tTable[2, 5]
              )
            ) %>%
            select(Phase, everything())
        }
      }
      if (trt == "water") {
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
              metric = metric,
              estimate = m_sum$tTable[2, 1],
              p_val = m_sum$tTable[2, 5]
            )
          )
      }
    }
  }

  lme_tbl <-
    exp_lme_stat %>%
    mutate(sig = gtools::stars.pval(p_val)) %>%
    mutate(sig = ifelse(sig != " ", sig, "ns")) %>%
    rename(Metric = metric, Estimate = estimate, `p-value` = p_val, Significance = sig) %>%
    mutate(
      across(Estimate, signif, 3),
      across(`p-value`, round, 4)
    )

  return(lme_tbl)
}

jrgce_warm_lme <- lme_calc(exp = "jrgce", trt = "warm")
jrgce_water_lme <- lme_calc(exp = "jrgce", trt = "water") %>%
  mutate(
    Experiment = "Jasper Ridge Global Change Experiment",
    Treatment = "Watering"
  ) %>%
  select(Experiment, Treatment, everything())

mclexp_water_lme <- lme_calc(exp = "mclexp", trt = "water") %>%
  mutate(
    Experiment = "McLaughlin Water Experiment",
    Treatment = "Watering"
  ) %>%
  select(Experiment, Treatment, Site, everything())

mclexp_drought_lme <- lme_calc(exp = "mclexp", trt = "drought") %>%
  mutate(
    Experiment = "McLaughlin Water Experiment",
    Treatment = "Drought"
  ) %>%
  select(Experiment, Treatment, Site, everything())

scide_drought_lme <- lme_calc(exp = "scide", trt = "drought") %>%
  mutate(
    Experiment = "Santa Cruz International Drought Experiment",
    Treatment = "Drought"
  ) %>%
  select(Experiment, Treatment, everything())

extra_lme <- bind_rows(
  jrgce_water_lme,
  mclexp_water_lme,
  mclexp_drought_lme,
  scide_drought_lme
) %>%
  mutate(Experiment = factor(Experiment,
    levels = c(
      "Jasper Ridge Global Change Experiment",
      "McLaughlin Water Experiment",
      "Santa Cruz International Drought Experiment"
    ),
    labels = c("JRGCE", "MWE", "SCIDE")
  )) %>%
  mutate(Metric = factor(Metric, levels = c("CTI", "CPI"))) %>%
  mutate(Treatment = factor(Treatment, levels = c("Watering", "Drought"))) %>%
  mutate(Site = factor(Site, levels = c("Serpentine", "Non-serpentine", "Arboretum", "Marshall Field", "Younger Lagoon"))) %>%
  select(Experiment, Treatment, Site, everything()) %>%
  arrange(Experiment, Treatment, Site, Metric)
