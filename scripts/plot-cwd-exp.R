# import niche and experimental data, calculate CTI and CPI
# niche data
niche_tbl <- read_rds(.path$sum_niche_cwd) %>%
  filter(occ_n > 100 | is.na(occ_n)) # species with many observations and dummy species

# JRGCE data
jrgce_tbl <- read_rds(.path$com_exp) %>%
  filter(site == "jrgce", year >= 1999) %>%
  inner_join(niche_tbl, by = "species") %>%
  group_by(site, year, plot, treat) %>%
  summarize(
    tmp_com_mean = sum(abund * tmp_occ_median) / sum(abund),
    ppt_com_mean = sum(abund * ppt_occ_median) / sum(abund),
    cwd_com_mean = sum(abund * cwd_occ_median) / sum(abund)
  ) %>%
  mutate(treat_T = str_sub(treat, start = 1L, end = 1L)) %>%
  mutate(treat_P = str_sub(treat, start = 2L, end = 2L)) %>%
  select(site, year, plot, treat_T, treat_P, tmp_com_mean, ppt_com_mean, cwd_com_mean) %>%
  pivot_longer(cols = tmp_com_mean:cwd_com_mean, names_to = "com_idx_name", values_to = "com_idx_value") %>%
  mutate(com_idx_name = factor(com_idx_name,
    levels = c("tmp_com_mean", "ppt_com_mean", "cwd_com_mean"),
    labels = c("CTI", "CPI", "CDI")
  ))

# warming phrases
warm_tbl <- tribble(
  ~tag, ~name, ~start, ~end, ~startyear,
  1, "Phase~I:~+80~W~m^-2%~~%+1~degree*C", -Inf, 2002, 1999,
  2, "Phase~II:~+100~W~m^-2%~~%+1.5~degree*C", 2003, 2009, 2003,
  3, "Phase~III:~+250~W~m^-2%~~%+2~degree*C", 2010, Inf, 2010 # end in 2014, but set to Inf to fill space
)

cwd_exp_gg <-
  ggplot(jrgce_tbl) +
  geom_rect( # warming phrases
    data = warm_tbl,
    aes(xmin = start - .5, xmax = end + .5, fill = tag),
    ymin = -Inf, ymax = Inf, alpha = 0.5
  ) +
  scale_fill_gradient(low = "antiquewhite", high = "orange") +
  geom_boxplot( # treatment effects
    aes(x = year, y = com_idx_value, col = treat_T, group = interaction(treat_T, year))
  ) +
  scale_color_manual(values = c("black", "red")) +
  ggpubr::stat_compare_means( # significance
    aes(x = year, y = com_idx_value, group = treat_T),
    method = "wilcox.test",
    label = "p.signif", hide.ns = FALSE
  ) +
  facet_wrap( # CTI & CDI panels
    ~com_idx_name,
    ncol = 1, scales = "free_y",
    strip.position = "left",
    labeller = labeller(com_idx_name = c(
      CTI = "Community Temperature Index\n(CTI, °C)",
      CPI = "Community Precipitation Index\n(CPI, mm)",
      CDI = "Community Drought Index\n(CDI, mm)"
    ))
  ) +
  scale_y_continuous(expand = expansion(mult = .1)) + # expand padding to show significance tests
  scale_x_continuous(expand = expansion(mult = 0, add = c(0.125, 0.125))) +
  labs(
    x = NULL, # "Year",
    y = NULL,
    color = "Warming treatment",
  ) +
  geom_text(
    data = warm_tbl %>%
      mutate(
        com_idx_name = factor("tmp_com_mean",
          levels = c("tmp_com_mean", "ppt_com_mean", "cwd_com_mean"),
          labels = c("CTI", "CPI", "CDI")
        )
      ),
    aes(
      label = name,
      x = startyear - 0.25, # y = cti_max
    ),
    y = 17, # manually label phase text
    parse = TRUE,
    hjust = 0,
  ) +
  coord_cartesian(clip = "off") +
  theme(
    strip.background = element_blank(),
    strip.placement = "outside",
    legend.position = "none",
    plot.margin = unit(c(2, 1, 1, 1), "lines") # expand margin to include warming labels
  )

# save figure file
if (.fig_save) {
  ggsave(
    plot = cwd_exp_gg,
    filename = str_c(.path$out_fig, "fig-main-exp-cwd.png"),
    width = 10,
    height = 6.18 / 2 * 3
  )
}

cwd_exp_water_gg <-
  ggplot(jrgce_tbl) +
  geom_boxplot( # treatment effects
    aes(x = year, y = com_idx_value, col = treat_P, group = interaction(treat_P, year))
  ) +
  scale_color_manual(values = c("black", "blue")) +
  ggpubr::stat_compare_means( # significance
    aes(x = year, y = com_idx_value, group = treat_P),
    method = "wilcox.test",
    label = "p.signif", hide.ns = FALSE
  ) +
  facet_wrap( # CTI & CDI panels
    ~com_idx_name,
    ncol = 1, scales = "free_y",
    strip.position = "left",
    labeller = labeller(com_idx_name = c(
      CTI = "Community Temperature Index\n(CTI, °C)",
      CPI = "Community Precipitation Index\n(CPI, mm)",
      CDI = "Community Drought Index\n(CDI, mm)"
    ))
  ) +
  scale_y_continuous(expand = expansion(mult = .1)) + # expand padding to show significance tests
  scale_x_continuous(expand = expansion(mult = 0, add = c(0.125, 0.125))) +
  labs(
    x = NULL, # "Year",
    y = NULL,
    color = "Watering treatment",
  ) +
  theme(
    strip.background = element_blank(),
    strip.placement = "outside",
    legend.position = "none",
    plot.margin = unit(c(2, 1, 1, 1), "lines"), # expand margin to include warming labels
    panel.background = element_rect(fill = "#F0F8FF")
  )

# save figure file
if (.fig_save) {
  ggsave(
    plot = cwd_exp_water_gg,
    filename = str_c(.path$out_fig, "fig-main-exp-water-cwd.pdf"),
    width = 10,
    height = 6.18 / 2 * 3
  )
}

# report stats
n_plt <- read_rds(.path$com_exp) %>%
  filter(site == "jrgce", year == 2014) %>%
  mutate(treat_T = str_sub(treat, start = 1L, end = 1L)) %>%
  distinct(plot, treat_T) %>%
  count(treat_T) %>%
  pull(n)
names(n_plt) <- c("ambient", "warming")

n_plt_water <- read_rds(.path$com_exp) %>%
  filter(site == "jrgce", year == 2014) %>%
  mutate(treat_P = str_sub(treat, start = 2L, end = 2L)) %>%
  distinct(plot, treat_P) %>%
  count(treat_P) %>%
  pull(n)
names(n_plt_water) <- c("ambient", "watering")

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
  for (metric in c("CTI", "CPI", "CDI")) {
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
  arrange(treatment) %>%
  mutate(sig = case_when(
    p <= 0.05 ~ "sig",
    TRUE ~ "ns"
  )) %>%
  # mutate(across(estimate, signif, 3)) %>%
  knitr::kable()
