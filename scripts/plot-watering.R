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
  mutate(treat_P = str_sub(treat, start = 2L, end = 2L)) %>%
  select(site, year, plot, treat_P, tmp_com_mean, ppt_com_mean) %>%
  pivot_longer(cols = tmp_com_mean:ppt_com_mean, names_to = "com_idx_name", values_to = "com_idx_value") %>%
  mutate(com_idx_name = factor(com_idx_name,
    levels = c("tmp_com_mean", "ppt_com_mean"),
    labels = c("CTI", "CPI")
  ))

exp_water_gg <-
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
  facet_wrap( # CTI & CPI panels
    ~com_idx_name,
    ncol = 1, scales = "free_y",
    strip.position = "left",
    labeller = labeller(com_idx_name = c(
      CTI = "Community Temperature Index\n(CTI, °C)",
      CPI = "Community Precipitation Index\n(CPI, mm)"
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
    plot = exp_water_gg,
    filename = str_c(.path$out_fig, "fig-supp-watering.png"),
    width = 10,
    height = 6.18
  )
}

# report stats
n_plt <- read_rds(.path$com_exp) %>%
  filter(site == "jrgce", year == 2014) %>%
  mutate(treat_P = str_sub(treat, start = 2L, end = 2L)) %>%
  distinct(plot, treat_P) %>%
  count(treat_P) %>%
  pull(n)
names(n_plt) <- c("ambient", "watering")
