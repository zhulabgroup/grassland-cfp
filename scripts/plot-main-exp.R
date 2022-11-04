# import niche and experimental data, calculate CTI and CPI
niche_tbl <- read_rds(.path$sum_niche) %>%
  filter(occ_n > 100 | is.na(occ_n)) # species with many observations and dummy species

exp_tbl <- read_rds(.path$com_exp) %>%
  inner_join(niche_tbl, by = "species") %>%
  group_by(site, year, plot, treat) %>%
  summarize(
    tmp_com_mean = sum(abund * tmp_occ_median) / sum(abund),
    ppt_com_mean = sum(abund * ppt_occ_median) / sum(abund)
  )

# compare CTI and CPI from ambient vs. warming treatments
exp_gg <-
  exp_tbl %>%
  filter(site == "jrgce") %>%
  mutate(treat_T = str_sub(treat, start = 1L, end = 1L)) %>%
  dplyr::select(site, year, plot, treat_T, tmp_com_mean, ppt_com_mean) %>%
  pivot_longer(cols = tmp_com_mean:ppt_com_mean, names_to = "com_idx_name", values_to = "com_idx_value") %>%
  mutate(com_idx_name = factor(com_idx_name,
                               levels = c("tmp_com_mean", "ppt_com_mean"),
                               labels = c("CTI", "CPI")
  )) %>%
  ggplot(aes(year, com_idx_value, col = treat_T, group = interaction(treat_T, year))) +
  geom_boxplot() +
  ggpubr::stat_compare_means(
    aes(group = treat_T),
    method = "wilcox.test",
    label = "p.signif", hide.ns = FALSE
  ) +
  facet_wrap(~com_idx_name,
             ncol = 1, scales = "free_y",
             strip.position = "left",
             labeller = labeller(com_idx_name = c(
               CTI = "Community Temperature Index\n(CTI, °C)",
               CPI = "Community Precipitation Index\n(CPI, mm)"
             ))
  ) +
  scale_y_continuous(expand = expansion(mult = .1)) + # expand padding to show significance tests
  labs(
    x = NULL, # "Year",
    y = NULL,
    color = "Warming treatment",
    title = "Jasper Ridge Global Change Experiment",
    subtitle = "<span style='color:black'>Ambient vs. </span><span style='color:red'>warming treatment</span>"
  ) +
  scale_color_manual(values = c("black", "red")) +
  theme(
    strip.background = element_blank(),
    strip.placement = "outside",
    legend.position = "none",
    plot.subtitle = ggtext::element_markdown()
  )

# save figure file
if (FALSE) {
  ggsave(
    plot = exp_gg,
    filename = "figures/fig2-exp.png",
    width = 10,
    height = 6.18
  )
}
