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
  select(site, year, plot, treat_T, tmp_com_mean, ppt_com_mean) %>%
  pivot_longer(cols = tmp_com_mean:ppt_com_mean, names_to = "com_idx_name", values_to = "com_idx_value") %>%
  mutate(com_idx_name = factor(com_idx_name,
    levels = c("tmp_com_mean", "ppt_com_mean"),
    labels = c("CTI", "CPI")
  ))

# warming phrases
warm_tbl <- tribble(
  ~tag, ~name, ~start, ~end, ~mid,
  1, "Phase~I:~+80~W~m^-2%~~%+1~degree*C", -Inf, 2002, mean(c(1998.5, 2002.5)),
  2, "Phase~II:~+100~W~m^-2%~~%+1.5~degree*C", 2003, 2009, mean(c(2002.5, 2009.5)),
  3, "Phase~III:~+250~W~m^-2%~~%+2~degree*C", 2010, Inf, mean(c(2009.5, 2014.5)) # end in 2014, but set to Inf to fill space
) %>%
  mutate(
    cti_max = jrgce_tbl %>%
      filter(com_idx_name == "CTI") %>%
      ungroup() %>%
      summarize(max = max(com_idx_value)) %>%
      pull(max),
    cti_min = jrgce_tbl %>%
      filter(com_idx_name == "CTI") %>%
      ungroup() %>%
      summarize(min = min(com_idx_value)) %>%
      pull(min),
    com_idx_name = factor("tmp_com_mean",
      levels = c("tmp_com_mean", "ppt_com_mean"),
      labels = c("CTI", "CPI")
    )
  )

exp_gg <-
  ggplot(jrgce_tbl) +
  geom_rect( # warming phrases
    data = warm_tbl %>% select(-com_idx_name),
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
  labs(
    x = NULL, # "Year",
    y = NULL,
    color = "Warming treatment",
  ) +
  theme(
    strip.background = element_blank(),
    strip.placement = "outside",
    legend.position = "none",
    plot.subtitle = ggtext::element_markdown()
  ) +
  geom_text(
    data = warm_tbl,
    aes(
      label = name,
      x = mid, y = cti_max
    ),
    nudge_y = .5,
    parse = TRUE
  )

# save figure file
if (.fig_save) {
  ggsave(
    plot = exp_gg,
    filename = str_c(.path$out_fig, "fig-main-exp-max.png"),
    width = 10,
    height = 6.18
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
