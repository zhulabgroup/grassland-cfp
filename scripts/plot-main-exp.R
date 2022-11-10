# import niche and experimental data, calculate CTI and CPI
# niche data
niche_tbl <- read_rds(.path$sum_niche) %>%
  filter(occ_n > 100 | is.na(occ_n)) # species with many observations and dummy species

# JRGCE data
jrgce_tbl <- read_rds(.path$com_exp) %>%
  filter(site == "jrgce") %>%
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

# warming phrases: +80 W m–2 (years 2‒5), to +100 W m–2 (years 6‒12), to +250 W m–2 (years 13‒17)
warm_tbl <- tribble(
  ~tag, ~name, ~start, ~end,
  0, "Pre", 1998, 1998,
  1, "Phase I", 1999, 2002,
  2, "Phase II", 2003, 2009,
  3, "Phase III", 2010, Inf # end in 2014, but set to Inf to fill space
)

exp_gg <- ggplot(jrgce_tbl) +
  geom_rect( # warming phrases
    data = warm_tbl,
    aes(xmin = start - .5, xmax = end + .5, fill = tag),
    ymin = -Inf, ymax = Inf, alpha = 0.2
  ) +
  scale_fill_gradient(low = "white", high = "orange") +
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
    title = "Jasper Ridge Global Change Experiment",
    subtitle = "<span style='color:black'>Ambient vs. </span><span style='color:red'>warming treatment</span>"
  ) +
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

# report stats
n_plt <- read_rds(.path$com_exp) %>%
  filter(site == "jrgce", year == 2014) %>%
  mutate(treat_T = str_sub(treat, start = 1L, end = 1L)) %>%
  distinct(plot, treat_T) %>%
  count(treat_T) %>%
  pull(n)
names(n_plt) <- c("ambient", "warming")
