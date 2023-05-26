# observational data ------------------------------------------------------
site_vec <- c(
  angelo = "Angelo Coast",
  carrizo = "Carrizo Plain",
  elkhorn = "Elkhorn Slough",
  jasper = "Jasper Ridge Serpentine",
  mclann = "McLaughlin Annual",
  mclserp = "McLaughlin Serpentine",
  morganterritory = "Morgan Territory",
  pleasantonridge = "Pleasanton Ridge",
  sunol = "Sunol",
  swanton = "Swanton Ranch",
  ucsc = "UC Santa Cruz",
  vascocaves = "Vasco Caves"
)

df_obs_rank <- read_rds(.path$com_obs) %>%
  filter(guild != "DUMMY") %>%
  left_join(
    group_by(., site, year, plot) %>%
      summarise(sum_abund = sum(abund)),
    by = c("site", "year", "plot")
  ) %>%
  mutate(dominance = abund / sum_abund) %>%
  group_by(site, year, plot) %>%
  arrange(site, year, plot, desc(dominance)) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  group_by(site, year, rank) %>%
  summarise(
    median = quantile(abund, 0.5),
    lower = quantile(abund, 0.025),
    upper = quantile(abund, 0.975)
  ) %>%
  ungroup()

obs_rank_gg <- ggplot(df_obs_rank) +
  geom_line(aes(x = rank, y = median, col = year, group = year), alpha = 1) +
  # geom_ribbon(aes(x = rank, ymin = lower, ymax = upper, fill = year, group = year), alpha = 0.25) +
  facet_wrap(. ~ site) +
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  facet_wrap(. ~ site,
    scales = "free",
    labeller = site_vec %>% as_labeller()
  ) +
  theme(
    strip.background = element_blank(),
    strip.placement = "outside",
    axis.text = element_text(size = 8),
    strip.text = element_text(hjust = 0)
  ) +
  labs(
    x = "Rank",
    y = "Abundance",
    col = "Year"
  ) +
  guides(fill = "none")

df_obs_even <- read_rds(.path$com_obs) %>%
  filter(guild != "DUMMY") %>%
  left_join(
    group_by(., site, year, plot) %>%
      summarise(sum_abund = sum(abund)),
    by = c("site", "year", "plot")
  ) %>%
  mutate(dominance = abund / sum_abund) %>%
  group_by(site, year, plot) %>%
  summarise(
    h = vegan::diversity(abund),
    spn = n()
  ) %>%
  mutate(even = h / log(spn)) %>%
  ungroup() %>%
  group_by(site) %>%
  nest() %>%
  mutate(
    p_val = map(data, ~ lm(even ~ year, data = .)) %>%
      map_dbl(~ broom::glance(.) %>% pull(p.value))
  ) %>%
  unnest(cols = data)

obs_even_gg <- ggplot(df_obs_even) +
  geom_point(aes(x = year, y = even), alpha = 0.1) +
  geom_smooth(
    aes(x = year, y = even, linetype = ifelse(p_val <= 0.05, "sig", "ns")),
    method = "lm", formula = y ~ x, se = FALSE,
    color = "red"
  ) +
  scale_linetype_manual(values = c("sig" = "solid", "ns" = "dashed")) +
  facet_wrap(. ~ site, labeller = site_vec %>% as_labeller()) +
  labs(
    x = "Year",
    y = "Evenness"
  ) +
  guides(linetype = "none")

# experimental data ------------------------------------------------------
plot_treat <- read_rds(.path$com_exp) %>%
  filter(site == "jrgce") %>%
  filter(year == 1999) %>%
  mutate(treat_T = str_sub(treat, start = 1L, end = 1L)) %>%
  distinct(plot, treat_T)

df_exp_rank <- read_rds(.path$com_exp) %>%
  filter(site == "jrgce") %>%
  filter(year > 1998) %>%
  filter(guild != "DUMMY") %>%
  left_join(plot_treat, by = "plot") %>%
  left_join(
    group_by(., treat_T, year, plot) %>%
      summarise(sum_abund = sum(abund)),
    by = c("treat_T", "year", "plot")
  ) %>%
  mutate(dominance = abund / sum_abund) %>%
  group_by(treat_T, year, plot) %>%
  arrange(treat_T, year, plot, desc(dominance)) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  mutate(phase = case_when(
    year <= 2002 ~ "Phase I",
    year >= 2010 ~ "Phase III",
    TRUE ~ "Phase II"
  )) %>%
  mutate(phaseyear = paste0(phase, ": ", year)) %>%
  mutate(phaseyear = factor(phaseyear,
    levels = c(
      "Phase I: 1999",
      "Phase I: 2000",
      "Phase I: 2001",
      "Phase I: 2002",
      " ",
      "  ",
      "   ",
      "Phase II: 2003",
      "Phase II: 2004",
      "Phase II: 2005",
      "Phase II: 2006",
      "Phase II: 2007",
      "Phase II: 2008",
      "Phase II: 2009",
      "Phase III: 2010",
      "Phase III: 2011",
      "Phase III: 2012",
      "Phase III: 2013",
      "Phase III: 2014",
      "    ",
      "     "
    )
  )) %>%
  group_by(phaseyear, treat_T, rank) %>%
  summarise(
    median = quantile(abund, 0.5),
    lower = quantile(abund, 0.025),
    upper = quantile(abund, 0.975)
  )

exp_rank_gg <- ggplot(df_exp_rank) +
  geom_line(aes(x = rank, y = median, col = treat_T, group = treat_T), alpha = 1) +
  geom_ribbon(aes(x = rank, ymin = lower, ymax = upper, fill = treat_T, group = treat_T), alpha = 0.25) +
  facet_wrap(. ~ site) +
  facet_wrap(. ~ phaseyear,
    scales = "free",
    nrow = 3,
    drop = FALSE
  ) +
  scale_color_manual(values = c("black", "red")) +
  scale_fill_manual(values = c("black", "red")) +
  theme(
    strip.background = element_blank(),
    strip.placement = "outside",
    axis.text = element_text(size = 8),
    strip.text = element_text(hjust = 0),
    legend.position = "none"
  ) +
  labs(
    x = "Rank",
    y = "Abundance",
    col = "Warming treatment"
  )

df_exp_even <- read_rds(.path$com_exp) %>%
  filter(site == "jrgce") %>%
  filter(year > 1998) %>%
  filter(guild != "DUMMY") %>%
  left_join(plot_treat, by = "plot") %>%
  left_join(
    group_by(., treat_T, year, plot) %>%
      summarise(sum_abund = sum(abund)),
    by = c("treat_T", "year", "plot")
  ) %>%
  mutate(dominance = abund / sum_abund) %>%
  group_by(treat_T, year, plot) %>%
  summarise(
    h = vegan::diversity(abund),
    spn = n()
  ) %>%
  mutate(even = h / log(spn)) %>%
  ungroup()

warm_tbl <- tribble(
  ~tag, ~name, ~start, ~end, ~startyear,
  1, "Phase~I:~+80~W~m^-2%~~%+1~degree*C", -Inf, 2002, 1999,
  2, "Phase~II:~+100~W~m^-2%~~%+1.5~degree*C", 2003, 2009, 2003,
  3, "Phase~III:~+250~W~m^-2%~~%+2~degree*C", 2010, Inf, 2010 # end in 2014, but set to Inf to fill space
)


exp_even_gg <-
  ggplot(df_exp_even) +
  geom_rect( # warming phrases
    data = warm_tbl,
    aes(xmin = start - .5, xmax = end + .5, fill = tag),
    ymin = -Inf, ymax = Inf, alpha = 0.5
  ) +
  scale_fill_gradient(low = "antiquewhite", high = "orange") +
  geom_boxplot( # treatment effects
    aes(x = year, y = even, col = treat_T, group = interaction(treat_T, year))
  ) +
  scale_color_manual(values = c("black", "red")) +
  ggpubr::stat_compare_means( # significance
    aes(x = year, y = even, group = treat_T),
    method = "wilcox.test",
    label = "p.signif", hide.ns = FALSE
  ) +
  scale_y_continuous(expand = expansion(mult = .1)) + # expand padding to show significance tests
  scale_x_continuous(expand = expansion(mult = 0, add = c(0.125, 0.125))) +
  labs(
    x = "Year",
    y = "Evenness",
    color = "Warming treatment",
  ) +
  geom_text(
    data = warm_tbl,
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

if (.fig_save) {
  ggsave(
    plot = obs_rank_gg,
    filename = str_c(.path$out_fig, "fig-supp-rank-obs.png"),
    width = 10,
    height = 8
  )
  ggsave(
    plot = exp_rank_gg,
    filename = str_c(.path$out_fig, "fig-supp-rank-exp.png"),
    width = 12,
    height = 8
  )
  ggsave(
    plot = obs_even_gg,
    filename = str_c(.path$out_fig, "fig-supp-even-obs.png"),
    width = 10,
    height = 8
  )
  ggsave(
    plot = exp_even_gg,
    filename = str_c(.path$out_fig, "fig-supp-even-exp.png"),
    width = 12,
    height = 8
  )
}