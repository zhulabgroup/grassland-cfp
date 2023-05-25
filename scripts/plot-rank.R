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
    group_by(., site, year) %>%
      summarise(sum_abund = sum(abund)),
    by = c("site", "year")
  ) %>%
  mutate(dominance = abund / sum_abund) %>%
  group_by(site, year) %>%
  arrange(site, year, desc(dominance)) %>%
  mutate(rank = rank(-dominance)) %>%
  ungroup()

obs_rank_gg <- ggplot(df_obs_rank) +
  geom_line(aes(x = rank, y = abund, col = year, group = year), alpha = 0.75) +
  facet_wrap(. ~ site) +
  scale_color_viridis_c() +
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
  )

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
}