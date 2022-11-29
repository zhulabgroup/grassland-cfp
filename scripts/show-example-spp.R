# observational data
obs_tbl <- read_rds(.path$com_obs) %>%
  group_by(site, year, plot) %>%
  summarize(tot_abund = sum(abund)) %>%
  ungroup() %>%
  full_join(read_rds(.path$com_obs), by = c("site", "year", "plot")) %>%
  mutate(rel_abund = abund / tot_abund)

obs_tbl %>%
  filter(species == "Danthonia californica") %>%
  ggplot(aes(year, rel_abund)) +
  geom_boxplot(aes(group = year)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") +
  ggpubr::stat_cor(aes(label = ..p.label..), p.accuracy = 0.05, color = "red") +
  scale_y_log10() +
  facet_wrap(~site)

obs_tbl %>%
  filter(species == "Stipa pulchra") %>%
  ggplot(aes(year, rel_abund)) +
  geom_boxplot(aes(group = year)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") +
  ggpubr::stat_cor(aes(label = ..p.label..), p.accuracy = 0.05, color = "red") +
  scale_y_log10() +
  facet_wrap(~site)

# experimental data
exp_tbl <- read_rds(.path$com_exp) %>%
  group_by(site, year, plot, treat) %>%
  summarize(tot_abund = sum(abund)) %>%
  ungroup() %>%
  full_join(read_rds(.path$com_exp), by = c("site", "year", "plot", "treat")) %>%
  mutate(rel_abund = abund / tot_abund) %>%
  filter(site == "jrgce") %>%
  mutate(treat = str_sub(treat, start = 1L, end = 1L))

exp_tbl %>%
  filter(species == "Danthonia californica") %>%
  ggplot(aes(year, rel_abund)) +
  geom_boxplot(aes(group = interaction(year, treat), color = treat)) +
  # scale_y_log10() +
  scale_color_manual(values = c("black", "red")) +
  ggpubr::stat_compare_means( # significance
    aes(x = year, y = rel_abund, group = treat),
    method = "wilcox.test",
    label = "p.signif", hide.ns = FALSE
  )

exp_tbl %>%
  filter(species == "Stipa pulchra") %>%
  ggplot(aes(year, rel_abund)) +
  geom_boxplot(aes(group = interaction(year, treat), color = treat)) +
  # scale_y_log10() +
  scale_color_manual(values = c("black", "red")) +
  ggpubr::stat_compare_means( # significance
    aes(x = year, y = rel_abund, group = treat),
    method = "wilcox.test",
    label = "p.signif", hide.ns = FALSE
  )
