# niche data --------------------------------------------------------------
niche_tbl <- read_rds(.path$sum_niche) %>%
  filter(occ_n > 100 | is.na(occ_n)) %>% # species with many observations and dummy species
  select(species, tmp_occ_median, ppt_occ_median)

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

obs_gainloss_tbl <- read_rds(str_c(.path$sum_gainloss, "obs.rds")) %>%
  mutate(species_sep = str_split(species, pattern = " ")) %>%
  rowwise() %>%
  mutate(species_short = str_c(str_sub(species_sep[1], 1, 3), str_sub(species_sep[2], 1, 3))) %>%
  select(-species_sep)

obs_gainloss_eg1 <- obs_gainloss_tbl %>%
  group_by(species, change) %>%
  summarize(n = n()) %>%
  filter(change != "no clear change") %>%
  group_by(change) %>%
  arrange(desc(n)) %>%
  slice(1)

obs_gainloss_eg2 <- obs_gainloss_tbl %>%
  group_by(species, complete_change) %>%
  summarize(n = n()) %>%
  filter(!is.na(complete_change)) %>%
  group_by(complete_change) %>%
  arrange(desc(n)) %>%
  slice(1)

obs_gainloss_tbl_long <- obs_gainloss_tbl %>%
  pivot_longer(cols = tmp_occ_median:ppt_occ_median, names_to = "variable", values_to = "value") %>%
  mutate(variable = factor(variable,
    levels = c("tmp_occ_median", "ppt_occ_median"),
    labels = c("tmp", "ppt")
  ))

obs_gainloss_tbl_long %>%
  group_by(variable, change) %>%
  summarise(
    median = median(value, na.rm = T),
    lower = quantile(value, 0.025, na.rm = T),
    upper = quantile(value, 0.975, na.rm = T)
  )

wilcox.test(obs_gainloss_tbl_long %>% filter(variable == "tmp", change == "increase") %>% pull(value),
  obs_gainloss_tbl_long %>% filter(variable == "tmp", change == "decrease") %>% pull(value),
  alternative = "two.sided"
)

wilcox.test(obs_gainloss_tbl_long %>% filter(variable == "ppt", change == "increase") %>% pull(value),
  obs_gainloss_tbl_long %>% filter(variable == "ppt", change == "decrease") %>% pull(value),
  alternative = "two.sided"
)

obs_gainloss_summ_gg <-
  ggpubr::ggboxplot(
    data = obs_gainloss_tbl_long %>%
      mutate(change = case_when(
        change == "increase" ~ "Increase",
        change == "decrease" ~ "Decrease",
        change == "no clear change" ~ "No change"
      )) %>%
      mutate(change = factor(change, levels = c("Increase", "Decrease", "No change"))),
    x = "change",
    col = "change",
    y = "value"
  ) +
  scale_color_manual(values = c(Increase = "dark green", `No change` = "lightgray", Decrease = "dark orange")) +
  ggpubr::stat_compare_means(
    label = "p.signif",
    hide.ns = FALSE,
    comparisons = list(c("Increase", "Decrease"), c("Increase", "No change"), c("Decrease", "No change")),
    size = 3
  ) + # Add pairwise comparisons p-value
  # ggpubr::stat_compare_means() +    # Add global p-value
  facet_wrap(. ~ variable,
    ncol = 1,
    scales = "free_y",
    strip.position = "left",
    labeller = labeller(variable = c(
      tmp = .varname$tmp,
      ppt = .varname$ppt
    ))
  ) +
  guides(col = "none") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme(
    strip.background = element_blank(),
    strip.placement = "outside"
  ) +
  theme(
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8)
  )

obs_gainloss_main_gg <-
  ggplot() +
  geom_point(
    data = niche_tbl,
    aes(
      x = tmp_occ_median,
      y = ppt_occ_median
    ), color = "gray", alpha = 0
  ) +
  geom_point(
    data = obs_gainloss_tbl,
    aes(
      x = tmp_occ_median,
      y = ppt_occ_median,
      size = dominance,
      color = change
    ), alpha = 1, pch = 21, fill = NA
  ) +
  geom_point(
    data = obs_gainloss_tbl %>% filter(change == "increase" | change == "decrease") %>% filter(!is.na(complete_change)),
    aes(
      x = tmp_occ_median,
      y = ppt_occ_median,
      size = dominance,
      color = change,
      fill = complete_change,
    ), alpha = 0.75, pch = 21
  ) +
  scale_color_manual(values = c(increase = "dark green", `no clear change` = "lightgray", decrease = "dark orange")) +
  scale_fill_manual(values = c(new = "dark green", lost = "dark orange")) +
  labs(x = .varname$tmp, y = .varname$ppt) +
  guides(
    fill = "none",
    size = "none",
    color = "none"
  ) +
  theme(axis.text = element_text(size = 8))

obs_gainloss_main_3row_gg <- obs_gainloss_main_gg +
  facet_wrap(. ~ site,
    labeller = site_vec %>% as_labeller(),
    nrow = 3
  ) +
  theme(strip.text = element_text(hjust = 0))

obs_gainloss_main_2row_gg <- obs_gainloss_main_gg +
  facet_wrap(. ~ site,
    labeller = site_vec %>% as_labeller(),
    nrow = 2
  ) +
  theme(strip.text = element_text(hjust = 0))

obs_gainloss_supp_gg <- obs_gainloss_main_gg +
  ggrepel::geom_text_repel(
    data = obs_gainloss_tbl %>% filter(change == "increase" | change == "decrease") %>% filter(!is.na(complete_change)),
    aes(
      x = tmp_occ_median,
      y = ppt_occ_median,
      color = change,
      label = paste0("italic('", species_short, "')")
    ),
    size = 3.88 / 1.68,
    alpha = 1,
    max.overlaps = 100,
    parse = T
  )

obs_gainloss_supp_3row_gg <-
  obs_gainloss_supp_gg +
  facet_wrap(. ~ site,
    labeller = site_vec %>% as_labeller(),
    nrow = 3
  ) +
  theme(strip.text = element_text(hjust = 0))

obs_gainloss_supp_2row_gg <-
  obs_gainloss_supp_gg +
  facet_wrap(. ~ site,
    labeller = site_vec %>% as_labeller(),
    nrow = 2
  ) +
  theme(strip.text = element_text(hjust = 0))


# experimental data -------------------------------------------------------
exp_gainloss_tbl <- read_rds(str_c(.path$sum_gainloss, "exp.rds")) %>%
  mutate(change = factor(change, levels = c("increase", "decrease", "no clear change"))) %>%
  filter(year != 1998) %>%
  mutate(species_sep = str_split(species, pattern = " ")) %>%
  rowwise() %>%
  mutate(species_short = str_c(str_sub(species_sep[1], 1, 3), str_sub(species_sep[2], 1, 3))) %>%
  select(-species_sep) %>%
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
  ))

exp_gainloss_eg <- exp_gainloss_tbl %>%
  group_by(species, change) %>%
  summarize(n = n()) %>%
  filter(change != "no clear change") %>%
  group_by(change) %>%
  arrange(desc(n)) %>%
  slice(1)

exp_gainloss_tbl_long <- exp_gainloss_tbl %>%
  pivot_longer(cols = tmp_occ_median:ppt_occ_median, names_to = "variable", values_to = "value") %>%
  mutate(variable = factor(variable,
    levels = c("tmp_occ_median", "ppt_occ_median"),
    labels = c("tmp", "ppt")
  ))

exp_gainloss_tbl_long %>%
  group_by(variable, change) %>%
  summarise(
    median = median(value, na.rm = T),
    lower = quantile(value, 0.025, na.rm = T),
    upper = quantile(value, 0.975, na.rm = T)
  )

wilcox.test(exp_gainloss_tbl_long %>% filter(variable == "tmp", change == "increase") %>% pull(value),
  exp_gainloss_tbl_long %>% filter(variable == "tmp", change == "decrease") %>% pull(value),
  alternative = "two.sided"
)

wilcox.test(exp_gainloss_tbl_long %>% filter(variable == "ppt", change == "increase") %>% pull(value),
  exp_gainloss_tbl_long %>% filter(variable == "ppt", change == "decrease") %>% pull(value),
  alternative = "two.sided"
)

exp_gainloss_summ_gg <-
  ggpubr::ggboxplot(
    data = exp_gainloss_tbl_long %>%
      mutate(change = case_when(
        change == "increase" ~ "Increase",
        change == "decrease" ~ "Decrease",
        change == "no clear change" ~ "No change"
      )) %>%
      mutate(change = factor(change, levels = c("Increase", "Decrease", "No change"))),
    x = "change",
    col = "change",
    y = "value"
  ) +
  scale_color_manual(values = c(Increase = "dark green", `No change` = "lightgray", Decrease = "dark orange")) +
  ggpubr::stat_compare_means(
    label = "p.signif",
    hide.ns = FALSE,
    comparisons = list(c("Increase", "Decrease"), c("Increase", "No change"), c("Decrease", "No change")),
    size = 3
  ) + # Add pairwise comparisons p-value
  # ggpubr::stat_compare_means() +    # Add global p-value
  facet_wrap(. ~ variable,
    ncol = 1,
    scales = "free_y",
    strip.position = "left",
    labeller = labeller(variable = c(
      tmp = .varname$tmp,
      ppt = .varname$ppt
    ))
  ) +
  guides(col = "none") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme(
    strip.background = element_blank(),
    strip.placement = "outside"
  ) +
  theme(
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8)
  )

exp_gainloss_main_gg <-
  ggplot() +
  geom_point(
    data = niche_tbl,
    aes(
      x = tmp_occ_median,
      y = ppt_occ_median
    ), color = "gray", alpha = 0
  ) +
  geom_point(
    data = exp_gainloss_tbl %>% filter(year >= 2010),
    aes(
      x = tmp_occ_median,
      y = ppt_occ_median,
      size = dominance,
      color = change
    ), alpha = 1, pch = 21, fill = NA
  ) +
  geom_point(
    data = exp_gainloss_tbl %>% filter(change == "increase" | change == "decrease") %>% filter(!is.na(complete_change)) %>% filter(year >= 2010),
    aes(
      x = tmp_occ_median,
      y = ppt_occ_median,
      size = dominance,
      color = change,
      fill = complete_change,
    ), alpha = 0.75, pch = 21
  ) +
  scale_color_manual(values = c(increase = "dark green", `no clear change` = "lightgray", decrease = "dark orange")) +
  scale_fill_manual(values = c(new = "dark green", lost = "dark orange")) +
  labs(x = .varname$tmp, y = .varname$ppt) +
  guides(
    fill = "none",
    size = "none",
    color = "none"
  ) +
  facet_wrap(. ~ phaseyear,
    nrow = 1
  ) +
  theme(axis.text = element_text(size = 8)) +
  theme(strip.text = element_text(hjust = 0))

exp_gainloss_supp_gg <- ggplot() +
  geom_point(
    data = niche_tbl,
    aes(
      x = tmp_occ_median,
      y = ppt_occ_median
    ), color = "gray", alpha = 0
  ) +
  geom_point(
    data = exp_gainloss_tbl,
    aes(
      x = tmp_occ_median,
      y = ppt_occ_median,
      size = dominance,
      color = change
    ), alpha = 1, pch = 21, fill = NA
  ) +
  geom_point(
    data = exp_gainloss_tbl %>% filter(change == "increase" | change == "decrease") %>% filter(!is.na(complete_change)),
    aes(
      x = tmp_occ_median,
      y = ppt_occ_median,
      size = dominance,
      color = change,
      fill = complete_change,
    ), alpha = 0.75, pch = 21
  ) +
  scale_color_manual(values = c(increase = "dark green", `no clear change` = "lightgray", decrease = "dark orange")) +
  scale_fill_manual(values = c(new = "dark green", lost = "dark orange")) +
  labs(x = .varname$tmp, y = .varname$ppt) +
  guides(size = "none", color = "none", fill = "none") +
  facet_wrap(. ~ phaseyear,
    nrow = 3,
    drop = FALSE
  ) +
  ggrepel::geom_text_repel(
    data = exp_gainloss_tbl %>% filter(change != "no clear change"),
    aes(
      x = tmp_occ_median,
      y = ppt_occ_median,
      color = change,
      label = paste0("italic('", species_short, "')")
    ),
    size = 3.88 / 1.68,
    alpha = 1,
    max.overlaps = 100,
    parse = T
  ) +
  theme(strip.text = element_text(hjust = 0))

# combine and save -------------------------------------------------------------
gainloss_main_gg <-
  obs_gainloss_main_3row_gg +
  exp_gainloss_main_gg +
  obs_gainloss_summ_gg +
  exp_gainloss_summ_gg +
  plot_layout(design = "
  AAAAC
  AAAAC
  AAAAD
  BBBBD
") +
  plot_annotation(tag_levels = "A")

# save main figure
if (.fig_save) {
  ggsave(
    plot = gainloss_main_gg,
    filename = str_c(.path$out_fig, "fig-main-gainloss.png"),
    width = 11,
    height = 11.5,
    device = png, type = "cairo"
  )
}

# save supplementary figures
if (.fig_save) {
  ggsave(
    plot = obs_gainloss_supp_3row_gg,
    filename = str_c(.path$out_fig, "fig-supp-gainloss-obs.png"),
    width = 10,
    height = 8,
    device = png, type = "cairo"
  )
  ggsave(
    plot = exp_gainloss_supp_gg,
    filename = str_c(.path$out_fig, "fig-supp-gainloss-exp.png"),
    width = 12,
    height = 8,
    device = png, type = "cairo"
  )
}

# save slide figures
if (.fig_save) {
  ggsave(
    plot = obs_gainloss_main_2row_gg,
    filename = str_c(.path$out_fig, "fig-slide-gainloss-obs.png"),
    width = 10,
    height = 10 * .5,
    device = png, type = "cairo"
  )
  ggsave(
    plot = obs_gainloss_supp_2row_gg,
    filename = str_c(.path$out_fig, "fig-slide-gainloss-obs-name.png"),
    width = 10,
    height = 10 * .5,
    device = png, type = "cairo"
  )
  ggsave(
    plot = exp_gainloss_main_gg,
    filename = str_c(.path$out_fig, "fig-slide-gainloss-exp.png"),
    width = 10,
    height = 10 * .5 * .618,
    device = png, type = "cairo"
  )
  ggsave(
    plot = obs_gainloss_summ_gg,
    filename = str_c(.path$out_fig, "fig-slide-gainloss-boxplot-obs.png"),
    width = 3,
    height = 3 * 1.618,
    device = png, type = "cairo"
  )
  ggsave(
    plot = exp_gainloss_summ_gg,
    filename = str_c(.path$out_fig, "fig-slide-gainloss-boxplot-exp.png"),
    width = 3,
    height = 3 * 1.618,
    device = png, type = "cairo"
  )
}

gainloss_tbl <- bind_rows(
  obs_gainloss_tbl %>% mutate(dataset = "observation"),
  exp_gainloss_tbl %>% mutate(dataset = "experiment") %>%
    mutate(site = "Jasper Ridge Global Change Experiment")
) %>%
  select(dataset, site, year, species_short, species, change, comment = complete_change) %>%
  mutate(comment = case_when(change != "no clear change" ~ comment))

write_csv(gainloss_tbl, .path$out_tab_gainloss)
