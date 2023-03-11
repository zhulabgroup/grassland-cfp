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

obs_gainloss_tbl <- read_rds(str_c(.path$sum_gainloss, "obs.rds"))

obs_gainloss_tbl %>%
  group_by(species, change) %>%
  summarize(n = n()) %>%
  filter(change != "no clear change") %>%
  group_by(change) %>%
  arrange(desc(n)) %>%
  slice(1)

obs_gainloss_tbl %>%
  group_by(species, complete) %>%
  summarize(n = n()) %>%
  filter(!is.na(complete)) %>%
  group_by(complete) %>%
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

wilcox.test(obs_gainloss_tbl_long %>% filter(variable == "tmp", change == "gain") %>% pull(value),
  obs_gainloss_tbl_long %>% filter(variable == "tmp", change == "loss") %>% pull(value),
  alternative = "two.sided"
)

wilcox.test(obs_gainloss_tbl_long %>% filter(variable == "ppt", change == "gain") %>% pull(value),
  obs_gainloss_tbl_long %>% filter(variable == "ppt", change == "loss") %>% pull(value),
  alternative = "two.sided"
)

obs_gainloss_summ_gg <-
  ggplot(obs_gainloss_tbl_long %>%
    mutate(change = case_when(
      change == "gain" ~ "Gain",
      change == "loss" ~ "Loss",
      change == "no clear change" ~ "No change"
    ))) +
  geom_boxplot(aes(x = change, col = change, y = value)) +
  scale_color_manual(values = c(Gain = "dark green", `No change` = "lightgray", Loss = "dark orange")) +
  facet_wrap(. ~ variable,
    ncol = 1,
    scales = "free_y",
    strip.position = "left",
    labeller = labeller(variable = c(
      tmp = "Mean annual temperature (°C)",
      ppt = "Mean annual precipitation (mm)"
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
    data = obs_gainloss_tbl %>% filter(change == "gain" | change == "loss") %>% filter(!is.na(complete)),
    aes(
      x = tmp_occ_median,
      y = ppt_occ_median,
      size = dominance,
      color = change,
      fill = complete,
    ), alpha = 0.75, pch = 21
  ) +
  scale_color_manual(values = c(gain = "dark green", `no clear change` = "lightgray", loss = "dark orange")) +
  scale_fill_manual(values = c(recruited = "dark green", extirpated = "dark orange")) +
  labs(x = "Mean annual temperature (°C)", y = "Mean annual precipitation (mm)") +
  guides(
    fill = "none",
    size = "none",
    color = "none"
  )

obs_gainloss_main_3row_gg <- obs_gainloss_main_gg +
  facet_wrap(. ~ site,
    labeller = site_vec %>% as_labeller(),
    nrow = 3
  )

obs_gainloss_main_2row_gg <- obs_gainloss_main_gg +
  facet_wrap(. ~ site,
    labeller = site_vec %>% as_labeller(),
    nrow = 2
  )

obs_gainloss_supp_gg <- obs_gainloss_main_gg +
  ggrepel::geom_text_repel(
    data = obs_gainloss_tbl %>% filter(change == "gain" | change == "loss") %>% filter(!is.na(complete)),
    aes(
      x = tmp_occ_median,
      y = ppt_occ_median,
      color = change,
      label = paste0("italic('", species, "')")
    ),
    size = 3.88 / 1.68,
    alpha = 1,
    cex = 3,
    max.overlaps = 100,
    parse = T
  )

obs_gainloss_supp_3row_gg <-
  obs_gainloss_supp_gg +
  facet_wrap(. ~ site,
    labeller = site_vec %>% as_labeller(),
    nrow = 3
  )

obs_gainloss_supp_2row_gg <-
  obs_gainloss_supp_gg +
  facet_wrap(. ~ site,
    labeller = site_vec %>% as_labeller(),
    nrow = 2
  )

# experimental data -------------------------------------------------------
exp_gainloss_tbl <- read_rds(str_c(.path$sum_gainloss, "exp.rds"))

exp_gainloss_tbl %>%
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

wilcox.test(exp_gainloss_tbl_long %>% filter(variable == "tmp", change == "gain") %>% pull(value),
  exp_gainloss_tbl_long %>% filter(variable == "tmp", change == "loss") %>% pull(value),
  alternative = "two.sided"
)

wilcox.test(exp_gainloss_tbl_long %>% filter(variable == "ppt", change == "gain") %>% pull(value),
  exp_gainloss_tbl_long %>% filter(variable == "ppt", change == "loss") %>% pull(value),
  alternative = "two.sided"
)

exp_gainloss_summ_gg <-
  ggplot(exp_gainloss_tbl_long %>%
    mutate(change = case_when(
      change == "gain" ~ "Gain",
      change == "loss" ~ "Loss",
      change == "no clear change" ~ "No change"
    ))) +
  geom_boxplot(aes(x = change, col = change, y = value)) +
  scale_color_manual(values = c(Gain = "dark green", `No change` = "lightgray", Loss = "dark orange")) +
  facet_wrap(. ~ variable,
    ncol = 1,
    scales = "free_y",
    strip.position = "left",
    labeller = labeller(variable = c(
      tmp = "Mean annual temperature (°C)",
      ppt = "Mean annual precipitation (mm)"
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
    data = exp_gainloss_tbl %>% filter(change == "gain" | change == "loss") %>% filter(!is.na(complete)) %>% filter(year >= 2010),
    aes(
      x = tmp_occ_median,
      y = ppt_occ_median,
      size = dominance,
      color = change,
      fill = complete,
    ), alpha = 0.75, pch = 21
  ) +
  scale_color_manual(values = c(gain = "dark green", `no clear change` = "lightgray", loss = "dark orange")) +
  scale_fill_manual(values = c(recruited = "dark green", extirpated = "dark orange")) +
  labs(x = "Mean annual temperature (°C)", y = "Mean annual precipitation (mm)") +
  guides(
    fill = "none",
    size = "none",
    color = "none"
  ) +
  facet_wrap(. ~ phaseyear,
    nrow = 1
  )

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
    data = exp_gainloss_tbl %>% filter(change == "gain" | change == "loss") %>% filter(!is.na(complete)),
    aes(
      x = tmp_occ_median,
      y = ppt_occ_median,
      size = dominance,
      color = change,
      fill = complete,
    ), alpha = 0.75, pch = 21
  ) +
  scale_color_manual(values = c(gain = "dark green", `no clear change` = "lightgray", loss = "dark orange")) +
  scale_fill_manual(values = c(recruited = "dark green", extirpated = "dark orange")) +
  labs(x = "Mean annual temperature (°C)", y = "Mean annual precipitation (mm)") +
  guides(fill = "none") +
  facet_wrap(. ~ phaseyear,
    ncol = 3
  ) +
  ggrepel::geom_text_repel(
    data = exp_gainloss_tbl %>% filter(change != "no clear change"),
    aes(
      x = tmp_occ_median,
      y = ppt_occ_median,
      color = change,
      label = paste0("italic('", species, "')")
    ),
    size = 3.88 / 1.68,
    alpha = 1,
    max.overlaps = 100,
    parse = T
  )

# combine and save -------------------------------------------------------------
gainloss_main_gg <-
  obs_gainloss_main_3row_gg +
  obs_gainloss_summ_gg +
  exp_gainloss_main_gg +
  exp_gainloss_summ_gg +
  plot_layout(design = "
  AAAAB
  AAAAB
  AAAAD
  CCCCD
") +
  plot_annotation(tag_levels = "A")

# save main figure
if (.fig_save) {
  ggsave(
    plot = gainloss_main_gg,
    filename = str_c(.path$out_fig, "fig-main-gainloss.png"),
    width = 10,
    height = 11
  )
}

# save supplementary figures
if (.fig_save) {
  ggsave(
    plot = obs_gainloss_supp_3row_gg,
    filename = str_c(.path$out_fig, "fig-supp-gainloss-obs.png"),
    width = 10,
    height = 8
  )
  ggsave(
    plot = exp_gainloss_supp_gg,
    filename = str_c(.path$out_fig, "fig-supp-gainloss-exp.png"),
    width = 8,
    height = 8 * 1.618
  )
}

# save slide figures
if (.fig_save) {
  ggsave(
    plot = obs_gainloss_main_2row_gg,
    filename = str_c(.path$out_fig, "fig-slide-gainloss-obs.png"),
    width = 10,
    height = 10 * .5
  )
  ggsave(
    plot = obs_gainloss_supp_2row_gg,
    filename = str_c(.path$out_fig, "fig-slide-gainloss-obs-name.png"),
    width = 10,
    height = 10 * .5
  )
  ggsave(
    plot = exp_gainloss_main_gg,
    filename = str_c(.path$out_fig, "fig-slide-gainloss-exp.png"),
    width = 10,
    height = 10 * .5 * .618
  )
}
