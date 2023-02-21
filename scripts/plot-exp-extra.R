# JRGCE
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
    filename = str_c(.path$out_fig, "fig-suppl-exp-water.png"),
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
names(n_plt) <- c("ambient", "warming")

# McLaughlin Water Experiment
# filter mclexp data
mclexp_tbl <- read_rds(.path$com_exp) %>%
  filter(site == "mclexp") %>%
  inner_join(niche_tbl, by = "species") %>%
  group_by(site, year, plot, treat) %>%
  summarize(
    tmp_com_mean = sum(abund * tmp_occ_median) / sum(abund),
    ppt_com_mean = sum(abund * ppt_occ_median) / sum(abund)
  ) %>%
  separate(treat, c("treat", "soil"), sep = 2) %>%
  pivot_longer(cols = tmp_com_mean:ppt_com_mean, names_to = "com_idx_name", values_to = "com_idx_value") %>%
  mutate(com_idx_name = factor(com_idx_name,
    levels = c("tmp_com_mean", "ppt_com_mean"),
    labels = c("CTI", "CPI")
  ))

plot_mclexp <- function(trt_tag = "Watering", s_tag = "Serpentine soil") {
  # filter data by treatment tag
  if (trt_tag == "Watering") {
    mclexp_tbl <- filter(mclexp_tbl, treat %in% c("_X", "WX"))
    box_col <- "blue"
    bg_col <- "#F0F8FF"
  } else if (trt_tag == "Drought") {
    mclexp_tbl <- filter(mclexp_tbl, treat %in% c("X_", "XD"))
    box_col <- "brown"
    bg_col <- "#FFFFE0"
  } else {
    stop("Unknown treatment tag")
  }

  # filter data by soil tag
  if (s_tag == "Serpentine soil") {
    mclexp_tbl <- filter(mclexp_tbl, soil == "S")
  } else if (s_tag == "Non-serpentine soil") {
    mclexp_tbl <- filter(mclexp_tbl, soil == "N")
  } else {
    stop("Unknown soil tag")
  }

  p <-
    ggplot(mclexp_tbl) +
    geom_boxplot( # treatment effects
      aes(x = year, y = com_idx_value, col = treat, group = interaction(treat, year))
    ) +
    scale_color_manual(values = c("black", box_col)) +
    ggpubr::stat_compare_means( # significance
      aes(x = year, y = com_idx_value, group = treat),
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
    ) +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside",
      legend.position = "none",
      plot.margin = unit(c(2, 1, 1, 1), "lines"), # expand margin to include warming labels
      panel.background = element_rect(fill = bg_col)
    ) +
    ggtitle(paste0(trt_tag, " treatment\n(", s_tag, ")"))

  return(p)
}

mclexp_gg <-
  plot_mclexp(trt_tag = "Watering", s_tag = "Serpentine soil") +
  plot_mclexp(trt_tag = "Watering", s_tag = "Non-serpentine soil") +
  plot_mclexp(trt_tag = "Drought", s_tag = "Serpentine soil") +
  plot_layout(design = "
  ABC
") +
  plot_annotation(tag_levels = "A")

# save figure file
if (.fig_save) {
  ggsave(
    plot = mclexp_gg,
    filename = str_c(.path$out_fig, "fig-supp-exp-mclexp.png"),
    width = 11,
    height = 8
  )
}

# Santa Cruz IDE
# filter data
scide_tbl <- read_rds(.path$com_exp) %>%
  filter(str_detect(site, "scide")) %>%
  inner_join(niche_tbl, by = "species") %>%
  group_by(site, year, plot, treat) %>%
  summarize(
    tmp_com_mean = sum(abund * tmp_occ_median) / sum(abund),
    ppt_com_mean = sum(abund * ppt_occ_median) / sum(abund)
  ) %>%
  pivot_longer(cols = tmp_com_mean:ppt_com_mean, names_to = "com_idx_name", values_to = "com_idx_value") %>%
  mutate(com_idx_name = factor(com_idx_name,
    levels = c("tmp_com_mean", "ppt_com_mean"),
    labels = c("CTI", "CPI")
  ))

plot_mclexp <- function(site_tag = "Arboretum") {
  # filter data by site tag
  if (site_tag == "Arboretum") {
    scide_tbl <- scide_tbl %>%
      filter(str_detect(site, "arboretum"))
  } else if (site_tag == "Marshall Field") {
    scide_tbl <- scide_tbl %>%
      filter(str_detect(site, "marshallfield"))
  } else if (site_tag == "Younger Lagoon") {
    scide_tbl <- scide_tbl %>%
      filter(str_detect(site, "ylr"))
  } else {
    stop("Unknown site tag")
  }

  box_col <- "brown"
  bg_col <- "#FFFFE0"

  p <-
    ggplot(scide_tbl) +
    geom_boxplot( # treatment effects
      aes(x = year, y = com_idx_value, col = treat, group = interaction(treat, year))
    ) +
    scale_color_manual(values = c("black", box_col)) +
    ggpubr::stat_compare_means( # significance
      aes(x = year, y = com_idx_value, group = treat),
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
    ) +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside",
      legend.position = "none",
      plot.margin = unit(c(2, 1, 1, 1), "lines"), # expand margin to include warming labels
      panel.background = element_rect(fill = bg_col)
    ) +
    ggtitle(site_tag)

  return(p)
}

scide_gg <-
  plot_mclexp(site_tag = "Arboretum") +
  plot_mclexp(site_tag = "Marshall Field") +
  plot_mclexp(site_tag = "Younger Lagoon") +
  plot_layout(design = "
  ABC
") +
  plot_annotation(tag_levels = "A")

# save figure file
if (.fig_save) {
  ggsave(
    plot = scide_gg,
    filename = str_c(.path$out_fig, "fig-supp-exp-scide.png"),
    width = 11,
    height = 8
  )
}
