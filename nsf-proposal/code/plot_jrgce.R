source("_setup.R")

jrgce_com <- read_rds("data/cfp_community.rds")$exp %>%
  filter(site == "jrgce", year >= 1999) %>%
  mutate(treat = str_sub(treat, start = 1L, end = 1L)) %>%
  select(year, plot, treat, species, abund)

jrgce_niche <- read_rds("data/cfp_niche.rds") %>%
  select(species, tmp = tmp_occ_median)

jrgce_cti <- inner_join(jrgce_com, jrgce_niche, by = "species") %>%
  group_by(year, treat, plot) %>%
  summarise(cti = sum(abund * tmp) / sum(abund)) %>%
  ungroup()

jrgce_npp <- read_csv("data/jrgce_biomass.csv", col_types = "iccd") %>%
  rename(year = yr, plot = plt, group = grp, bio = bio) %>%
  filter(year >= 1999) %>%
  pivot_wider(names_from = group, values_from = bio) %>%
  select(year, plot, anpp = ANPP)

jrgce_tbl <- inner_join(jrgce_cti, jrgce_npp, by = c("year", "plot")) %>%
  gather(key = "var", value = "value", -year, -treat, -plot) %>%
  mutate(var = factor(var, levels = c("cti", "anpp")))

jrgce_tbl_ambient <- jrgce_tbl %>%
  filter(treat == "_") %>%
  group_by(var, year) %>%
  summarise(ambient = mean(value))

jrgce_tbl_diff <- jrgce_tbl %>%
  filter(treat == "T") %>%
  left_join(jrgce_tbl_ambient, by = c("var", "year")) %>%
  mutate(value = value - ambient)

jrgce_tbl_diff_summ <- jrgce_tbl_diff %>%
  group_by(year, treat, var) %>%
  summarise(
    median = median(value),
    lower = quantile(value, 0.025),
    upper = quantile(value, 0.975)
  ) %>%
  ungroup()

warm_tbl <- tribble(
  ~tag, ~phase, ~start, ~end, ~startyear,
  1, "Phase I", -Inf, 2002, 1999,
  2, "Phase II", 2003, 2009, 2003,
  3, "Phase III", 2010, Inf, 2010
)

jrgce_p <-
  ggplot(jrgce_tbl_diff_summ %>%
    mutate(phase = case_when(
      year < 2003 ~ "Phase I",
      year < 2010 ~ "Phase II",
      TRUE ~ "Phase III"
    )) %>%
    mutate(label = factor(var,
      levels = c("cti", "anpp"),
      labels = c(
        expression(Delta ~ "CTI (" * degree * "C)"),
        expression(Delta ~ "ANPP (g m"^-2 * " y"^-1 * ")")
      )
    )) %>%
    filter(var == "cti")) +
  ggthemes::theme_few() +
  geom_rect( # warming phases
    data = warm_tbl,
    aes(xmin = start - .5, xmax = end + .5, fill = tag),
    ymin = -Inf, ymax = Inf, alpha = 0.5,
    show.legend = F
  ) +
  scale_fill_gradient(low = "antiquewhite", high = "orange") +
  geom_line(
    aes(x = year, y = median, group = phase)
  ) +
  geom_point(
    aes(x = year, y = median),
    size = 2
  ) +
  geom_errorbar(
    aes(x = year, ymin = lower, ymax = upper),
    width = 0
  ) +
  # scale_color_manual(values = c("black", "red")) +
  facet_wrap(
    ~label,
    ncol = 1, scales = "free_y",
    strip.position = "left",
    labeller = label_parsed
  ) +
  # scale_y_continuous(expand = expansion(mult = .1)) +
  scale_x_continuous(
    # expand = expansion(mult = 0, add = c(0.125, 0.125)),
    breaks = c(warm_tbl$start, warm_tbl$end, 1999, 2014) %>% unique() %>% .[abs(.) != Inf]
  ) +
  labs(
    x = NULL,
    y = NULL,
    color = "Warming treatment",
  ) +
  geom_text(
    data = warm_tbl %>%
      mutate(
        var = factor("cti",
          levels = c("cti", "anpp")
        )
      ) %>%
      mutate(label = factor(var,
        levels = c("cti", "anpp"),
        labels = c(
          expression(Delta ~ "CTI (" * degree * "C)"),
          expression(Delta ~ "ANPP (g m"^-2 * " y"^-1 * ")")
        )
      )) %>%
      filter(var == "cti"),
    aes(
      label = phase,
      x = startyear - 0.25,
    ),
    y = 1.2,
    hjust = 0,
  ) +
  coord_cartesian(clip = "off") +
  theme(
    strip.background = element_blank(),
    strip.placement = "outside",
    legend.position = "none",
    plot.margin = unit(c(2, 1, 1, 1), "lines")
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

ggsave(
  plot = jrgce_p,
  filename = "nsf-proposal/figures/jrgce.pdf",
  width = 6,
  height = 4,
  device = pdf
)
