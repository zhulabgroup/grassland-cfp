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

warm_tbl <- tribble(
  ~tag, ~phase, ~start, ~end, ~startyear,
  1, "Phase I", -Inf, 2002, 1999,
  2, "Phase II", 2003, 2009, 2003,
  3, "Phase III", 2010, Inf, 2010
)

jrgce_p <-
  ggplot(jrgce_tbl) +
  ggthemes::theme_few() +
  geom_rect( # warming phases
    data = warm_tbl,
    aes(xmin = start - .5, xmax = end + .5, fill = tag),
    ymin = -Inf, ymax = Inf, alpha = 0.5,
    show.legend = F
  ) +
  scale_fill_gradient(low = "antiquewhite", high = "orange") +
  geom_boxplot( # treatment effects
    aes(x = year, y = value, col = treat, group = interaction(treat, year))
  ) +
  scale_color_manual(values = c("black", "red")) +
  facet_wrap(
    ~var,
    ncol = 1, scales = "free_y",
    strip.position = "left",
    labeller = labeller(var = c(
      cti = "CTI",
      anpp = "ANPP"
    ))
  ) +
  scale_y_continuous(expand = expansion(mult = .1)) +
  scale_x_continuous(
    expand = expansion(mult = 0, add = c(0.125, 0.125)),
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
      ),
    aes(
      label = phase,
      x = startyear - 0.25,
    ),
    y = 17,
    hjust = 0,
  ) +
  coord_cartesian(clip = "off") +
  theme(
    strip.background = element_blank(),
    strip.placement = "outside",
    legend.position = "none",
    plot.margin = unit(c(2, 1, 1, 1), "lines")
  )

ggsave(
  plot = jrgce_p,
  filename = "nsf-proposal/figures/jrgce.png",
  width = 10,
  height = 6.18,
  device = png, type = "cairo"
)
