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
  vascocaves = "Vasco Caves",
  jrgce = "Jasper Ridge\nGlobal Change Experiment",
  mclexp = "McLaughlin Water Experiment",
  scide = "Santa Cruz\nInternational Drought Experiment"
)

data_avail_tbl <- read_rds(.path$com_obs) %>%
  group_by(site) %>%
  summarise(nyear = unique(year) %>% length()) %>%
  arrange(nyear)

data_avail <- bind_rows(
  read_rds(.path$com_exp) %>%
    distinct(site, plot, year),
  read_rds(.path$com_obs) %>%
    distinct(site, plot, year)
) %>%
  rowwise() %>%
  mutate(site = str_split(site, "_", simplify = T)[1]) %>%
  ungroup() %>%
  filter(!(site == "jrgce" & year == 1998)) %>% # pre-treatment
  filter(!(site == "scide" & year == 2015)) %>% # pre-treatment
  mutate(sitename = site_vec[site]) %>%
  filter(!is.na(sitename)) %>%
  group_by(sitename, year) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(count) %>%
  mutate(sitename = factor(sitename, levels = site_vec))

data_avail_gg <- ggplot(data_avail) +
  geom_segment(
    data = data_avail %>%
      group_by(sitename) %>%
      summarise(min = min(year), max = max(year)),
    aes(x = min, xend = max, y = sitename, yend = sitename), linewidth = 3, col = "dark green", alpha = 0.4
  ) +
  geom_point(data = data_avail, aes(x = year, y = sitename, cex = count), pch = 19) +
  scale_size("Sample size", range = c(2, 4), breaks = c(10, 50, 100)) +
  xlab("") +
  ylab("") +
  scale_y_discrete(limits = rev)

# save figure
if (.fig_save) {
  ggsave(
    plot = data_avail_gg,
    filename = str_c(.path$out_fig, "fig-supp-data-avail.png"),
    width = 10,
    height = 10 * .618,
    device = png, type = "cairo"
  )
}
