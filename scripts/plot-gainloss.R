niche_tbl <- read_rds(.path$sum_niche) %>%
  filter(occ_n > 100 | is.na(occ_n)) %>% # species with many observations and dummy species
  select(species, tmp_occ_median, ppt_occ_median)

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

read_rds(.path$com_obs) %>%
  filter(site == "morganterritory") %>%
  slice(126, 127)

obs_gainloss_tbl_list <- vector(mode = "list")
for (siteoi in names(site_vec)) {
  obs_gainloss_tbl_list[[siteoi]] <- read_rds(.path$com_obs) %>%
    filter(site == siteoi) %>%
    group_by(year, plot, species) %>%
    summarise(abund = sum(abund)) %>% # doing this because of duplicated records. not the ideal solution.
    ungroup() %>%
    spread(key = "species", value = "abund") %>%
    mutate_if(is.numeric, ~ replace_na(., 0)) %>%
    gather(key = "species", value = "abund", -year, -plot) %>%
    group_by(species) %>%
    nest() %>%
    mutate(
      map(data, ~ lm(abund ~ year, data = .)) %>%
        map_df(~ broom::tidy(.) %>%
          filter(term == "year") %>%
          select(estimate, p.value)),
    ) %>%
    unnest(cols = data) %>%
    distinct(species, estimate, p.value) %>%
    mutate(change = case_when(
      (estimate > 0 & p.value <= 0.05) ~ "gain",
      (estimate < 0 & p.value <= 0.05) ~ "loss",
      TRUE ~ "no clear change"
    )) %>%
    left_join(niche_tbl, by = "species") %>%
    mutate(site = siteoi)
}
obs_gainloss_tbl <- bind_rows(obs_gainloss_tbl_list)

gainloss_gg <-
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
      color = change,
      size = estimate %>% abs() %>% sqrt()
    ), alpha = 0.5
  ) +
  scale_color_manual(values = c(gain = "dark green", `no clear change` = "gray", loss = "dark orange")) +
  labs(x = "Mean annual temperature (°C)", y = "Mean annual precipitation (mm)") +
  guides(size = "none") +
  facet_wrap(. ~ site,
    labeller = site_vec %>% as_labeller(),
    nrow = 3
  )

# save figure file
if (.fig_save) {
  ggsave(
    plot = gainloss_gg,
    filename = str_c(.path$out_fig, "fig-supp-gainloss-obs.png"),
    width = 12,
    height = 10
  )
}
