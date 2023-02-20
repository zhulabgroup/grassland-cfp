# compare Jasper Ridge serpentine observation vs. annual experiment (ambient) CTI vs. temperature over years
source("scripts/report-stats.R")

jr_obs_tbl <-
  bind_rows(
    obs_tbl %>%
      filter(site == "jasper") %>%
      group_by(year) %>%
      summarize(cti = mean(tmp_com_mean)) %>%
      ungroup() %>%
      mutate(type = "obs", variable = "cti") %>%
      select(type, year, variable, value = cti),
    read_rds(.path$cli_chelsa_annual) %>%
      filter(abbr == "jasper", year >= 1983, year <= 2015) %>%
      mutate(type = "obs", variable = "tmp") %>% # tmp = CHELSA annual tmp
      select(type, year, variable, value = tmp)
  )

jr_exp_tbl <-
  bind_rows(
    exp_cti_tbl %>%
      mutate(type = "exp_amb", variable = "cti") %>%
      select(type, year = Year, variable, value = Ambient),
    jrgce_avgt_tbl %>%
      filter(harvest_year >= 1999, harvest_year <= 2014) %>%
      mutate(type = "exp_amb", variable = "tmp") %>% # tmp = AVGT308_126
      select(type, year = harvest_year, variable, value = amb)
  )

jr_obs_gg <- ggplot(jr_obs_tbl, aes(year, value)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = NULL, y = NULL) +
  facet_wrap(~variable,
    ncol = 1, scales = "free_y",
    strip.position = "left",
    labeller = c(
      cti = "Community Temperature Index (CTI, °C)",
      tmp = "Air temperature (°C)"
    ) %>%
      as_labeller()
  ) +
  theme(
    strip.background = element_blank(),
    strip.placement = "outside"
  ) +
  ggtitle("Observed serpentine grassland")

jr_exp_gg <- ggplot(jr_exp_tbl, aes(year, value)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = NULL, y = NULL) +
  facet_wrap(~variable,
    ncol = 1, scales = "free_y"
  ) +
  theme(
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_blank()
  ) +
  ggtitle("Experimental (ambient) annual grassland")

jrcomp_gg <- jr_obs_gg + jr_exp_gg

# save figure file
if (.fig_save) {
  ggsave(
    plot = jrcomp_gg,
    filename = str_c(.path$out_fig, "fig-supp-jrcomp2.png"),
    width = 10,
    height = 6.18
  )
}
