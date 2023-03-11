# replot CTI-CPI for Elkhorn ----------------------------------------------
source("scripts/plot-main-obs.R")
elkhorn_gg_cwm <- plot_cwm(obs_idx_tbl, "elkhorn", cti_lab = "CTI (°C)", cpi_lab = "CPI (mm)", yr_axis = TRUE)

# plot gainloss for Elkhorn -----------------------------------------------
niche_tbl <- read_rds(.path$sum_niche) %>%
  filter(occ_n > 100 | is.na(occ_n)) %>% # species with many observations and dummy species
  select(species, tmp_occ_median, ppt_occ_median)

elkhorn_tbl <- read_rds(str_c(.path$sum_gainloss, "obs.rds")) %>%
  filter(site == "elkhorn")

elkhorn_gg_blank <-
  ggplot() +
  geom_point(
    data = niche_tbl,
    aes(
      x = tmp_occ_median,
      y = ppt_occ_median
    ), color = "gray", alpha = 0
  ) +
  labs(x = "Mean annual temperature (°C)", y = "Mean annual precipitation (mm)") +
  guides(
    fill = "none",
    size = "none",
    color = "none"
  )

elkhorn_gg_allspp <-
  elkhorn_gg_blank +
  geom_point(
    data = elkhorn_tbl,
    aes(
      x = tmp_occ_median,
      y = ppt_occ_median,
      size = dominance
    ), alpha = 1, color = "gray", fill = NA, pch = 21
  )

elkhorn_gg_notannotated <-
  elkhorn_gg_blank +
  geom_point(
    data = elkhorn_tbl,
    aes(
      x = tmp_occ_median,
      y = ppt_occ_median,
      size = dominance,
      color = change
    ), alpha = 1, pch = 21, fill = NA
  ) +
  geom_point(
    data = elkhorn_tbl %>% filter(change == "gain" | change == "loss") %>% filter(!is.na(complete)),
    aes(
      x = tmp_occ_median,
      y = ppt_occ_median,
      size = dominance,
      color = change,
      fill = complete,
    ), alpha = 0.75, pch = 21
  )

elkhorn_gg_gain <-
  elkhorn_gg_notannotated +
  scale_color_manual(values = c(gain = "dark green", `no clear change` = "lightgray", loss = "lightgray")) +
  scale_fill_manual(values = c(recruited = "00FFFFFF", extirpated = "00FFFFFF"))

elkhorn_gg_gain_recruited <-
  elkhorn_gg_notannotated +
  scale_color_manual(values = c(gain = "dark green", `no clear change` = "lightgray", loss = "lightgray")) +
  scale_fill_manual(values = c(recruited = "dark green", extirpated = "00FFFFFF"))

elkhorn_gg_gainloss_recruited <-
  elkhorn_gg_notannotated +
  scale_color_manual(values = c(gain = "dark green", `no clear change` = "lightgray", loss = "dark orange")) +
  scale_fill_manual(values = c(recruited = "dark green", extirpated = "00FFFFFF"))

elkhorn_gg_gainloss_recruitedextirpated <-
  elkhorn_gg_notannotated +
  scale_color_manual(values = c(gain = "dark green", `no clear change` = "lightgray", loss = "dark orange")) +
  scale_fill_manual(values = c(recruited = "dark green", extirpated = "dark orange"))

# save slide figures
if (.fig_save) {
  ggsave(
    plot = elkhorn_gg_cwm,
    filename = str_c(.path$out_fig, "fig-slide-elkhorn0-cwm.png"),
    width = 4,
    height = 4 * 1.618
  )
  ggsave(
    plot = elkhorn_gg_allspp,
    filename = str_c(.path$out_fig, "fig-slide-elkhorn1-allspp.png"),
    width = 4,
    height = 4 * 1.618
  )
  ggsave(
    plot = elkhorn_gg_gain,
    filename = str_c(.path$out_fig, "fig-slide-elkhorn2-gain.png"),
    width = 4,
    height = 4 * 1.618
  )
  ggsave(
    plot = elkhorn_gg_gain_recruited,
    filename = str_c(.path$out_fig, "fig-slide-elkhorn3-gain-recruited.png"),
    width = 4,
    height = 4 * 1.618
  )
  ggsave(
    plot = elkhorn_gg_gainloss_recruited,
    filename = str_c(.path$out_fig, "fig-slide-elkhorn4-gainloss-recruited.png"),
    width = 4,
    height = 4 * 1.618
  )
  ggsave(
    plot = elkhorn_gg_gainloss_recruitedextirpated,
    filename = str_c(.path$out_fig, "fig-slide-elkhorn5-gainloss-recruitedextirpated.png"),
    width = 4,
    height = 4 * 1.618
  )
}
