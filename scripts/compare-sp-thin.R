# read original and thinned niche tables
niche_tbl <- read_rds(.path$sum_niche) %>%
  filter(occ_n > 100) %>%
  select(species, occ_n)

thin_tbl <- read_rds(.path$sum_thin) %>%
  inner_join(niche_tbl, ., by = "species")

thin_tmp_gg <-
  ggplot(thin_tbl, aes(full_median_tmp, thin_median_tmp)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm", formula = y ~ x - 1, se = FALSE, color = "red", lty = "dashed") +
  ggpubr::stat_cor(
    aes(label = paste(..r.label.., ..p.label.., sep = "*`,`~")),
    p.accuracy = 0.05,
    color = "red"
  ) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(x = "Full data median temperature (°C)", y = "Thinned data median temperature (°C)") +
  theme(
    aspect.ratio = 1,
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

thin_ppt_gg <-
  ggplot(thin_tbl, aes(full_median_ppt, thin_median_ppt)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm", formula = y ~ x - 1, se = FALSE, color = "red", lty = "dashed") +
  ggpubr::stat_cor(
    aes(label = paste(..r.label.., ..p.label.., sep = "*`,`~")),
    p.accuracy = 0.05,
    color = "red"
  ) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(x = "Full data median precipitation (mm)", y = "Thinned data median precipitation (mm)") +
  theme(
    aspect.ratio = 1,
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

thin_comp_gg <- thin_tmp_gg + thin_ppt_gg

# save figure
if (.fig_save) {
  ggsave(
    plot = thin_comp_gg,
    filename = str_c(.path$out_fig, "fig-supp-thin-comp.pdf"),
    width = 10,
    height = 7
  )
}
