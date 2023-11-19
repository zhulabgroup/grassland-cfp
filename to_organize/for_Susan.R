df <- read_rds("/nfs/turbo/seas-zhukai/grassland/community/gainloss/obs.rds")
df %>%
  filter(site == "mclann") %>%
  filter(change != "no clear change") %>%
  group_by(change) %>%
  arrange(change, desc(dominance)) %>%
  filter(dominance >= 0.05) %>%
  ungroup()

p <- ggplot() +
  geom_point(
    data = df %>%
      filter(site == "mclann"),
    aes(
      x = tmp_occ_median,
      y = ppt_occ_median
    ), color = "gray", alpha = 0.1
  ) +
  geom_point(
    data = df %>%
      filter(site == "mclann"),
    aes(
      x = tmp_occ_median,
      y = ppt_occ_median,
      size = dominance,
      color = change
    ), alpha = 1, pch = 21, fill = NA
  ) +
  geom_point(
    data = df %>%
      filter(site == "mclann") %>%
      filter(change == "increase" | change == "decrease") %>% filter(!is.na(complete_change)),
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
  labs(x = "Mean annual temperature (°C)", y = "Annual precipitation (mm)") +
  guides(
    fill = "none",
    size = "none",
    color = "none"
  ) +
  theme(axis.text = element_text(size = 8)) +
  ggrepel::geom_text_repel(
    data = df %>%
      filter(site == "mclann") %>%
      filter(change != "no clear change"),
    aes(
      x = tmp_occ_median,
      y = ppt_occ_median,
      color = change,
      label = paste0("italic('", species, "')")
    ),
    size = 3.88 / 1.68 * 1.5,
    alpha = 1,
    max.overlaps = 100,
    parse = T
  ) +
  theme_classic()

ggsave(
  plot = p,
  filename = str_c("/nfs/turbo/seas-zhukai/grassland/output/manuscript/for-Susan.png"),
  width = 10,
  height = 10,
  device = png, type = "cairo"
)
