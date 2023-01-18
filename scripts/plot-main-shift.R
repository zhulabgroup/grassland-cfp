
### experiment plot
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
  mutate(treat_T = str_sub(treat, start = 1L, end = 1L)) %>%
  select(site, year, plot, treat_T, tmp_com_mean, ppt_com_mean) %>%
  pivot_longer(cols = tmp_com_mean:ppt_com_mean, names_to = "com_idx_name", values_to = "com_idx_value") %>%
  mutate(com_idx_name = factor(com_idx_name,
    levels = c("tmp_com_mean", "ppt_com_mean"),
    labels = c("CTI", "CPI")
  ))

# warming phrases: +80 W m2 (years 25), to +100 W m2 (years 612), to +250 W m2 (years 1317)
warm_tbl <- tribble(
  ~tag, ~name, ~start, ~end,
  1, "Phase I", -Inf, 2002,
  2, "Phase II", 2003, 2009,
  3, "Phase III", 2010, Inf # end in 2014, but set to Inf to fill space
)

warm_vec <- c(
  `Phase I` = "Phase~I:~+80~W~m^-2",
  `Phase II` = "Phase~II:~+100~W~m^-2",
  `Phase III` = "Phase~III:~+250~W~m^-2"
)

# plot in niche space
df_exp_sum <- jrgce_tbl %>%
  mutate(phase = case_when(
    year <= 2002 ~ "Phase I",
    year >= 2010 ~ "Phase III",
    TRUE ~ "Phase II"
  )) %>%
  group_by(site, phase, year, treat_T, com_idx_name) %>%
  summarise(
    m = median(com_idx_value),
    se = sd(com_idx_value) / sqrt(n())
  ) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c("site", "phase", "year", "treat_T"),
    names_from = com_idx_name,
    values_from = c("m", "se")
  ) %>%
  mutate(treatment = case_when(
    treat_T == "_" ~ "ambient",
    treat_T == "T" ~ "warming"
  ))

df_exp_test <- jrgce_tbl %>%
  group_by(site, year, com_idx_name) %>%
  nest() %>%
  mutate(p = map_dbl(data, ~ wilcox.test(com_idx_value ~ treat_T, data = .)$p.value)) %>% # get p value for wilcoxon test
  select(-data) %>%
  pivot_wider(
    id_cols = c("site", "year"),
    names_from = com_idx_name,
    values_from = p
  ) %>%
  mutate(significance = case_when(
    CTI < 0.05 & CPI < 0.05 ~ "sig",
    # CTI >= 0.05 & CPI >= 0.05 ~ "?",
    TRUE ~ "ns"
  )) %>%
  select(-CTI, -CPI)

df_exp_shift <- jrgce_tbl %>%
  mutate(phase = case_when(
    year <= 2002 ~ "Phase I",
    year >= 2010 ~ "Phase III",
    TRUE ~ "Phase II"
  )) %>%
  group_by(site, phase, year, treat_T, com_idx_name) %>%
  summarise(m = median(com_idx_value)) %>%
  ungroup() %>%
  mutate(group_metric = paste0(com_idx_name, treat_T)) %>%
  select(-com_idx_name, -treat_T) %>%
  pivot_wider(
    id_cols = c("site", "phase", "year"),
    names_from = group_metric,
    values_from = m
  ) %>%
  left_join(df_exp_test, by = c("site", "year"))

p_exp <- ggplot() +
  geom_rect( # warming phrases
    data = warm_tbl %>% rename(phase = name),
    aes(fill = tag),
    xmin = -Inf, xmax = Inf,
    ymin = -Inf, ymax = Inf,
    alpha = 0.5
  ) +
  scale_fill_gradient(low = "antiquewhite", high = "orange") +
  geom_point(
    data = df_exp_sum,
    aes(x = m_CTI, y = m_CPI, col = treatment)
  ) +
  scale_color_manual(values = c("ambient" = "black", "warming" = "red")) +
  geom_segment(
    data = df_exp_shift,
    aes(
      x = CTI_, xend = CTIT, y = CPI_, yend = CPIT,
      group = phase,
      alpha = significance
    ),
    arrow = arrow(length = unit(0.2, "cm")),
    linewidth = 0.8
  ) +
  scale_alpha_manual(values = c("sig" = 1, "ns" = 0.25)) +
  facet_wrap(. ~ phase,
    nrow = 1,
    labeller = warm_vec %>% as_labeller(label_parsed)
  ) +
  # ggtitle(warm_vec[1])+
  xlab("CTI (°C)") +
  ylab("CPI (mm)") +
  guides(
    fill = "none"
  ) +
  theme(strip.text.x = element_text(hjust = 0)) +
  theme(legend.position = "bottom")


### observation plot

# import niche and observational data, calculate CTI and CPI
niche_tbl <- read_rds(.path$sum_niche) %>%
  filter(occ_n > 100 | is.na(occ_n)) # species with many observations and dummy species

obs_tbl <- read_rds(.path$com_obs) %>%
  inner_join(niche_tbl, by = "species") %>%
  group_by(site, year, plot) %>%
  summarize(
    tmp_com_mean = sum(abund * tmp_occ_median) / sum(abund),
    ppt_com_mean = sum(abund * ppt_occ_median) / sum(abund)
  )

# setup site labels
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

# reshape data
obs_idx_tbl <- obs_tbl %>%
  dplyr::select(site, year, plot, tmp_com_mean, ppt_com_mean) %>%
  pivot_longer(cols = tmp_com_mean:ppt_com_mean, names_to = "com_idx_name", values_to = "com_idx_value") %>%
  mutate(com_idx_name = factor(com_idx_name,
    levels = c("tmp_com_mean", "ppt_com_mean"),
    labels = c("CTI", "CPI")
  ))


# plot in niche space
df_obs_sum <- obs_idx_tbl %>%
  group_by(site, year, com_idx_name) %>%
  summarise(
    m = median(com_idx_value),
    se = sd(com_idx_value) / sqrt(n())
  ) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c("site", "year"),
    names_from = com_idx_name,
    values_from = c("m", "se")
  )

df_obs_test <- obs_idx_tbl %>%
  group_by(site, com_idx_name) %>%
  nest() %>%
  mutate(
    map(data, ~ lm(com_idx_value ~ year, data = .)) %>%
      map_df(~ broom::tidy(.) %>%
        filter(term == "year") %>%
        select(p.value)) # ,
  ) %>%
  select(-data) %>%
  ungroup() %>%
  rename(p = p.value) %>%
  pivot_wider(
    id_cols = site,
    names_from = com_idx_name,
    values_from = p
  ) %>%
  mutate(significance = case_when(
    CTI < 0.05 & CPI < 0.05 ~ "sig",
    # CTI >= 0.05 & CPI >= 0.05 ~ "?",
    TRUE ~ "ns"
  )) %>%
  select(-CTI, -CPI)

df_obs_shift <- obs_idx_tbl %>%
  group_by(site, com_idx_name) %>%
  do(broom::augment(lm(com_idx_value ~ year, data = .))) %>%
  select(site, com_idx_name, year, fitted = .fitted) %>%
  filter(year == min(year) | year == max(year)) %>%
  mutate(year = case_when(
    year == min(year) ~ "start",
    year == max(year) ~ "end"
  )) %>%
  ungroup() %>%
  distinct() %>%
  pivot_wider(
    id_cols = c("site", "year"),
    names_from = com_idx_name,
    values_from = fitted
  ) %>%
  pivot_wider(
    id_cols = site,
    names_from = year,
    values_from = c("CTI", "CPI")
  ) %>%
  left_join(df_obs_test, by = "site")

p_obs <- ggplot() +
  geom_point(
    data = df_obs_sum,
    aes(x = m_CTI, y = m_CPI, col = year),
    alpha = 0.6
  ) +
  geom_segment(
    data = df_obs_shift,
    aes(
      x = CTI_start, xend = CTI_end, y = CPI_start, yend = CPI_end,
      alpha = significance
    ),
    arrow = arrow(length = unit(0.2, "cm")),
    linewidth = 0.8
  ) +
  scale_alpha_manual(values = c("sig" = 1, "ns" = 0.5)) +
  scale_color_viridis_c(
    option = "magma",
    # , trans = "reverse", direction = -1
  ) +
  facet_wrap(. ~ site, labeller = site_vec %>% as_labeller()) +
  xlab("CTI (°C)") +
  ylab("CPI (mm)") +
  theme(strip.text.x = element_text(hjust = 0)) +
  theme(legend.position = "bottom") +
  guides(color = guide_colorbar(barwidth = 10))

### compare exp and obs
df_all_shift <- bind_rows(
  df_obs_shift %>%
    select(site, CTI0 = CTI_start, CTI1 = CTI_end, CPI0 = CPI_start, CPI1 = CPI_end, significance) %>%
    mutate(group = "observation"),
  df_exp_shift %>%
    # filter(phase == "Phase III") %>%
    mutate(site = paste(site, year, sep = "_")) %>%
    select(site, CTI0 = CTI_, CTI1 = CTIT, CPI0 = CPI_, CPI1 = CPIT, significance) %>%
    mutate(group = "experiment")
)

df_all_shift %>%
  filter(significance == "sig") %>%
  mutate(slope = (CPI1 - CPI0) / (CTI1 - CTI0)) %>%
  group_by(group) %>%
  summarise(
    mean = mean(slope),
    se = sd(slope) / sqrt(n())
  ) %>%
  ungroup()

df_all_shift %>%
  filter(significance == "sig") %>%
  mutate(slope = (CTI1 - CTI0) / (CPI1 - CPI0)) %>%
  group_by(group) %>%
  summarise(
    mean = mean(slope),
    se = sd(slope) / sqrt(n())
  ) %>%
  ungroup()

p_compare <- ggplot(df_all_shift) +
  geom_segment(
    aes(
      x = CTI0, xend = CTI1, y = CPI0, yend = CPI1,
      group = site, col = group,
      alpha = significance
    ),
    arrow = arrow(length = unit(0.2, "cm")),
    linewidth = 1
  ) +
  scale_color_manual(values = c("experiment" = "#e28a2b", "observation" = "#384c6b")) +
  scale_alpha_manual(values = c("sig2" = 1, "sig1" = 0.25, "ns" = 0.25)) +
  guides(alpha = "none") +
  xlab("Community Temperature Index (CTI, °C)") +
  ylab("Community Precipitation Index (CPI, mm)") +
  theme(
    legend.title = element_blank(),
    legend.position = c(.2, .2),
    legend.text = element_text(size = rel(0.8))
  ) +
  geom_rect(
    aes(
      xmin = layer_scales(p_exp)$x$range$range[1],
      xmax = layer_scales(p_exp)$x$range$range[2],
      ymin = layer_scales(p_exp)$y$range$range[1],
      ymax = layer_scales(p_exp)$y$range$range[2]
    ),
    fill = NA,
    linetype = 3,
    color = "orange"
  ) +
  geom_text(
    aes(
      x = layer_scales(p_exp)$x$range$range[2] + 0.025,
      y = layer_scales(p_exp)$y$range$range[2] + 0.05
    ),
    label = "C",
    color = "orange"
  )

### plot all sites in niche space
# import data
niche_tbl <- read_rds(.path$sum_niche) %>%
  filter(occ_n > 100) # no dummy species

gbif_chelsa_sf <- read_rds(.path$geo_clim) %>%
  select(geometry, key, species, tmp = chelsa_tmp, ppt = chelsa_ppt, vpd = chelsa_vpd) %>%
  filter(species %in% niche_tbl$species) %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")

df_exp_all_sum <- jrgce_tbl %>%
  mutate(phase = case_when(
    year <= 2002 ~ "Phase I",
    year >= 2010 ~ "Phase III",
    TRUE ~ "Phase II"
  )) %>%
  group_by(site, phase, com_idx_name) %>%
  summarise(
    m = median(com_idx_value),
    se = sd(com_idx_value) / sqrt(n())
  ) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c("site", "phase"),
    names_from = com_idx_name,
    values_from = c("m", "se")
  ) %>%
  select(-phase)

df_obs_all_sum <- obs_idx_tbl %>%
  group_by(site, com_idx_name) %>%
  summarise(
    m = median(com_idx_value),
    se = sd(com_idx_value) / sqrt(n())
  ) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = site,
    names_from = com_idx_name,
    values_from = c("m", "se")
  )

df_all_sum <- bind_rows(
  df_obs_all_sum %>% mutate(group = "observation"),
  df_exp_all_sum %>% mutate(group = "experiment")
)

p_niche <- ggplot() +
  geom_point(
    data = niche_tbl,
    aes(
      x = tmp_occ_median,
      y = ppt_occ_median # ,
      # xmin = tmp_occ_median - tmp_occ_sd / sqrt(occ_n),
      # xmax = tmp_occ_median + tmp_occ_sd / sqrt(occ_n),
      # ymin = ppt_occ_median - ppt_occ_sd / sqrt(occ_n),
      # ymax = ppt_occ_median + ppt_occ_sd / sqrt(occ_n)
    ),
    col = gray(.75),
    alpha = 0.5
  ) +
  geom_point(
    data = df_all_sum,
    aes(x = m_CTI, y = m_CPI, col = group),
    size = 2.5, alpha = 0.75
  ) +
  scale_color_manual(values = c("experiment" = "#e28a2b", "observation" = "#384c6b")) +
  # geom_errorbarh(data=df_all_sum,
  #               aes(y=m_CPI,xmin=m_CTI-se_CTI, xmax=m_CTI+se_CTI, col=group)) +
  # geom_errorbar(data=df_all_sum,
  #                aes(x=m_CTI,ymin=m_CPI-se_CPI, ymax=m_CPI+se_CPI, col=group)) +
  labs(x = "Mean annual temperature (°C)", y = "Mean annual precipitation (mm)") +
  theme(
    legend.title = element_blank(),
    legend.position = c(.2, .2),
    legend.text = element_text(size = rel(0.8))
  ) +
  geom_rect(
    aes(
      xmin = layer_scales(p_compare)$x$range$range[1],
      xmax = layer_scales(p_compare)$x$range$range[2],
      ymin = layer_scales(p_compare)$y$range$range[1],
      ymax = layer_scales(p_compare)$y$range$range[2]
    ),
    fill = NA,
    linetype = 3,
    color = "black"
  ) +
  geom_text(
    aes(
      x = layer_scales(p_compare)$x$range$range[2] + 0.1,
      y = layer_scales(p_compare)$y$range$range[2] + 0.05
    ),
    label = "B",
    color = "black"
  )

### combine panels
shift_gg <- p_niche + p_compare + p_exp + p_obs +
  plot_annotation(tag_levels = "A") +
  plot_layout(design = "
  AABB
  AABB
  CCCC
  DDDD
  DDDD
  DDDD
  ")

# save figure file
if (.fig_save) {
  ggsave(
    plot = shift_gg,
    filename = str_c(.path$out_fig, "fig-main-shift.pdf"),
    width = 9,
    height = 13
  )
}
