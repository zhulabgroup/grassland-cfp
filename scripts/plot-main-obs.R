# import niche and observational data, calculate CTI and CPI
niche_tbl <- read_rds(.path$sum_niche) %>%
  filter(occ_n > 100 | is.na(occ_n)) # species with many observations and dummy species

obs_tbl <- read_rds(.path$com_obs) %>%
  inner_join(niche_tbl, by = "species") %>%
  group_by(site, year, plot) %>%
  summarize(
    tmp_com_mean = sum(abund * tmp_occ_mean) / sum(abund),
    ppt_com_mean = sum(abund * ppt_occ_mean) / sum(abund)
  )

# setup site labels
site_vec <- c(
  angelo = "Angelo Coast",
  carrizo = "Carrizo Plain",
  elkhorn = "Elkhorn Slough",
  jasper = "Jasper Ridge",
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

samp_size_tbl <- obs_tbl %>%
  group_by(site) %>%
  summarize(
    n_plot = n_distinct(plot),
    n_year = n_distinct(year)
  )

# define plotting function
plot_cwm <- function(tbl, site_name, cti_lab = "", cpi_lab = "", yr_lab = NULL) {
  # prepare site data
  site_lab <- site_vec[site_name]
  site_tbl <- tbl %>%
    filter(site == site_name) %>%
    group_by(com_idx_name) %>%
    nest() %>%
    mutate( # lm and p value
      p_val = map(data, ~ lm(com_idx_value ~ year, data = .)) %>%
        map_dbl(~ broom::glance(.) %>% pull(p.value))
    ) %>%
    unnest(cols = data)

  # plot
  ggplot(site_tbl, aes(year, com_idx_value)) +
    geom_boxplot(aes(group = year)) +
    geom_smooth( # add lm trend line when significant
      data = . %>% filter(p_val < 0.05),
      method = "lm", formula = y ~ x, se = FALSE,
      color = "red"
    ) +
    facet_wrap(~com_idx_name,
      ncol = 1, scales = "free_y",
      strip.position = "left",
      labeller = labeller(com_idx_name = c(
        CTI = cti_lab, # "Community Temperature Index\n(CTI, °C)",
        CPI = cpi_lab # "Community Precipitation Index\n(CPI, mm)"
      ))
    ) +
    expand_limits(x = range(tbl$year)) +
    labs(
      x = yr_lab, y = NULL,
      title = site_lab
    ) +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside",
      plot.title = element_text(size = 11)
    )
}

# apply plotting function, make individual panels, combine
obs_gg <-
  plot_cwm(obs_idx_tbl, "angelo", cti_lab = "CTI (°C)", cpi_lab = "CPI (mm)") +
  plot_cwm(obs_idx_tbl, "carrizo") +
  plot_cwm(obs_idx_tbl, "elkhorn") +
  plot_cwm(obs_idx_tbl, "jasper") +
  plot_cwm(obs_idx_tbl, "mclann", cti_lab = "CTI (°C)", cpi_lab = "CPI (mm)") +
  plot_cwm(obs_idx_tbl, "mclserp") +
  plot_cwm(obs_idx_tbl, "morganterritory") +
  plot_cwm(obs_idx_tbl, "pleasantonridge") +
  plot_cwm(obs_idx_tbl, "sunol", cti_lab = "CTI (°C)", cpi_lab = "CPI (mm)") +
  plot_cwm(obs_idx_tbl, "swanton") +
  plot_cwm(obs_idx_tbl, "ucsc") +
  plot_cwm(obs_idx_tbl, "vascocaves") +
  plot_annotation(tag_levels = "A") +
  plot_layout(design = "
  ABCD
  EFGH
  IJKL
  ")

# save figure file
if (FALSE) {
  ggsave(
    plot = obs_gg,
    filename = "figures/fig3-obs.png",
    width = 10,
    height = 10 * 1.618
  )
}
