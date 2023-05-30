# make a map for observational sites and grassland percent cover
source("scripts/plot-site-map.R")

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

samp_size_tbl <- obs_tbl %>%
  group_by(site) %>%
  summarize(
    n_plot = n_distinct(plot),
    n_year = n_distinct(year)
  )

# define plotting function
plot_cwm <- function(tbl, site_name, cti_lab = "", cpi_lab = "", yr_lab = NULL, yr_axis = FALSE) {
  # prepare site data
  site_lab <- site_vec[site_name]
  site_lab <- str_c(LETTERS[which(site_vec == site_lab)], site_lab, sep = ". ")

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
  out_gg <- ggplot(site_tbl, aes(year, com_idx_value)) +
    geom_boxplot(aes(group = year), color = "gray", outlier.shape = 20) +
    geom_smooth( # add lm trend line when significant
      data = . %>% filter(p_val <= 0.05),
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

  # remove yr axis text
  if (yr_axis) {
    return(out_gg)
  } else {
    return(out_gg + theme(axis.text.x = element_blank()))
  }
}

# apply plotting function, make individual panels, combine
obs_gg <-
  site_map_gg +
  plot_cwm(obs_idx_tbl, "angelo", cti_lab = "CTI (°C)", cpi_lab = "CPI (mm)") +
  plot_cwm(obs_idx_tbl, "carrizo") +
  plot_cwm(obs_idx_tbl, "elkhorn", cti_lab = "CTI (°C)", cpi_lab = "CPI (mm)") +
  plot_cwm(obs_idx_tbl, "jasper") +
  plot_cwm(obs_idx_tbl, "mclann", cti_lab = "CTI (°C)", cpi_lab = "CPI (mm)") +
  plot_cwm(obs_idx_tbl, "mclserp") +
  plot_cwm(obs_idx_tbl, "morganterritory") +
  plot_cwm(obs_idx_tbl, "pleasantonridge") +
  plot_cwm(obs_idx_tbl, "sunol", cti_lab = "CTI (°C)", cpi_lab = "CPI (mm)", yr_axis = TRUE) +
  plot_cwm(obs_idx_tbl, "swanton", yr_axis = TRUE) +
  plot_cwm(obs_idx_tbl, "ucsc", yr_axis = TRUE) +
  plot_cwm(obs_idx_tbl, "vascocaves", yr_axis = TRUE) +
  plot_layout(design = "
  AABC
  AADE
  FGHI
  JKLM
")

# save figure file
if (.fig_save) {
  ggsave(
    plot = obs_gg,
    filename = str_c(.path$out_fig, "fig-main-obs-comb.png"),
    width = 11,
    height = 11 * 1.5,
    device = png, type = "cairo"
  )
}

# for slides
obs_gg_2row <-
  plot_cwm(obs_idx_tbl, "angelo", cti_lab = "CTI (°C)", cpi_lab = "CPI (mm)") +
  plot_cwm(obs_idx_tbl, "carrizo") +
  plot_cwm(obs_idx_tbl, "elkhorn") +
  plot_cwm(obs_idx_tbl, "jasper") +
  plot_cwm(obs_idx_tbl, "mclann") +
  plot_cwm(obs_idx_tbl, "mclserp") +
  plot_cwm(obs_idx_tbl, "morganterritory", cti_lab = "CTI (°C)", cpi_lab = "CPI (mm)", yr_axis = TRUE) +
  plot_cwm(obs_idx_tbl, "pleasantonridge", yr_axis = TRUE) +
  plot_cwm(obs_idx_tbl, "sunol", yr_axis = TRUE) +
  plot_cwm(obs_idx_tbl, "swanton", yr_axis = TRUE) +
  plot_cwm(obs_idx_tbl, "ucsc", yr_axis = TRUE) +
  plot_cwm(obs_idx_tbl, "vascocaves", yr_axis = TRUE) +
  plot_layout(nrow = 2)

if (.fig_save) {
  ggsave(
    plot = obs_gg_2row,
    filename = str_c(.path$out_fig, "fig-slide-obs.png"),
    width = 9.32 * 1.75,
    height = 3.74 * 1.75,
    device = png, type = "cairo"
  )
}

obs_gg_3row <-
  plot_cwm(obs_idx_tbl, "angelo", cti_lab = "CTI (°C)", cpi_lab = "CPI (mm)") +
  plot_cwm(obs_idx_tbl, "carrizo") +
  plot_cwm(obs_idx_tbl, "elkhorn") +
  plot_cwm(obs_idx_tbl, "jasper") +
  plot_cwm(obs_idx_tbl, "mclann", cti_lab = "CTI (°C)", cpi_lab = "CPI (mm)") +
  plot_cwm(obs_idx_tbl, "mclserp") +
  plot_cwm(obs_idx_tbl, "morganterritory") +
  plot_cwm(obs_idx_tbl, "pleasantonridge") +
  plot_cwm(obs_idx_tbl, "sunol", cti_lab = "CTI (°C)", cpi_lab = "CPI (mm)", yr_axis = TRUE) +
  plot_cwm(obs_idx_tbl, "swanton", yr_axis = TRUE) +
  plot_cwm(obs_idx_tbl, "ucsc", yr_axis = TRUE) +
  plot_cwm(obs_idx_tbl, "vascocaves", yr_axis = TRUE) +
  plot_layout(nrow = 3)

if (.fig_save) {
  ggsave(
    plot = obs_gg_3row,
    filename = str_c(.path$out_fig, "fig-slide-obs-3row.png"),
    width = 9.32 * 2.5 / 2,
    height = 3.74 * 2.5,
    device = png, type = "cairo"
  )
}
