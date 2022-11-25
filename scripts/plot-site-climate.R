# plot climate change at obs sites
# get site climate data
clim_tbl <- read_rds(.path$cli_chelsa_annual) %>%
  filter(abbr %in% c(
    "angelo", "carrizo", "elkhorn", "jasper", "mclann", "mclserp",
    "morganterritory", "pleasantonridge", "sunol",
    "swanton", "ucsc", "vascocaves"
  ))

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

# define plotting function
plot_cc <- function(data, site_abbr,
                    tmp_lab = "", ppt_lab = "", yr_lab = NULL, yr_axis = FALSE) {
  # prepare site data
  site_lab <- site_vec[site_abbr]
  site_lab <- str_c(LETTERS[which(site_vec == site_lab)], site_lab, sep = ". ")

  site_tbl <- data %>%
    filter(abbr == site_abbr) %>%
    select(abbr, year, tmp, ppt) %>%
    pivot_longer(tmp:ppt, names_to = "clim_var", values_to = "clim_val") %>%
    mutate(clim_var = factor(clim_var,
      levels = c("tmp", "ppt")
    )) %>%
    group_by(clim_var) %>%
    nest() %>%
    mutate(
      p_val = map(data, ~ lm(clim_val ~ year, data = .)) %>%
        map_dbl(~ broom::glance(.) %>% pull(p.value))
    ) %>%
    unnest(cols = data)

  # plot
  out_gg <- ggplot(site_tbl, aes(year, clim_val)) +
    geom_point(color = gray(.5, .5), shape = 20) +
    geom_smooth(
      aes(linetype = ifelse(p_val < 0.05, "sig", "ns")),
      method = "lm", formula = y ~ x, se = FALSE,
      color = "red"
    ) +
    scale_linetype_manual(values = c("sig" = "solid", "ns" = "dashed")) +
    # ggpubr::stat_cor(aes(label = ..p.label..),
    #   p.accuracy = 0.05,
    #   color = "red"
    # ) +
    facet_wrap(~clim_var,
      ncol = 1, scales = "free_y",
      strip.position = "left",
      labeller = labeller(clim_var = c(tmp = tmp_lab, ppt = ppt_lab))
    ) +
    expand_limits(x = c(min(data$year) - 1, max(data$year + 1))) +
    labs(
      title = site_lab,
      x = yr_lab, y = NULL
    ) +
    theme(
      legend.position = "none",
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

site_cc_gg <-
  plot_cc(clim_tbl, "angelo", tmp_lab = "Temperature (°C)", ppt_lab = "Precipitation (mm)") +
  plot_cc(clim_tbl, "carrizo") +
  plot_cc(clim_tbl, "elkhorn") +
  plot_cc(clim_tbl, "jasper") +
  plot_cc(clim_tbl, "mclann", tmp_lab = "Temperature (°C)", ppt_lab = "Precipitation (mm)") +
  plot_cc(clim_tbl, "mclserp") +
  plot_cc(clim_tbl, "morganterritory") +
  plot_cc(clim_tbl, "pleasantonridge") +
  plot_cc(clim_tbl, "sunol", tmp_lab = "Temperature (°C)", ppt_lab = "Precipitation (mm)", yr_axis = TRUE) +
  plot_cc(clim_tbl, "swanton", yr_axis = TRUE) +
  plot_cc(clim_tbl, "ucsc", yr_axis = TRUE) +
  plot_cc(clim_tbl, "vascocaves", yr_axis = TRUE) +
  plot_layout(ncol = 4, nrow = 3)
