#' @export
plot_site_climate_change <- function(dat_clim_site, dat_avail, layout = "surround", nrow = NULL) {
  sf_cfp <- read_cfp(path_cfp = system.file("extdata", "cfp", package = "grassland"))
  ras_grass <- read_grasscover(path_grass = system.file("extdata", "cfp-grassland-percent-cover.tif", package = "grassland"))

  site_map_gg <- plot_site_map(sf_cfp, ras_grass)

  if (layout == "surround") {
    gg_cc_sites <- list(
      plot_site_cc(dat_clim_site, dat_avail, "angelo", tmp_lab = "Temperature (°C)", ppt_lab = "Precipitation (mm)"),
      plot_site_cc(dat_clim_site, dat_avail, "carrizo"),
      plot_site_cc(dat_clim_site, dat_avail, "elkhorn", tmp_lab = "Temperature (°C)", ppt_lab = "Precipitation (mm)"),
      plot_site_cc(dat_clim_site, dat_avail, "jasper"),
      plot_site_cc(dat_clim_site, dat_avail, "mclann", tmp_lab = "Temperature (°C)", ppt_lab = "Precipitation (mm)"),
      plot_site_cc(dat_clim_site, dat_avail, "mclserp"),
      plot_site_cc(dat_clim_site, dat_avail, "morganterritory"),
      plot_site_cc(dat_clim_site, dat_avail, "pleasantonridge"),
      plot_site_cc(dat_clim_site, dat_avail, "sunol", tmp_lab = "Temperature (°C)", ppt_lab = "Precipitation (mm)", yr_axis = TRUE),
      plot_site_cc(dat_clim_site, dat_avail, "swanton", yr_axis = TRUE),
      plot_site_cc(dat_clim_site, dat_avail, "ucsc", yr_axis = TRUE),
      plot_site_cc(dat_clim_site, dat_avail, "vascocaves", yr_axis = TRUE)
    )
  } else if (layout == "landsc") {
    if (nrow == 2) {
      gg_cc_sites <- list(
        plot_site_cc(dat_clim_site, dat_avail, "angelo", tmp_lab = "Temperature (°C)", ppt_lab = "Precipitation (mm)"),
        plot_site_cc(dat_clim_site, dat_avail, "carrizo"),
        plot_site_cc(dat_clim_site, dat_avail, "elkhorn"),
        plot_site_cc(dat_clim_site, dat_avail, "jasper"),
        plot_site_cc(dat_clim_site, dat_avail, "mclann"),
        plot_site_cc(dat_clim_site, dat_avail, "mclserp"),
        plot_site_cc(dat_clim_site, dat_avail, "morganterritory", tmp_lab = "Temperature (°C)", ppt_lab = "Precipitation (mm)", yr_axis = TRUE),
        plot_site_cc(dat_clim_site, dat_avail, "pleasantonridge", yr_axis = TRUE),
        plot_site_cc(dat_clim_site, dat_avail, "sunol", yr_axis = TRUE),
        plot_site_cc(dat_clim_site, dat_avail, "swanton", yr_axis = TRUE),
        plot_site_cc(dat_clim_site, dat_avail, "ucsc", yr_axis = TRUE),
        plot_site_cc(dat_clim_site, dat_avail, "vascocaves", yr_axis = TRUE)
      )
    }
  }

  if (layout == "surround") {
    gg_cc <- site_map_gg +
      gg_cc_sites +
      plot_layout(design = "
  AABC
  AADE
  FGHI
  JKLM
")
  } else if (layout == "landsc") {
    gg_cc <-
      wrap_plots(gg_cc_sites) +
      plot_layout(nrow = nrow)
  }

  return(gg_cc)
}

#' @export
plot_site_cc <- function(data, dat_avail, site_name,
                         tmp_lab = "", ppt_lab = "", yr_lab = NULL, yr_axis = FALSE) {
  # prepare site data
  site_lab <- plot_site_name(site_name, with_letter = T)

  df_range <- dat_avail %>%
    filter(site == site_name) %>%
    summarise(
      min = min(year),
      max = max(year)
    )

  site_tbl <- data %>%
    filter(site == site_name) %>%
    select(site, year, tmp, ppt) %>%
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
  out_gg <-
    ggplot(site_tbl) +
    geom_rect( # highlight observational years
      data = df_range,
      aes(
        xmin = min, xmax = max,
        ymin = -Inf, ymax = Inf
      ),
      fill = "orange", alpha = 0.25
    ) +
    geom_point(
      aes(x = year, y = clim_val),
      color = "gray", shape = 20
    ) +
    geom_smooth(
      aes(x = year, y = clim_val, linetype = ifelse(p_val <= 0.05, "sig", "ns")),
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
