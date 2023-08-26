plot_community_index <- function(option, dat_index) {
  if (option == "obs") {
    community_index_gg <- plot_community_index_obs(obs_tbl = dat_index$obs)
  }
  if (option == "exp") {
    community_index_gg <- plot_community_index_exp(exp_tbl = dat_index$exp)
  }

  return(community_index_gg)
}

plot_community_index_obs <- function(obs_tbl) {
  sf_cfp <- read_cfp(path_cfp = system.file("extdata", "cfp", package = "grassland"))
  ras_grass <- read_grasscover(path_grass = system.file("extdata", "cfp-grassland-percent-cover.tif", package = "grassland"))

  site_map_gg <- plot_site_map(sf_cfp, ras_grass)
  # reshape data
  obs_idx_tbl <- obs_tbl %>%
    select(site, year, plot, tmp_com_mean, ppt_com_mean) %>%
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

  return(obs_gg)
}

# define plotting function
plot_cwm <- function(tbl, site_name, cti_lab = "", cpi_lab = "", yr_lab = NULL, yr_axis = FALSE) {
  # prepare site data
  site_lab <- plot_site_name(site_name, with_letter = T)

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

plot_site_name <- function(name, with_letter = F) {
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

  if (with_letter) {
    label <- str_c(LETTERS[which(site_vec == site_vec[name])], site_vec[name], sep = ". ")
  } else {
    label <- site_vec[name]
  }

  return(label)
}


plot_community_index_exp <- function(exp_tbl, site = "jrgce") {
  # JRGCE data
  jrgce_tbl <- exp_tbl %>%
    filter(site == "jrgce", year >= 1999) %>%
    mutate(treat_T = str_sub(treat, start = 1L, end = 1L)) %>%
    # mutate(subgrp = str_sub(treat, start = 2L, end = 4L)) %>%
    # filter(subgrp == "___") %>%
    select(site, year, plot, treat_T, tmp_com_mean, ppt_com_mean) %>%
    pivot_longer(cols = tmp_com_mean:ppt_com_mean, names_to = "com_idx_name", values_to = "com_idx_value") %>%
    mutate(com_idx_name = factor(com_idx_name,
      levels = c("tmp_com_mean", "ppt_com_mean"),
      labels = c("CTI", "CPI")
    ))

  warm_tbl <- tribble(
    ~tag, ~name, ~start, ~end, ~startyear,
    1, "Phase~I:~+80~W~m^-2%~~%+1~degree*C", -Inf, 2002, 1999,
    2, "Phase~II:~+100~W~m^-2%~~%+1.5~degree*C", 2003, 2009, 2003,
    3, "Phase~III:~+250~W~m^-2%~~%+2~degree*C", 2010, Inf, 2010 # end in 2014, but set to Inf to fill space
  )

  exp_gg <-
    ggplot(jrgce_tbl) +
    geom_rect( # warming phrases
      data = warm_tbl,
      aes(xmin = start - .5, xmax = end + .5, fill = tag),
      ymin = -Inf, ymax = Inf, alpha = 0.5
    ) +
    scale_fill_gradient(low = "antiquewhite", high = "orange") +
    geom_boxplot( # treatment effects
      aes(x = year, y = com_idx_value, col = treat_T, group = interaction(treat_T, year))
    ) +
    scale_color_manual(values = c("black", "red")) +
    ggpubr::stat_compare_means( # significance
      aes(x = year, y = com_idx_value, group = treat_T),
      method = "wilcox.test",
      label = "p.signif", hide.ns = FALSE
    ) +
    facet_wrap( # CTI & CPI panels
      ~com_idx_name,
      ncol = 1, scales = "free_y",
      strip.position = "left",
      labeller = labeller(com_idx_name = c(
        CTI = "Community Temperature Index\n(CTI, °C)",
        CPI = "Community Precipitation Index\n(CPI, mm)"
      ))
    ) +
    scale_y_continuous(expand = expansion(mult = .1)) + # expand padding to show significance tests
    scale_x_continuous(expand = expansion(mult = 0, add = c(0.125, 0.125))) +
    labs(
      x = NULL, # "Year",
      y = NULL,
      color = "Warming treatment",
    ) +
    geom_text(
      data = warm_tbl %>%
        mutate(
          com_idx_name = factor("tmp_com_mean",
            levels = c("tmp_com_mean", "ppt_com_mean"),
            labels = c("CTI", "CPI")
          )
        ),
      aes(
        label = name,
        x = startyear - 0.25, # y = cti_max
      ),
      y = 17, # manually label phase text
      parse = TRUE,
      hjust = 0,
    ) +
    coord_cartesian(clip = "off") +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside",
      legend.position = "none",
      plot.margin = unit(c(2, 1, 1, 1), "lines") # expand margin to include warming labels
    )

  return(exp_gg)
}
