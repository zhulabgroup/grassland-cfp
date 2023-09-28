plot_community_index <- function(option, dat_index, experiment = NULL, treatment = NULL) {
  if (option == "obs") {
    community_index_gg <- plot_community_index_obs(obs_tbl = dat_index$obs)
  }
  if (option == "exp") {
    community_index_gg <- plot_community_index_exp(exp_tbl = dat_index$exp, experiment = experiment, treatment = treatment)
  }

  return(community_index_gg)
}

plot_community_index_obs <- function(obs_tbl) {
  sf_cfp <- read_cfp(path_cfp = system.file("extdata", "cfp", package = "grassland"))
  ras_grass <- read_grasscover(path_grass = system.file("extdata", "cfp-grassland-percent-cover.tif", package = "grassland"))

  site_map_gg <- plot_site_map(sf_cfp, ras_grass)
  # reshape data
  obs_idx_tbl <- obs_tbl %>%
    select(any_of(c("site", "year", "plot", "tmp_com_mean", "ppt_com_mean", "cwd_com_mean"))) %>%
    gather(key = "com_idx_name", value = "com_idx_value", -site, -year, -plot) %>%
    mutate(com_idx_name = factor(com_idx_name,
      levels = c("tmp_com_mean", "ppt_com_mean", "cwd_com_mean"),
      labels = c("CTI", "CPI", "CDI")
    ))

  samp_size_tbl <- obs_tbl %>%
    group_by(site) %>%
    summarize(
      n_plot = n_distinct(plot),
      n_year = n_distinct(year)
    )

  # apply plotting function, make individual panels, combine
  if (!"cwd_com_mean" %in% colnames(obs_tbl)) {
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
  } else {
    obs_gg <-
      site_map_gg +
      plot_cwm(obs_idx_tbl, "angelo", cti_lab = "CTI (°C)", cpi_lab = "CPI (mm)", cdi_lab = "CDI (mm)") +
      plot_cwm(obs_idx_tbl, "carrizo") +
      plot_cwm(obs_idx_tbl, "elkhorn", cti_lab = "CTI (°C)", cpi_lab = "CPI (mm)", cdi_lab = "CDI (mm)") +
      plot_cwm(obs_idx_tbl, "jasper") +
      plot_cwm(obs_idx_tbl, "mclann", cti_lab = "CTI (°C)", cpi_lab = "CPI (mm)", cdi_lab = "CDI (mm)") +
      plot_cwm(obs_idx_tbl, "mclserp") +
      plot_cwm(obs_idx_tbl, "morganterritory") +
      plot_cwm(obs_idx_tbl, "pleasantonridge") +
      plot_cwm(obs_idx_tbl, "sunol", cti_lab = "CTI (°C)", cpi_lab = "CPI (mm)", cdi_lab = "CDI (mm)", yr_axis = TRUE) +
      plot_cwm(obs_idx_tbl, "swanton", yr_axis = TRUE) +
      plot_cwm(obs_idx_tbl, "ucsc", yr_axis = TRUE) +
      plot_cwm(obs_idx_tbl, "vascocaves", yr_axis = TRUE) +
      plot_layout(design = "
  AABC
  AADE
  FGHI
  JKLM
")
  }

  return(obs_gg)
}

# define plotting function
plot_cwm <- function(tbl, site_name, cti_lab = "", cpi_lab = "", cdi_lab = "", yr_lab = NULL, yr_axis = FALSE) {
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
        CPI = cpi_lab, # "Community Precipitation Index\n(CPI, mm)"
        CDI = cdi_lab
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
  if (with_letter) {
    label <- str_c(LETTERS[which(read_site_name() == read_site_name()[name])], read_site_name()[name], sep = ". ")
  } else {
    label <- read_site_name()[name]
  }

  return(label)
}


plot_community_index_exp <- function(exp_tbl, experiment = "jrgce", treatment = "warming") {
  if (experiment == "jrgce") {
    p <- plot_community_index_jrgce(exp_tbl, treatment)
  }
  if (experiment == "mclexp") {
    p <- plot_community_index_mclexp(exp_tbl)
  }
  if (experiment == "scide") {
    p <- plot_community_index_scide(exp_tbl)
  }

  return(p)
}

plot_community_index_jrgce <- function(exp_tbl, treatment) {
  if (treatment == "warming") {
    p <- plot_community_index_jrgce_warming(exp_tbl)
  }
  if (treatment == "watering") {
    p <- plot_community_index_jrgce_watering(exp_tbl)
  }
  return(p)
}

plot_community_index_jrgce_warming <- function(exp_tbl) {
  # JRGCE data
  jrgce_tbl <- exp_tbl %>%
    filter(site == "jrgce", year >= 1999) %>%
    mutate(treat_T = str_sub(treat, start = 1L, end = 1L)) %>%
    # mutate(subgrp = str_sub(treat, start = 2L, end = 4L)) %>%
    # filter(subgrp == "___") %>%
    select(any_of(c("site", "year", "plot", "treat_T", "tmp_com_mean", "ppt_com_mean", "cwd_com_mean"))) %>%
    gather(key = "com_idx_name", value = "com_idx_value", -site, -year, -plot, -treat_T) %>%
    mutate(com_idx_name = factor(com_idx_name,
      levels = c("tmp_com_mean", "ppt_com_mean", "cwd_com_mean"),
      labels = c("CTI", "CPI", "CDI")
    ))

  change_tbl <- jrgce_tbl %>%
    mutate(grp = case_when(
      year <= 2002 ~ "Phase I",
      year >= 2010 ~ "Phase III",
      TRUE ~ "Phase II"
    )) %>%
    rename(
      trt = treat_T,
      value = com_idx_value
    ) %>%
    group_by(grp, com_idx_name) %>%
    nest() %>%
    mutate(test_index_change_model(dat_model = data[[1]], option = "exp") %>%
      test_change_summ()) %>%
    select(-data) %>%
    mutate(sig = if_else(str_detect(sig, "\\*"), sig, "ns")) %>%
    mutate(unit = case_when(
      com_idx_name == "CTI" ~ "°C",
      com_idx_name == "CPI" ~ "mm",
      com_idx_name == "CDI" ~ "mm",
    )) %>%
    mutate(start = case_when(
      grp == "Phase I" ~ 1999,
      grp == "Phase II" ~ 2003,
      grp == "Phase III" ~ 2010
    )) %>%
    mutate(end = case_when(
      grp == "Phase I" ~ 2002,
      grp == "Phase II" ~ 2009,
      grp == "Phase III" ~ 2014
    )) %>%
    left_join(
      jrgce_tbl %>%
        group_by(com_idx_name) %>%
        summarise(max = max(com_idx_value)),
      by = "com_idx_name"
    ) %>%
    mutate(max = case_when(
      com_idx_name == "CTI" ~ max + 1,
      com_idx_name == "CPI" ~ max + 200,
      com_idx_name == "CDI" ~ max + 200
    ))

  change_tbl_year <- jrgce_tbl %>%
    mutate(grp = year) %>%
    rename(
      trt = treat_T,
      value = com_idx_value
    ) %>%
    group_by(grp, com_idx_name) %>%
    nest() %>%
    mutate(test_index_change_model(dat_model = data[[1]], option = "exp") %>%
      test_change_summ()) %>%
    select(-data) %>%
    mutate(sig = if_else(str_detect(sig, "\\*"), sig, "ns")) %>%
    left_join(
      jrgce_tbl %>%
        group_by(com_idx_name) %>%
        summarise(max = max(com_idx_value)),
      by = "com_idx_name"
    ) %>%
    mutate(max = case_when(
      com_idx_name == "CTI" ~ max + 0.5,
      com_idx_name == "CPI" ~ max + 100,
      com_idx_name == "CDI" ~ max + 100
    ))

  warm_tbl <- read_warm_treatment()

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
    facet_wrap( # CTI & CPI panels
      ~com_idx_name,
      ncol = 1, scales = "free_y",
      strip.position = "left",
      labeller = labeller(com_idx_name = c(
        CTI = "Community Temperature Index\n(CTI, °C)",
        CPI = "Community Precipitation Index\n(CPI, mm)",
        CDI = "Community Drought Index\n(CDI, mm)"
      ))
    ) +
    geom_text(data = change_tbl, aes(x = (start + end) / 2, y = max, label = str_c(estimate %>% signif(3), " ", unit, " (", sig, ")", sep = ""))) +
    geom_text(data = change_tbl_year, aes(x = grp, y = max, label = sig)) +
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
            levels = c("tmp_com_mean", "ppt_com_mean", "cwd_com_mean"),
            labels = c("CTI", "CPI", "CDI")
          )
        ),
      aes(
        label = name,
        x = startyear - 0.25, # y = cti_max
      ),
      y = 18, # manually label phase text
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

plot_community_index_jrgce_watering <- function(exp_tbl) {
  jrgce_tbl <- exp_tbl %>%
    filter(site == "jrgce", year >= 1999) %>%
    mutate(treat_P = str_sub(treat, start = 2L, end = 2L)) %>%
    select(site, year, plot, treat_P, tmp_com_mean, ppt_com_mean) %>%
    pivot_longer(cols = tmp_com_mean:ppt_com_mean, names_to = "com_idx_name", values_to = "com_idx_value") %>%
    mutate(com_idx_name = factor(com_idx_name,
      levels = c("tmp_com_mean", "ppt_com_mean"),
      labels = c("CTI", "CPI")
    ))

  change_tbl <- jrgce_tbl %>%
    rename(
      trt = treat_P,
      value = com_idx_value
    ) %>%
    group_by(com_idx_name) %>%
    nest() %>%
    mutate(test_index_change_model(dat_model = data[[1]], option = "exp") %>%
      test_change_summ()) %>%
    select(-data) %>%
    mutate(sig = if_else(str_detect(sig, "\\*"), sig, "ns")) %>%
    mutate(unit = case_when(
      com_idx_name == "CTI" ~ "°C",
      com_idx_name == "CPI" ~ "mm",
      com_idx_name == "CDI" ~ "mm",
    )) %>%
    mutate(
      start = jrgce_tbl %>% pull(year) %>% min(),
      end = jrgce_tbl %>% pull(year) %>% max()
    ) %>%
    left_join(
      jrgce_tbl %>%
        group_by(com_idx_name) %>%
        summarise(max = max(com_idx_value)),
      by = "com_idx_name"
    ) %>%
    mutate(max = case_when(
      com_idx_name == "CTI" ~ max + 1,
      com_idx_name == "CPI" ~ max + 200
    ))

  change_tbl_year <- jrgce_tbl %>%
    mutate(grp = year) %>%
    rename(
      trt = treat_P,
      value = com_idx_value
    ) %>%
    group_by(grp, com_idx_name) %>%
    nest() %>%
    mutate(test_index_change_model(dat_model = data[[1]], option = "exp") %>%
      test_change_summ()) %>%
    select(-data) %>%
    mutate(sig = if_else(str_detect(sig, "\\*"), sig, "ns")) %>%
    left_join(
      jrgce_tbl %>%
        group_by(com_idx_name) %>%
        summarise(max = max(com_idx_value)),
      by = "com_idx_name"
    ) %>%
    mutate(max = case_when(
      com_idx_name == "CTI" ~ max + 0.5,
      com_idx_name == "CPI" ~ max + 100
    ))

  exp_water_gg <-
    ggplot(jrgce_tbl) +
    geom_boxplot( # treatment effects
      aes(x = year, y = com_idx_value, col = treat_P, group = interaction(treat_P, year))
    ) +
    scale_color_manual(values = c("black", "blue")) +
    facet_wrap( # CTI & CPI panels
      ~com_idx_name,
      ncol = 1, scales = "free_y",
      strip.position = "left",
      labeller = labeller(com_idx_name = c(
        CTI = "Community Temperature Index\n(CTI, °C)",
        CPI = "Community Precipitation Index\n(CPI, mm)"
      ))
    ) +
    geom_text(data = change_tbl, aes(x = (start + end) / 2, y = max, label = str_c(estimate %>% signif(3), " ", unit, " (", sig, ")", sep = ""))) +
    geom_text(data = change_tbl_year, aes(x = grp, y = max, label = sig)) +
    scale_y_continuous(expand = expansion(mult = .1)) + # expand padding to show significance tests
    scale_x_continuous(expand = expansion(mult = 0, add = c(0.125, 0.125))) +
    labs(
      x = NULL, # "Year",
      y = NULL,
      color = "Watering treatment",
    ) +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside",
      legend.position = "none",
      plot.margin = unit(c(2, 1, 1, 1), "lines"), # expand margin to include warming labels
      panel.background = element_rect(fill = "#F0F8FF")
    )

  return(exp_water_gg)
}

plot_community_index_mclexp <- function(exp_tbl) {
  mclexp_tbl <- exp_tbl %>%
    filter(site == "mclexp", year > 2015) %>%
    separate(treat, c("treat", "soil"), sep = 2) %>%
    select(site, year, plot, treat, soil, tmp_com_mean, ppt_com_mean) %>%
    pivot_longer(cols = tmp_com_mean:ppt_com_mean, names_to = "com_idx_name", values_to = "com_idx_value") %>%
    mutate(com_idx_name = factor(com_idx_name,
      levels = c("tmp_com_mean", "ppt_com_mean"),
      labels = c("CTI", "CPI")
    ))
  mclexp_gg <-
    plot_mclexp(mclexp_tbl, l_tag = "A", trt_tag = "Watering", s_tag = "Serpentine soil") +
    plot_mclexp(mclexp_tbl, l_tag = "B", trt_tag = "Watering", s_tag = "Non-serpentine soil") +
    plot_mclexp(mclexp_tbl, l_tag = "C", trt_tag = "Drought", s_tag = "Serpentine soil") +
    plot_layout(design = "
  ABC
")

  return(mclexp_gg)
}

plot_mclexp <- function(mclexp_tbl, l_tag = "A", trt_tag = "Watering", s_tag = "Serpentine soil") {
  # filter data by treatment tag
  if (trt_tag == "Watering") {
    mclexp_tbl <- filter(mclexp_tbl, treat %in% c("_X", "WX"))
    box_col <- "blue"
    bg_col <- "#F0F8FF"
  } else if (trt_tag == "Drought") {
    mclexp_tbl <- filter(mclexp_tbl, treat %in% c("X_", "XD"))
    box_col <- "brown"
    bg_col <- "#FFFFE0"
  } else {
    stop("Unknown treatment tag")
  }

  # filter data by soil tag
  if (s_tag == "Serpentine soil") {
    mclexp_tbl <- filter(mclexp_tbl, soil == "S")
  } else if (s_tag == "Non-serpentine soil") {
    mclexp_tbl <- filter(mclexp_tbl, soil == "N")
  } else {
    stop("Unknown soil tag")
  }

  change_tbl <- mclexp_tbl %>%
    rename(
      trt = treat,
      value = com_idx_value
    ) %>%
    group_by(com_idx_name) %>%
    nest() %>%
    mutate(test_index_change_model(dat_model = data[[1]], option = "exp") %>%
      test_change_summ()) %>%
    select(-data) %>%
    mutate(sig = if_else(str_detect(sig, "\\*"), sig, "ns")) %>%
    mutate(unit = case_when(
      com_idx_name == "CTI" ~ "°C",
      com_idx_name == "CPI" ~ "mm",
      com_idx_name == "CDI" ~ "mm",
    )) %>%
    mutate(
      start = mclexp_tbl %>% pull(year) %>% min(),
      end = mclexp_tbl %>% pull(year) %>% max()
    ) %>%
    left_join(
      mclexp_tbl %>%
        group_by(com_idx_name) %>%
        summarise(max = max(com_idx_value)),
      by = "com_idx_name"
    ) %>%
    mutate(max = case_when(
      com_idx_name == "CTI" ~ max + 1,
      com_idx_name == "CPI" ~ max + 200
    ))

  change_tbl_year <- mclexp_tbl %>%
    mutate(grp = year) %>%
    rename(
      trt = treat,
      value = com_idx_value
    ) %>%
    group_by(grp, com_idx_name) %>%
    nest() %>%
    mutate(test_index_change_model(dat_model = data[[1]], option = "exp") %>%
      test_change_summ()) %>%
    select(-data) %>%
    mutate(sig = if_else(str_detect(sig, "\\*"), sig, "ns")) %>%
    left_join(
      mclexp_tbl %>%
        group_by(com_idx_name) %>%
        summarise(max = max(com_idx_value)),
      by = "com_idx_name"
    ) %>%
    mutate(max = case_when(
      com_idx_name == "CTI" ~ max + 0.5,
      com_idx_name == "CPI" ~ max + 100
    ))

  p <-
    ggplot(mclexp_tbl) +
    geom_boxplot( # treatment effects
      aes(x = year, y = com_idx_value, col = treat, group = interaction(treat, year))
    ) +
    scale_color_manual(values = c("black", box_col)) +
    facet_wrap( # CTI & CPI panels
      ~com_idx_name,
      ncol = 1, scales = "free_y",
      strip.position = "left",
      labeller = labeller(com_idx_name = c(
        CTI = "Community Temperature Index\n(CTI, °C)",
        CPI = "Community Precipitation Index\n(CPI, mm)"
      ))
    ) +
    geom_text(data = change_tbl, aes(x = (start + end) / 2, y = max, label = str_c(estimate %>% signif(3), " ", unit, " (", sig, ")", sep = ""))) +
    geom_text(data = change_tbl_year, aes(x = grp, y = max, label = sig)) +
    scale_y_continuous(expand = expansion(mult = .1)) + # expand padding to show significance tests
    scale_x_continuous(expand = expansion(mult = 0, add = c(0.125, 0.125))) +
    labs(
      x = NULL, # "Year",
      y = NULL,
    ) +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside",
      legend.position = "none",
      plot.margin = unit(c(2, 1, 1, 1), "lines"), # expand margin to include warming labels
      panel.background = element_rect(fill = bg_col)
    ) +
    ggtitle(str_c(l_tag, ". ", trt_tag, " treatment\n(", s_tag, ")"))

  return(p)
}

plot_community_index_scide <- function(exp_tbl) {
  scide_tbl <- exp_tbl %>%
    filter(str_detect(site, "scide")) %>%
    select(site, year, plot, treat, tmp_com_mean, ppt_com_mean) %>%
    pivot_longer(cols = tmp_com_mean:ppt_com_mean, names_to = "com_idx_name", values_to = "com_idx_value") %>%
    mutate(com_idx_name = factor(com_idx_name,
      levels = c("tmp_com_mean", "ppt_com_mean"),
      labels = c("CTI", "CPI")
    ))

  scide_gg <-
    plot_scide(scide_tbl, l_tag = "A", site_tag = "Arboretum") +
    plot_scide(scide_tbl, l_tag = "B", site_tag = "Marshall Field") +
    plot_scide(scide_tbl, l_tag = "C", site_tag = "Younger Lagoon") +
    plot_layout(design = "
  ABC
")

  return(scide_gg)
}

plot_scide <- function(scide_tbl, l_tag = "A", site_tag = "Arboretum") {
  # filter data by site tag
  if (site_tag == "Arboretum") {
    scide_tbl <- scide_tbl %>%
      filter(str_detect(site, "arboretum"))
  } else if (site_tag == "Marshall Field") {
    scide_tbl <- scide_tbl %>%
      filter(str_detect(site, "marshallfield"))
  } else if (site_tag == "Younger Lagoon") {
    scide_tbl <- scide_tbl %>%
      filter(str_detect(site, "ylr"))
  } else {
    stop("Unknown site tag")
  }

  box_col <- "brown"
  bg_col <- "#FFFFE0"

  change_tbl <- scide_tbl %>%
    rename(
      trt = treat,
      value = com_idx_value
    ) %>%
    group_by(com_idx_name) %>%
    nest() %>%
    mutate(test_index_change_model(dat_model = data[[1]], option = "exp") %>%
      test_change_summ()) %>%
    select(-data) %>%
    mutate(sig = if_else(str_detect(sig, "\\*"), sig, "ns")) %>%
    mutate(unit = case_when(
      com_idx_name == "CTI" ~ "°C",
      com_idx_name == "CPI" ~ "mm",
      com_idx_name == "CDI" ~ "mm",
    )) %>%
    mutate(
      start = scide_tbl %>% pull(year) %>% min(),
      end = scide_tbl %>% pull(year) %>% max()
    ) %>%
    left_join(
      scide_tbl %>%
        group_by(com_idx_name) %>%
        summarise(max = max(com_idx_value)),
      by = "com_idx_name"
    ) %>%
    mutate(max = case_when(
      com_idx_name == "CTI" ~ max + 1,
      com_idx_name == "CPI" ~ max + 200
    ))

  change_tbl_year <- scide_tbl %>%
    mutate(grp = year) %>%
    rename(
      trt = treat,
      value = com_idx_value
    ) %>%
    group_by(grp, com_idx_name) %>%
    nest() %>%
    mutate(test_index_change_model(dat_model = data[[1]], option = "exp") %>%
      test_change_summ()) %>%
    select(-data) %>%
    mutate(sig = if_else(str_detect(sig, "\\*"), sig, "ns")) %>%
    left_join(
      scide_tbl %>%
        group_by(com_idx_name) %>%
        summarise(max = max(com_idx_value)),
      by = "com_idx_name"
    ) %>%
    mutate(max = case_when(
      com_idx_name == "CTI" ~ max + 0.5,
      com_idx_name == "CPI" ~ max + 100
    ))

  p <-
    ggplot(scide_tbl) +
    geom_boxplot( # treatment effects
      aes(x = year, y = com_idx_value, col = treat, group = interaction(treat, year))
    ) +
    scale_color_manual(values = c("black", box_col)) +
    facet_wrap( # CTI & CPI panels
      ~com_idx_name,
      ncol = 1, scales = "free_y",
      strip.position = "left",
      labeller = labeller(com_idx_name = c(
        CTI = "Community Temperature Index\n(CTI, °C)",
        CPI = "Community Precipitation Index\n(CPI, mm)"
      ))
    ) +
    geom_text(data = change_tbl, aes(x = (start + end) / 2, y = max, label = str_c(estimate %>% signif(3), " ", unit, " (", sig, ")", sep = " "))) +
    geom_text(data = change_tbl_year, aes(x = grp, y = max, label = sig)) +
    scale_y_continuous(expand = expansion(mult = .1)) + # expand padding to show significance tests
    scale_x_continuous(expand = expansion(mult = 0, add = c(0.125, 0.125))) +
    labs(
      x = NULL, # "Year",
      y = NULL,
    ) +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside",
      legend.position = "none",
      plot.margin = unit(c(2, 1, 1, 1), "lines"), # expand margin to include warming labels
      panel.background = element_rect(fill = bg_col)
    ) +
    ggtitle(str_c(l_tag, ". ", site_tag))

  return(p)
}
