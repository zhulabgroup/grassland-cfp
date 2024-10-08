#' Plot Community Index
#'
#' This function plots the changes in community indices including CTI and CPI over time at observational sites or under treatments in global change experiments.
#'
#' @param option A string to choose between two types of plots. It should be set to "obs" to plot changes over time at observational sites, or set of "exp" to plot changes under treatments in global change experiments.
#' @param dat_index A list with calculated community indices for observations and experiments.
#' @param experiment A string indicating the specific experiment to plot for. Only valid when "option" is set to "exp". Options are "jrgce" for the Jasper Ridge Global Change Experiment, "mwe" for the McLaughlin Water Experiment, and "scide" for the Santa Cruz International Drought Experiment.
#' @param treatment A string indicating the specific treatment in the experiment to plot for. Only valid when "option" is set to "exp" and "experiment" is set tp "jrgce". Options are "warming" or "watering".
#' @param layout A string indicating the layout of the observational plot, either "surround" to have all panels surrounding the site map, or "landsc" to have a simple landscape layout. Only valid when "option" is set to "obs".
#' @param nrow An integer indicating the number of rows in the observational plot. Only valid when "option" is set to "obs" and "layout" is set to "landsc".
#' @param onesite An optional parameter specifying a specific site to be plotted.
#'
#' @return A ggplot2 object of community index plot.
#' @export
#' @examples
#' \dontrun{
#' plot_community_index(option = "obs", dat_index, layout = "surround")
#' plot_community_index(option = "exp", dat_index, experiment = "jrgce", treatment = "warming")
#' }
#' @export
plot_community_index <- function(option, dat_index, experiment = NULL, treatment = NULL, layout = "surround", nrow = NULL,
                                 onesite = NULL) {
  if (option == "obs") {
    community_index_gg <- plot_community_index_obs(obs_tbl = dat_index$obs, layout = layout, nrow = nrow, onesite = onesite)
  }
  if (option == "exp") {
    community_index_gg <- plot_community_index_exp(exp_tbl = dat_index$exp, experiment = experiment, treatment = treatment)
  }

  return(community_index_gg)
}

plot_community_index_obs <- function(obs_tbl, layout = "surround", nrow = NULL, onesite = NULL) {
  site_map_gg <- plot_site_map(sf_cfp = NULL, ras_grass = NULL)
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
    if (layout == "surround") {
      obs_gg_sites <- list(
        plot_cwm(obs_idx_tbl, "angelo", cti_lab = "CTI (°C)", cpi_lab = "CPI (mm)"),
        plot_cwm(obs_idx_tbl, "carrizo"),
        plot_cwm(obs_idx_tbl, "elkhorn", cti_lab = "CTI (°C)", cpi_lab = "CPI (mm)"),
        plot_cwm(obs_idx_tbl, "jasper"),
        plot_cwm(obs_idx_tbl, "mclann", cti_lab = "CTI (°C)", cpi_lab = "CPI (mm)"),
        plot_cwm(obs_idx_tbl, "mclserp"),
        plot_cwm(obs_idx_tbl, "morganterritory"),
        plot_cwm(obs_idx_tbl, "pleasantonridge"),
        plot_cwm(obs_idx_tbl, "sunol", cti_lab = "CTI (°C)", cpi_lab = "CPI (mm)", yr_axis = TRUE),
        plot_cwm(obs_idx_tbl, "swanton", yr_axis = TRUE),
        plot_cwm(obs_idx_tbl, "ucsc", yr_axis = TRUE),
        plot_cwm(obs_idx_tbl, "vascocaves", yr_axis = TRUE)
      )
    } else if (layout == "landsc") {
      if (nrow == 2) {
        obs_gg_sites <- list(
          plot_cwm(obs_idx_tbl, "angelo", cti_lab = "CTI (°C)", cpi_lab = "CPI (mm)"),
          plot_cwm(obs_idx_tbl, "carrizo"),
          plot_cwm(obs_idx_tbl, "elkhorn"),
          plot_cwm(obs_idx_tbl, "jasper"),
          plot_cwm(obs_idx_tbl, "mclann"),
          plot_cwm(obs_idx_tbl, "mclserp"),
          plot_cwm(obs_idx_tbl, "morganterritory", cti_lab = "CTI (°C)", cpi_lab = "CPI (mm)", yr_axis = TRUE),
          plot_cwm(obs_idx_tbl, "pleasantonridge", yr_axis = TRUE),
          plot_cwm(obs_idx_tbl, "sunol", yr_axis = TRUE),
          plot_cwm(obs_idx_tbl, "swanton", yr_axis = TRUE),
          plot_cwm(obs_idx_tbl, "ucsc", yr_axis = TRUE),
          plot_cwm(obs_idx_tbl, "vascocaves", yr_axis = TRUE)
        )
      }
      if (nrow == 3) {
        obs_gg_sites <- list(
          plot_cwm(obs_idx_tbl, "angelo", cti_lab = "CTI (°C)", cpi_lab = "CPI (mm)"),
          plot_cwm(obs_idx_tbl, "carrizo"),
          plot_cwm(obs_idx_tbl, "elkhorn"),
          plot_cwm(obs_idx_tbl, "jasper"),
          plot_cwm(obs_idx_tbl, "mclann", cti_lab = "CTI (°C)", cpi_lab = "CPI (mm)"),
          plot_cwm(obs_idx_tbl, "mclserp"),
          plot_cwm(obs_idx_tbl, "morganterritory"),
          plot_cwm(obs_idx_tbl, "pleasantonridge"),
          plot_cwm(obs_idx_tbl, "sunol", cti_lab = "CTI (°C)", cpi_lab = "CPI (mm)", yr_axis = TRUE),
          plot_cwm(obs_idx_tbl, "swanton", yr_axis = TRUE),
          plot_cwm(obs_idx_tbl, "ucsc", yr_axis = TRUE),
          plot_cwm(obs_idx_tbl, "vascocaves", yr_axis = TRUE)
        )
      }
    }
  } else {
    obs_gg_sites <- list(
      plot_cwm(obs_idx_tbl, "angelo", cti_lab = "CTI (°C)", cpi_lab = "CPI (mm)", cdi_lab = "CDI (mm)"),
      plot_cwm(obs_idx_tbl, "carrizo"),
      plot_cwm(obs_idx_tbl, "elkhorn", cti_lab = "CTI (°C)", cpi_lab = "CPI (mm)", cdi_lab = "CDI (mm)"),
      plot_cwm(obs_idx_tbl, "jasper"),
      plot_cwm(obs_idx_tbl, "mclann", cti_lab = "CTI (°C)", cpi_lab = "CPI (mm)", cdi_lab = "CDI (mm)"),
      plot_cwm(obs_idx_tbl, "mclserp"),
      plot_cwm(obs_idx_tbl, "morganterritory"),
      plot_cwm(obs_idx_tbl, "pleasantonridge"),
      plot_cwm(obs_idx_tbl, "sunol", cti_lab = "CTI (°C)", cpi_lab = "CPI (mm)", cdi_lab = "CDI (mm)", yr_axis = TRUE),
      plot_cwm(obs_idx_tbl, "swanton", yr_axis = TRUE),
      plot_cwm(obs_idx_tbl, "ucsc", yr_axis = TRUE),
      plot_cwm(obs_idx_tbl, "vascocaves", yr_axis = TRUE)
    )
  }
  if (layout == "surround") {
    obs_gg <-
      site_map_gg +
      obs_gg_sites +
      plot_layout(design = "
          AABC
          AADE
          FGHI
          JKLM
        ")
  } else if (layout == "landsc") {
    obs_gg <-
      wrap_plots(obs_gg_sites) +
      plot_layout(nrow = nrow)
  }

  if (!is.null(onesite)) {
    obs_gg <- plot_cwm(obs_idx_tbl, site_name = onesite, cti_lab = "CTI (°C)", cpi_lab = "CPI (mm)", yr_axis = TRUE)
  }

  return(obs_gg)
}

plot_cwm <- function(tbl, site_name, cti_lab = "", cpi_lab = "", cdi_lab = "", yr_lab = NULL, yr_axis = FALSE) {
  # prepare site data
  site_lab <- plot_site_name(site_name, with_letter = T)

  site_tbl <- tbl %>%
    filter(site == site_name) %>%
    group_by(com_idx_name) %>%
    nest() %>%
    mutate( # lm and p value
      map(data, ~ lm(com_idx_value ~ year, data = .)) %>%
        map_df(~ broom::tidy(.) %>%
          filter(term != "(Intercept)") %>%
          select(
            beta = estimate,
            p.value
          ))
    ) %>%
    mutate(sig = gtools::stars.pval(p.value)) %>%
    mutate(sig = ifelse(sig != " " & sig != ".", sig, "ns")) %>%
    mutate(beta = beta %>% signif(3)) %>%
    unnest(cols = data)

  # plot
  out_gg <- ggplot(site_tbl, aes(year, com_idx_value)) +
    geom_boxplot(aes(group = year), color = "gray", outlier.shape = 20) +
    geom_smooth( # add lm trend line when significant
      data = . %>% filter(p.value <= 0.05),
      method = "lm", formula = y ~ x, se = FALSE,
      color = "red"
    ) +
    geom_text(
      data = . %>%
        distinct(com_idx_name, .keep_all = T) %>%
        mutate(p_value_label = tidy_p_value(p.value, sig)),
      aes(label = p_value_label),
      parse = T,
      x = -Inf, y = Inf,
      vjust = 1.5, hjust = -0.2
    ) +
    facet_wrap(~com_idx_name,
      ncol = 1, scales = "free_y",
      strip.position = "left",
      labeller = labeller(com_idx_name = c(
        CTI = cti_lab,
        CPI = cpi_lab,
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
  if (experiment == "mwe") {
    p <- plot_community_index_mwe(exp_tbl)
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
    select(any_of(c("site", "year", "plot", "treat_T", "tmp_com_mean", "ppt_com_mean", "cwd_com_mean"))) %>%
    gather(key = "com_idx_name", value = "com_idx_value", -site, -year, -plot, -treat_T) %>%
    mutate(com_idx_name = factor(com_idx_name,
      levels = c("tmp_com_mean", "ppt_com_mean", "cwd_com_mean"),
      labels = c("CTI", "CPI", "CDI")
    ))

  change_tbl <- jrgce_tbl %>%
    mutate(phase = case_when(
      year <= 2002 ~ "Phase I",
      year >= 2010 ~ "Phase III",
      TRUE ~ "Phase II"
    )) %>%
    rename(
      trt = treat_T,
      value = com_idx_value
    ) %>%
    group_by(com_idx_name, phase) %>%
    nest() %>%
    rowwise() %>%
    mutate(summary = test_index_change_model(dat_model = data, option = "exp") %>%
      test_change_summ()) %>%
    unnest(summary) %>%
    select(-data) %>%
    mutate(delta = estimate %>% signif(3)) %>%
    mutate(sig = if_else(str_detect(sig, "\\*"), sig, "ns")) %>%
    mutate(unit = case_when(
      com_idx_name == "CTI" ~ "°C",
      com_idx_name == "CPI" ~ "mm",
      com_idx_name == "CDI" ~ "mm",
    )) %>%
    mutate(start = case_when(
      phase == "Phase I" ~ 1999,
      phase == "Phase II" ~ 2003,
      phase == "Phase III" ~ 2010
    )) %>%
    mutate(end = case_when(
      phase == "Phase I" ~ 2002,
      phase == "Phase II" ~ 2009,
      phase == "Phase III" ~ 2014
    )) %>%
    left_join(
      jrgce_tbl %>%
        group_by(com_idx_name) %>%
        summarise(max = max(com_idx_value)),
      by = "com_idx_name"
    ) %>%
    mutate(max = case_when(
      com_idx_name == "CTI" ~ max + 0.1,
      com_idx_name == "CPI" ~ max + 20,
      com_idx_name == "CDI" ~ max + 20
    ))

  warm_tbl <- read_warm_treatment()

  exp_gg <-
    ggplot(jrgce_tbl) +
    geom_rect( # warming phases
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
    geom_segment(
      data = change_tbl,
      aes(x = start, xend = end, y = max, yend = max)
    ) +
    geom_text(
      data = change_tbl %>%
        mutate(p_value_label = tidy_p_value(p.value, sig)),
      aes(
        label = p_value_label,
        x = (start + end) / 2, y = Inf
      ),
      parse = T,
      vjust = 1.5
    ) +
    scale_y_continuous(expand = expansion(mult = .1)) +
    scale_x_continuous(
      expand = expansion(mult = 0, add = c(0.125, 0.125)),
      breaks = c(change_tbl$start, change_tbl$end) %>% unique()
    ) +
    labs(
      x = NULL,
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
        label = phase,
        x = startyear - 0.25,
      ),
      y = 17,
      hjust = 0,
    ) +
    coord_cartesian(clip = "off") +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside",
      legend.position = "none",
      plot.margin = unit(c(2, 1, 1, 1), "lines")
    )

  return(exp_gg)
}

plot_community_index_jrgce_watering <- function(exp_tbl) {
  # JRGCE data
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
    mutate(subgrp = NA) %>%
    group_by(com_idx_name) %>%
    nest() %>%
    rowwise() %>%
    mutate(summary = test_index_change_model(dat_model = data, option = "exp") %>%
      test_change_summ()) %>%
    unnest(summary) %>%
    select(-data) %>%
    mutate(delta = estimate %>% signif(3)) %>%
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
      com_idx_name == "CTI" ~ max + 0.1,
      com_idx_name == "CPI" ~ max + 20
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
    geom_segment(
      data = change_tbl,
      aes(x = start, xend = end, y = max, yend = max)
    ) +
    geom_text(
      data = change_tbl %>%
        mutate(p_value_label = tidy_p_value(p.value, sig)),
      aes(
        label = p_value_label,
        x = (start + end) / 2, y = Inf
      ),
      parse = T,
      vjust = 1.5
    ) +
    scale_y_continuous(expand = expansion(mult = .1)) +
    scale_x_continuous(
      expand = expansion(mult = 0, add = c(0.125, 0.125)),
      breaks = c(change_tbl$start, change_tbl$end) %>% unique()
    ) +
    labs(
      x = NULL,
      y = NULL,
      color = "Watering treatment",
    ) +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside",
      legend.position = "none",
      plot.margin = unit(c(2, 1, 1, 1), "lines"),
      panel.background = element_rect(fill = "#F0F8FF")
    )

  return(exp_water_gg)
}

plot_community_index_mwe <- function(exp_tbl) {
  mwe_tbl <- exp_tbl %>%
    filter(site == "mwe", year > 2015) %>%
    separate(treat, c("treat", "soil"), sep = 2) %>%
    select(site, year, plot, treat, soil, tmp_com_mean, ppt_com_mean) %>%
    pivot_longer(cols = tmp_com_mean:ppt_com_mean, names_to = "com_idx_name", values_to = "com_idx_value") %>%
    mutate(com_idx_name = factor(com_idx_name,
      levels = c("tmp_com_mean", "ppt_com_mean"),
      labels = c("CTI", "CPI")
    ))
  mwe_gg <-
    plot_mwe(mwe_tbl, l_tag = "A", trt_tag = "Watering", s_tag = "Serpentine soil") +
    plot_mwe(mwe_tbl, l_tag = "B", trt_tag = "Watering", s_tag = "Non-serpentine soil") +
    plot_mwe(mwe_tbl, l_tag = "C", trt_tag = "Drought", s_tag = "Serpentine soil") +
    plot_layout(design = "
  ABC
")

  return(mwe_gg)
}

plot_mwe <- function(mwe_tbl, l_tag = "A", trt_tag = "Watering", s_tag = "Serpentine soil") {
  # filter data by treatment tag
  if (trt_tag == "Watering") {
    mwe_tbl <- filter(mwe_tbl, treat %in% c("_X", "WX"))
    box_col <- "blue"
    bg_col <- "#F0F8FF"
  } else if (trt_tag == "Drought") {
    mwe_tbl <- filter(mwe_tbl, treat %in% c("X_", "XD"))
    box_col <- "brown"
    bg_col <- "#FFFFE0"
  } else {
    stop("Unknown treatment tag")
  }

  # filter data by soil tag
  if (s_tag == "Serpentine soil") {
    mwe_tbl <- filter(mwe_tbl, soil == "S")
  } else if (s_tag == "Non-serpentine soil") {
    mwe_tbl <- filter(mwe_tbl, soil == "N")
  } else {
    stop("Unknown soil tag")
  }

  change_tbl <- mwe_tbl %>%
    rename(
      trt = treat,
      value = com_idx_value,
      subgrp = soil
    ) %>%
    group_by(com_idx_name) %>%
    nest() %>%
    rowwise() %>%
    mutate(summary = test_index_change_model(dat_model = data, option = "exp") %>%
      test_change_summ()) %>%
    unnest(summary) %>%
    select(-data) %>%
    mutate(delta = estimate %>% signif(3)) %>%
    mutate(sig = if_else(str_detect(sig, "\\*"), sig, "ns")) %>%
    mutate(unit = case_when(
      com_idx_name == "CTI" ~ "°C",
      com_idx_name == "CPI" ~ "mm",
      com_idx_name == "CDI" ~ "mm",
    )) %>%
    mutate(
      start = mwe_tbl %>% pull(year) %>% min(),
      end = mwe_tbl %>% pull(year) %>% max()
    ) %>%
    left_join(
      mwe_tbl %>%
        group_by(com_idx_name) %>%
        summarise(max = max(com_idx_value)),
      by = "com_idx_name"
    ) %>%
    mutate(max = case_when(
      com_idx_name == "CTI" ~ max + 0.1,
      com_idx_name == "CPI" ~ max + 20
    ))

  p <-
    ggplot(mwe_tbl) +
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
    geom_segment(
      data = change_tbl,
      aes(x = start, xend = end, y = max, yend = max)
    ) +
    geom_text(
      data = change_tbl %>%
        mutate(p_value_label = tidy_p_value(p.value, sig)),
      aes(
        label = p_value_label,
        x = (start + end) / 2, y = Inf
      ),
      parse = T,
      vjust = 1.5
    ) +
    scale_y_continuous(expand = expansion(mult = .1)) +
    scale_x_continuous(
      expand = expansion(mult = 0, add = c(0.125, 0.125)),
      breaks = c(change_tbl$start, change_tbl$end) %>% unique()
    ) +
    labs(
      x = NULL,
      y = NULL,
    ) +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside",
      legend.position = "none",
      plot.margin = unit(c(2, 1, 1, 1), "lines"),
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
      value = com_idx_value,
      subgrp = site
    ) %>%
    group_by(com_idx_name) %>%
    nest() %>%
    rowwise() %>%
    mutate(summary = test_index_change_model(dat_model = data, option = "exp") %>%
      test_change_summ()) %>%
    unnest(summary) %>%
    select(-data) %>%
    mutate(delta = estimate %>% signif(3)) %>%
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
      com_idx_name == "CTI" ~ max + 0.1,
      com_idx_name == "CPI" ~ max + 20
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
    geom_segment(
      data = change_tbl,
      aes(x = start, xend = end, y = max, yend = max)
    ) +
    geom_text(
      data = change_tbl %>%
        mutate(p_value_label = tidy_p_value(p.value, sig)),
      aes(
        label = p_value_label,
        x = (start + end) / 2, y = Inf
      ),
      parse = T,
      vjust = 1.5
    ) +
    scale_y_continuous(expand = expansion(mult = .1)) +
    scale_x_continuous(
      expand = expansion(mult = 0, add = c(0.125, 0.125)),
      breaks = c(change_tbl$start, change_tbl$end) %>% unique()
    ) +
    labs(
      x = NULL,
      y = NULL,
    ) +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside",
      legend.position = "none",
      plot.margin = unit(c(2, 1, 1, 1), "lines"),
      panel.background = element_rect(fill = bg_col)
    ) +
    ggtitle(str_c(l_tag, ". ", site_tag))

  return(p)
}
