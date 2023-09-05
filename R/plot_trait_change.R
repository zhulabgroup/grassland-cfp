plot_trait_change <- function(option, dat_community, dat_niche) {
  if (option == "obs") {
    gg_trait_change <- plot_trait_change_obs(dat_community_obs = dat_community$obs, dat_niche)
  }
  if (option == "exp") {
    gg_trait_change <- plot_trait_change_exp(dat_community_exp = dat_community$exp, dat_niche)
  }

  return(gg_trait_change)
}


plot_trait_change_exp <- function(dat_community_exp, dat_niche) {
  df_weight <- dat_community_exp %>%
    filter(site == "jrgce", year >= 1999) %>%
    select(-site) %>%
    mutate(trt = str_sub(treat, start = 1L, end = 1L)) %>%
    select(year, plot, trt, species, abund) %>%
    # spread(key = "species", value = "abund", fill = 0) %>%
    # gather(key = "species", value = "abund",  -year,- plot, -trt) %>%
    group_by(trt, plot, year) %>%
    mutate(weight = abund / sum(abund)) %>% # convert all abundance (absolute or relative) to percentage, such that all plots get equal weight later in lme
    ungroup() %>%
    select(-abund) %>%
    left_join(
      dat_niche %>%
        select(species,
          tmp = tmp_occ_median,
          ppt = ppt_occ_median
        ),
      by = "species"
    ) %>%
    gather(key = "trait", value = "value", -year, -trt, -plot, -species, -weight) %>%
    mutate(trait = factor(trait, levels = c("tmp", "ppt")))

  df_stack <- tidy_stack_weighted_data(df_weight)

  # df_weight_mean <- df_weight %>%
  #   group_by(year, trt, species, trait, value) %>%
  #   summarise(weight = mean(weight)) %>%
  #   ungroup()

  change_tbl <- df_weight %>%
    mutate(grp = case_when(
      year <= 2002 ~ "Phase I",
      year >= 2010 ~ "Phase III",
      TRUE ~ "Phase II"
    )) %>%
    group_by(grp, trait) %>%
    nest() %>%
    mutate(test_trait_change_model(dat_lme = data[[1]], option = "exp") %>%
      test_change_summ()) %>%
    select(-data) %>%
    mutate(sig = if_else(str_detect(sig, "\\*"), sig, "ns")) %>%
    mutate(unit = case_when(
      trait == "tmp" ~ "°C",
      trait == "ppt" ~ "mm"
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
      df_weight %>%
        group_by(trait) %>%
        summarise(max = max(value)),
      by = "trait"
    ) %>%
    mutate(max = case_when(
      trait == "tmp" ~ max + 0.5,
      trait == "ppt" ~ max + 100
    ))

  warm_tbl <- read_warm_treatment()

  exp_gg <-
    ggplot() +
    geom_rect( # warming phrases
      data = warm_tbl,
      aes(xmin = start - .5, xmax = end + .5, fill = tag),
      ymin = -Inf, ymax = Inf, alpha = 0.5
    ) +
    scale_fill_gradient(low = "antiquewhite", high = "orange") +
    geom_boxplot(
      data = df_stack,
      aes(x = year, y = value, col = trt, group = interaction(trt, year)),
      color = "black", fill = NA, outlier.shape = 20, outlier.alpha = 0, coef = 0
    ) +
    geom_jitter(
      data = df_weight %>%
        mutate(year = case_when(
          trt == "_" ~ year - 0.2,
          trt == "T" ~ year + 0.2
        )),
      aes(x = year, y = value, col = trt, size = weight),
      alpha = 0.02
    ) +
    scale_color_manual(values = c("_" = "black", "T" = "red")) +
    facet_wrap( # CTI & CPI panels
      ~trait,
      ncol = 1, scales = "free_y",
      strip.position = "left",
      labeller = labeller(trait = c(
        tmp = "Mean annual temperature\n(MAT, °C)",
        ppt = "Annual precipitation\n(AP, mm)"
      ))
    ) +
    geom_text(data = change_tbl, aes(x = end, y = max, label = str_c("effect size: ", estimate %>% signif(3), unit, sig, sep = " ")), hjust = 1) +
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
          trait = factor("tmp",
            levels = c("tmp", "ppt")
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

plot_trait_change_obs <- function(dat_community_obs, dat_niche) {
  sf_cfp <- read_cfp(path_cfp = system.file("extdata", "cfp", package = "grassland"))
  ras_grass <- read_grasscover(path_grass = system.file("extdata", "cfp-grassland-percent-cover.tif", package = "grassland"))

  site_map_gg <- plot_site_map(sf_cfp, ras_grass)

  obs_gg <-
    site_map_gg +
    plot_trait(dat_community_obs, dat_niche, "angelo", tmp_lab = "MAT (°C)", ppt_lab = "AP (mm)") +
    plot_trait(dat_community_obs, dat_niche, "carrizo") +
    plot_trait(dat_community_obs, dat_niche, "elkhorn", tmp_lab = "MAT (°C)", ppt_lab = "AP (mm)") +
    plot_trait(dat_community_obs, dat_niche, "jasper") +
    plot_trait(dat_community_obs, dat_niche, "mclann", tmp_lab = "MAT (°C)", ppt_lab = "AP (mm)") +
    plot_trait(dat_community_obs, dat_niche, "mclserp") +
    plot_trait(dat_community_obs, dat_niche, "morganterritory") +
    plot_trait(dat_community_obs, dat_niche, "pleasantonridge") +
    plot_trait(dat_community_obs, dat_niche, "sunol", tmp_lab = "MAT (°C)", ppt_lab = "AP (mm)", yr_axis = TRUE) +
    plot_trait(dat_community_obs, dat_niche, "swanton", yr_axis = TRUE) +
    plot_trait(dat_community_obs, dat_niche, "ucsc", yr_axis = TRUE) +
    plot_trait(dat_community_obs, dat_niche, "vascocaves", yr_axis = TRUE) +
    plot_layout(design = "
  AABC
  AADE
  FGHI
  JKLM
")
  return(obs_gg)
}

plot_trait <- function(dat_community_obs, dat_niche, site_name, tmp_lab = "", ppt_lab = "", yr_lab = NULL, yr_axis = FALSE) {
  # prepare site data
  site_lab <- read_site_name()[site_name]
  site_lab <- str_c(LETTERS[which(read_site_name() == site_lab)], site_lab, sep = ". ")

  df_weight <- dat_community_obs %>%
    filter(site == site_name) %>%
    group_by(plot, year) %>%
    mutate(weight = abund / sum(abund)) %>% # convert all abundance (absolute or relative) to percentage, such that all plots get equal weight later in lme
    ungroup() %>%
    select(year, plot, species, weight) %>%
    left_join(
      dat_niche %>%
        select(species,
          tmp = tmp_occ_median,
          ppt = ppt_occ_median
        ),
      by = "species"
    ) %>%
    gather(key = "var", value = "value", -year, -plot, -species, -weight) %>%
    mutate(var = factor(var, levels = c("tmp", "ppt"))) %>%
    group_by(var) %>%
    nest() %>%
    mutate( # lm and p value
      p_val = map(data, ~ lm(value ~ year, data = ., weights = .$weight)) %>%
        map_dbl(~ broom::glance(.) %>% pull(p.value))
    ) %>%
    unnest(cols = data)

  n_plot <- dat_community_obs %>%
    filter(site == site_name) %>%
    distinct(plot) %>%
    nrow()

  df_stack <- tidy_stack_weighted_data(df_weight)

  # plot
  set.seed(1)
  out_gg <- ggplot() +
    geom_jitter(
      data = df_weight %>%
        group_by(var) %>%
        filter(
          value >= quantile(value, 0.025, na.rm = T),
          value <= quantile(value, 0.975, na.rm = T)
        ) %>%
        ungroup(),
      aes(x = year, y = value, size = weight),
      pch = 21, alpha = 0.1 / (n_plot), fill = "blue", color = "blue"
    ) +
    geom_boxplot(
      data = df_stack,
      aes(x = year, y = value, group = year),
      color = "#5A5A5A", fill = NA, outlier.shape = 20, outlier.alpha = 0, coef = 0
    ) +
    geom_smooth( # add lm trend line when significant
      data = df_weight %>% filter(p_val <= 0.05),
      aes(x = year, y = value, weight = weight),
      method = "lm", formula = y ~ x, se = FALSE,
      color = "red"
    ) +
    facet_wrap(~var,
      ncol = 1, scales = "free_y",
      strip.position = "left",
      labeller = labeller(var = c(
        tmp = tmp_lab, # "Mean annual temperature (°C)",
        ppt = ppt_lab # "Annual precipitation (mm)"
      ))
    ) +
    expand_limits(x = range(dat_community_obs$year)) +
    labs(
      x = yr_lab, y = NULL,
      title = site_lab
    ) +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside",
      plot.title = element_text(size = 11),
      legend.position = "none"
    )

  # remove yr axis text
  if (yr_axis) {
    return(out_gg)
  } else {
    return(out_gg + theme(axis.text.x = element_blank()))
  }
}

tidy_stack_weighted_data <- function(df_weight) {
  df_stack <- df_weight %>%
    mutate(repeat_count = (weight * 1000) %>% round()) %>%
    rowwise() %>%
    slice(rep(seq_len(n()), each = repeat_count)) %>%
    select(-repeat_count) %>%
    ungroup()

  return(df_stack)
}
