#' @export
plot_guild_percentage <- function(option, dat_community) {
  if (option == "obs") {
    gg_guild_percentage <- plot_guild_percentage_obs(dat_community_obs = dat_community$obs)
  }
  if (option == "exp") {
    gg_guild_percentage <- plot_guild_percentage_jrgce_warming(dat_community_exp = dat_community$exp)
  }

  return(gg_guild_percentage)
}

#' @export
plot_guild_percentage_obs <- function(dat_community_obs) {
  obs_guild_tbl <- dat_community_obs %>%
    mutate(
      native = str_sub(guild, 1, 1),
      annual = str_sub(guild, 2, 2),
      grass = str_sub(guild, 3, 3)
    ) %>%
    select(-guild) %>%
    group_by(site, year, plot) %>%
    summarise(
      native_perc = sum(abund * (native == "N")) / sum(abund),
      annual_perc = sum(abund * (annual == "A")) / sum(abund),
      grass_perc = sum(abund * (grass == "G")) / sum(abund)
    ) %>%
    ungroup()

  sf_cfp <- read_cfp(path_cfp = system.file("extdata", "cfp", package = "grassland"))
  ras_grass <- read_grasscover(path_grass = system.file("extdata", "cfp-grassland-percent-cover.tif", package = "grassland"))

  site_map_gg <- plot_site_map(sf_cfp, ras_grass)

  obs_guild_gg <-
    site_map_gg +
    plot_guild_site(obs_guild_tbl, "angelo", native_lab = "% Natives", annual_lab = "% Annuals", grass_lab = "% Grasses") +
    plot_guild_site(obs_guild_tbl, "carrizo") +
    plot_guild_site(obs_guild_tbl, "elkhorn", native_lab = "% Natives", annual_lab = "% Annuals", grass_lab = "% Grasses") +
    plot_guild_site(obs_guild_tbl, "jasper") +
    plot_guild_site(obs_guild_tbl, "mclann", native_lab = "% Natives", annual_lab = "% Annuals", grass_lab = "% Grasses") +
    plot_guild_site(obs_guild_tbl, "mclserp") +
    plot_guild_site(obs_guild_tbl, "morganterritory") +
    plot_guild_site(obs_guild_tbl, "pleasantonridge") +
    plot_guild_site(obs_guild_tbl, "sunol", native_lab = "% Natives", annual_lab = "% Annuals", grass_lab = "% Grasses", yr_axis = TRUE) +
    plot_guild_site(obs_guild_tbl, "swanton", yr_axis = TRUE) +
    plot_guild_site(obs_guild_tbl, "ucsc", yr_axis = TRUE) +
    plot_guild_site(obs_guild_tbl, "vascocaves", yr_axis = TRUE) +
    plot_layout(design = "
  AABC
  AADE
  FGHI
  JKLM
")

  return(obs_guild_gg)
}

#' @export
plot_guild_site <- function(data, site_name,
                            native_lab = "", annual_lab = "", grass_lab = "", yr_lab = NULL, yr_axis = FALSE) {
  # prepare site data

  samp_size_tbl <- data %>%
    filter(site == site_name) %>%
    summarize(
      n_plot = n_distinct(plot),
      n_year = n_distinct(year),
      n = n_distinct(plot, year)
    )

  site_lab <- plot_site_name(site_name, with_letter = T)

  site_tbl <- data %>%
    filter(site == site_name) %>%
    select(site, year, plot, native_perc, annual_perc, grass_perc) %>%
    pivot_longer(native_perc:grass_perc, names_to = "group", values_to = "value") %>%
    mutate(group = factor(group,
      levels = c("native_perc", "annual_perc", "grass_perc")
    )) %>%
    group_by(group) %>%
    nest() %>%
    mutate(
      map(data, ~ lm(value ~ year, data = .)) %>%
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
  out_gg <-
    ggplot(site_tbl) +
    geom_boxplot(
      aes(x = year, y = value, group = year),
      color = "gray", outlier.shape = 20
    ) +
    geom_smooth(
      data = . %>% filter(p.value <= 0.05),
      aes(x = year, y = value),
      method = "lm", formula = y ~ x, se = FALSE,
      color = "red"
    ) +
    geom_text(
      data = . %>% distinct(group, .keep_all = T) %>%
        mutate(p_value_label = tidy_p_value(p.value, sig)),
      aes(
        label = p_value_label # ,
        # alpha = ifelse(p.value <= 0.05, "sig", "ns")
      ),
      parse = T,
      x = -Inf, y = Inf,
      vjust = 1.5, hjust = -0.2
    ) +
    # scale_alpha_manual(values = c("ns" = 0.5, "sig" = 1)) +
    facet_wrap(~group,
      ncol = 1, scales = "free_y",
      strip.position = "left",
      labeller = labeller(group = c(native_perc = native_lab, annual_perc = annual_lab, grass_perc = grass_lab))
    ) +
    expand_limits(x = c(min(data$year) - 1, max(data$year + 1))) +
    scale_y_continuous(
      labels = function(x) {
        trans <- x * 100
      },
      limits = c(0, 1.4),
      # expand = expansion(mult = .05) # expand padding to show significance tests
    ) +
    labs(
      x = yr_lab, y = NULL,
      title = site_lab,
      subtitle = bquote(
        "(" * italic(n[p]) * " = " * .(samp_size_tbl$n_plot) * ", " * italic(n[t]) * " = " * .(samp_size_tbl$n_year) * ", " * italic("n") * " = " * .(samp_size_tbl$n) * ")"
      )
    ) +
    theme(
      legend.position = "none",
      strip.background = element_blank(),
      strip.placement = "outside",
      plot.title = element_text(size = 11, hjust = 0),
      plot.subtitle = element_text(size = 11, hjust = 0)
    ) +
    guides(alpha = "none")

  # remove yr axis text
  if (yr_axis) {
    return(out_gg)
  } else {
    return(out_gg + theme(axis.text.x = element_blank()))
  }
}

plot_guild_percentage_jrgce_warming <- function(dat_community_exp) {
  exp_guild_tbl <- dat_community_exp %>%
    filter(site == "jrgce", year >= 1999) %>%
    filter(guild != "DUMMY") %>%
    mutate(
      native = str_sub(guild, 1, 1),
      annual = str_sub(guild, 2, 2),
      grass = str_sub(guild, 3, 3)
    ) %>%
    select(-guild) %>%
    group_by(site, year, plot, treat) %>%
    summarize(
      native_perc = sum(abund * (native == "N")) / sum(abund),
      annual_perc = sum(abund * (annual == "A")) / sum(abund),
      grass_perc = sum(abund * (grass == "G")) / sum(abund)
    ) %>%
    mutate(treat_T = str_sub(treat, start = 1L, end = 1L)) %>%
    select(site, year, plot, treat_T, native_perc, annual_perc, grass_perc) %>%
    pivot_longer(cols = native_perc:grass_perc, names_to = "group", values_to = "value") %>%
    mutate(group = factor(group,
      levels = c("native_perc", "annual_perc", "grass_perc")
    )) %>%
    ungroup()

  change_tbl <- exp_guild_tbl %>%
    mutate(phase = case_when(
      year <= 2002 ~ "Phase I",
      year >= 2010 ~ "Phase III",
      TRUE ~ "Phase II"
    )) %>%
    rename(
      trt = treat_T,
      value = value
    ) %>%
    group_by(phase, group) %>%
    nest() %>%
    rowwise() %>%
    mutate(summary = test_index_change_model(dat_model = data, option = "exp") %>%
      test_change_summ()) %>%
    unnest(summary) %>%
    select(-data) %>%
    mutate(delta = estimate %>% signif(3)) %>%
    mutate(sig = if_else(str_detect(sig, "\\*"), sig, "ns")) %>%
    mutate(unit = "%") %>%
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
    mutate(max = 1.2)

  # change_tbl_year <- exp_guild_tbl %>%
  #   mutate(grp = year) %>%
  #   rename(
  #     trt = treat_T,
  #     value = value
  #   ) %>%
  #   group_by(grp, group) %>%
  #   nest() %>%
  #   mutate(test_index_change_model(dat_model = data[[1]], option = "exp") %>%
  #     test_change_summ()) %>%
  #   select(-data) %>%
  #   mutate(sig = if_else(str_detect(sig, "\\*"), sig, "ns")) %>%
  #   mutate(max = 1.1)

  warm_tbl <- read_warm_treatment()

  exp_guild_gg <-
    ggplot(exp_guild_tbl) +
    geom_rect( # warming phrases
      data = warm_tbl,
      aes(xmin = start - .5, xmax = end + .5, fill = tag),
      ymin = -Inf, ymax = Inf, alpha = 0.5
    ) +
    scale_fill_gradient(low = "antiquewhite", high = "orange") +
    geom_boxplot( # treatment effects
      aes(x = year, y = value, col = treat_T, group = interaction(treat_T, year))
    ) +
    scale_color_manual(values = c("black", "red")) +
    facet_wrap(
      ~group,
      ncol = 1, scales = "free_y",
      strip.position = "left",
      labeller = labeller(group = c(
        native_perc = "% Natives",
        annual_perc = "% Annuals",
        grass_perc = "% Grasses"
      ))
    ) +
    geom_text(
      data = change_tbl %>%
        mutate(p_value_label = tidy_p_value(p.value, sig)),
      aes(
        label = p_value_label,
        x = (start + end) / 2, y = Inf # ,
        # alpha = ifelse(p.value <= 0.05, "sig", "ns")
      ),
      parse = T,
      vjust = 1.5
    ) +
    geom_segment(
      data = change_tbl,
      aes(x = start, xend = end, y = max, yend = max)
    ) +
    # scale_alpha_manual(values = c("ns" = 0.5, "sig" = 1)) +
    # geom_text(
    #   data = change_tbl_year,
    #   aes(x = grp, y = Inf, label = sig),
    #   vjust = 2
    # ) +
    scale_y_continuous(
      labels = function(x) {
        trans <- x * 100
      },
      limits = c(0, 1.3),
      breaks = c(0, 0.5, 1),
      expand = expansion(mult = 0.05)
    ) + # expand padding to show significance tests
    scale_x_continuous(
      expand = expansion(mult = 0, add = c(0.125, 0.125)),
      breaks = c(change_tbl$start, change_tbl$end) %>% unique()
    ) +
    labs(
      x = NULL, # "Year",
      y = NULL,
      color = "Warming treatment",
    ) +
    geom_text(
      data = warm_tbl %>%
        mutate(
          group = factor("native_perc",
            levels = c("native_perc", "annual_perc", "grass_perc")
          )
        ),
      aes(
        label = phase,
        x = startyear - 0.25,
      ),
      y = 1.45, # manually label phase text
      # parse = TRUE,
      hjust = 0,
    ) +
    coord_cartesian(clip = "off") +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside",
      legend.position = "none",
      plot.margin = unit(c(2, 1, 1, 1), "lines") # expand margin to include warming labels
    )

  return(exp_guild_gg)
}
