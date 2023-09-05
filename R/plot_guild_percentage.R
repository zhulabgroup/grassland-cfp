plot_guild_percentage <- function(option, dat_community) {
  if (option == "obs") {
    gg_guild_percentage <- plot_guild_percentage_obs(dat_community_obs = dat_community$obs)
  }
  if (option == "exp") {
    gg_guild_percentage <- plot_guild_percentage_jrgce_warming(dat_community_exp = dat_community$exp)
  }

  return(gg_guild_percentage)
}

plot_guild_percentage_obs <- function(dat_community_obs) {
  obs_guild_tbl <- dat_community_obs %>%
    mutate(
      native = str_sub(guild, 1, 1),
      annual = str_sub(guild, 2, 2),
      grass = str_sub(guild, 3, 3)
    ) %>%
    select(-guild) %>%
    group_by(abbr = site, year, plot) %>%
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

plot_guild_site <- function(data, site_abbr,
                            native_lab = "", annual_lab = "", grass_lab = "", yr_lab = NULL, yr_axis = FALSE) {
  # prepare site data
  site_lab <- read_site_name()[site_abbr]
  site_lab <- str_c(LETTERS[which(read_site_name() == site_lab)], site_lab, sep = ". ")

  site_tbl <- data %>%
    filter(abbr == site_abbr) %>%
    select(abbr, year, plot, native_perc, annual_perc, grass_perc) %>%
    pivot_longer(native_perc:grass_perc, names_to = "group", values_to = "value") %>%
    mutate(group = factor(group,
      levels = c("native_perc", "annual_perc", "grass_perc")
    )) %>%
    group_by(group) %>%
    nest() %>%
    mutate(
      p_val = map(data, ~ lm(value ~ year, data = .)) %>%
        map_dbl(~ broom::glance(.) %>% pull(p.value))
    ) %>%
    unnest(cols = data)

  # plot
  out_gg <-
    ggplot(site_tbl) +
    geom_boxplot(
      aes(x = year, y = value, group = year),
      color = "#5A5A5A", outlier.shape = 20
    ) +
    geom_smooth(
      data = . %>% filter(p_val <= 0.05),
      aes(x = year, y = value),
      method = "lm", formula = y ~ x, se = FALSE,
      color = "red"
    ) +
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
      limits = c(0, 1)
    ) +
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
    ))

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
    # ggpubr::stat_compare_means( # significance
    #   aes(x = year, y = value, group = treat_T),
    #   method = "wilcox.test",
    #   label = "p.signif", hide.ns = FALSE
    # ) +
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
    scale_y_continuous(
      labels = function(x) {
        trans <- x * 100
      },
      limits = c(0, 1), expand = expansion(mult = .15)
    ) + # expand padding to show significance tests
    scale_x_continuous(expand = expansion(mult = 0, add = c(0.125, 0.125))) +
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
        label = name,
        x = startyear - 0.25,
      ),
      y = 1.2, # manually label phase text
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
}
