### plot guild in niche space
guild_niche_tbl <- read_rds(.path$sum_niche) %>%
  filter(occ_n > 100 | is.na(occ_n)) %>% # species with many observations and dummy species
  select(species, tmp_occ_median, ppt_occ_median) %>%
  left_join(
    bind_rows(
      read_rds(.path$com_exp) %>%
        select(species, guild),
      read_rds(.path$com_obs) %>%
        select(species, guild)
    ) %>%
      distinct(species, guild),
    by = "species"
  ) %>%
  filter(!is.na(guild)) %>% # why is there NA?
  mutate(
    native = str_sub(guild, 1, 1),
    annual = str_sub(guild, 2, 2),
    grass = str_sub(guild, 3, 3)
  ) %>%
  select(-guild) %>%
  mutate(native = factor(native,
    levels = c("N", "E", "D"),
    labels = c("Native", "Exotic", "DUMMY")
  )) %>%
  mutate(annual = factor(annual,
    levels = c("P", "A", "U"),
    labels = c("Perennial", "Annual", "DUMMY")
  )) %>%
  mutate(grass = factor(grass,
    levels = c("F", "S", "G", "R", "T", "U", "M"),
    labels = c("Forb", "Shrub", "Grass", "Rush", "Tree", "Unknown", "DUMMY")
  )) %>%
  gather(key = "group", value = "guild", -species, -tmp_occ_median, -ppt_occ_median) %>%
  mutate(group = factor(group,
    levels = c("native", "annual", "grass"),
    labels = c("Origin", "Life history", "Functional group")
  )) %>% 
  mutate(guild = factor(guild,
                        levels = c("Native", "Exotic",
                                   "Annual", "Perennial",
                                   "Grass", "Forb"
                                   )))

plot_guild_niche <- function(g, pal) {
  p <- ggplot(data = guild_niche_tbl %>%
    filter(guild != "DUMMY") %>%
    filter(guild != "Unknown") %>%
    filter(group == g)) +
    geom_point(
      aes(
        x = tmp_occ_median,
        y = ppt_occ_median,
        color = guild
      ),
      alpha = 0.5
    ) +
    stat_ellipse(
      aes(
        x = tmp_occ_median,
        y = ppt_occ_median,
        color = guild
      )
    ) +
    scale_color_brewer(palette = pal) +
    labs(x = "Mean annual temperature (°C)", y = "Mean annual precipitation (mm)") +
    theme(
      legend.position = c(0.2, 0.2),
      legend.title = element_blank()
    )

  return(p)
}

guild_niche_gg <-
  plot_guild_niche("Origin", "Set1") +
  plot_guild_niche("Life history", "Set2") +
  plot_guild_niche("Functional group", "Accent") +
  plot_annotation(tag_levels = "A") +
  plot_layout(design = "
  ABC
  ")

# save figure
if (.fig_save) {
  ggsave(
    plot = guild_niche_gg,
    filename = str_c(.path$out_fig, "fig-supp-guild-niche.png"),
    width = 12,
    height = 5
  )
}

### changes at experimental site
# summarize percentage of natives, annuals, and grasses
exp_guild_tbl <- read_rds(.path$com_exp) %>%
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

# warming phrases
warm_tbl <- tribble(
  ~tag, ~name, ~start, ~end, ~startyear,
  1, "Phase~I:~+80~W~m^-2%~~%+1~degree*C", -Inf, 2002, 1999,
  2, "Phase~II:~+100~W~m^-2%~~%+1.5~degree*C", 2003, 2009, 2003,
  3, "Phase~III:~+250~W~m^-2%~~%+2~degree*C", 2010, Inf, 2010 # end in 2014, but set to Inf to fill space
)

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
  ggpubr::stat_compare_means( # significance
    aes(x = year, y = value, group = treat_T),
    method = "wilcox.test",
    label = "p.signif", hide.ns = FALSE
  ) +
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
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = .15)) + # expand padding to show significance tests
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

# save figure
if (.fig_save) {
  ggsave(
    plot = exp_guild_gg,
    filename = str_c(.path$out_fig, "fig-supp-guild-exp.png"),
    width = 11,
    height = 6.18 * 1.5
  )
}

### changes at observational sites
# make a map for observational sites and grassland percent cover
source("scripts/plot-site-map.R")

# summarize percentage of natives, annuals, and grasses
obs_guild_tbl <- read_rds(.path$com_obs) %>%
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

# define plotting function
plot_guild <- function(data, site_abbr,
                       native_lab = "", annual_lab = "", grass_lab = "", yr_lab = NULL, yr_axis = FALSE) {
  # prepare site data
  site_lab <- site_vec[site_abbr]
  site_lab <- str_c(LETTERS[which(site_vec == site_lab)], site_lab, sep = ". ")

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
      color = "gray", outlier.shape = 20
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
    ylim(0, 1) +
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

obs_guild_gg <-
  site_map_gg +
  plot_guild(obs_guild_tbl, "angelo", native_lab = "% Natives", annual_lab = "% Annuals", grass_lab = "% Grasses") +
  plot_guild(obs_guild_tbl, "carrizo") +
  plot_guild(obs_guild_tbl, "elkhorn", native_lab = "% Natives", annual_lab = "% Annuals", grass_lab = "% Grasses") +
  plot_guild(obs_guild_tbl, "jasper") +
  plot_guild(obs_guild_tbl, "mclann", native_lab = "% Natives", annual_lab = "% Annuals", grass_lab = "% Grasses") +
  plot_guild(obs_guild_tbl, "mclserp") +
  plot_guild(obs_guild_tbl, "morganterritory") +
  plot_guild(obs_guild_tbl, "pleasantonridge") +
  plot_guild(obs_guild_tbl, "sunol", native_lab = "% Natives", annual_lab = "% Annuals", grass_lab = "% Grasses", yr_axis = TRUE) +
  plot_guild(obs_guild_tbl, "swanton", yr_axis = TRUE) +
  plot_guild(obs_guild_tbl, "ucsc", yr_axis = TRUE) +
  plot_guild(obs_guild_tbl, "vascocaves", yr_axis = TRUE) +
  plot_layout(design = "
  AABC
  AADE
  FGHI
  JKLM
")

# save figure
if (.fig_save) {
  ggsave(
    plot = obs_guild_gg,
    filename = str_c(.path$out_fig, "fig-supp-guild-obs.png"),
    width = 11,
    height = 11 * 1.5
  )
}

# for slides
obs_guild_landsc_gg <- 
  plot_guild(obs_guild_tbl, "angelo", native_lab = "% Natives", annual_lab = "% Annuals", grass_lab = "% Grasses") +
  plot_guild(obs_guild_tbl, "carrizo") +
  plot_guild(obs_guild_tbl, "elkhorn") +
  plot_guild(obs_guild_tbl, "jasper") +
  plot_guild(obs_guild_tbl, "mclann") +
  plot_guild(obs_guild_tbl, "mclserp") +
  plot_guild(obs_guild_tbl, "morganterritory", native_lab = "% Natives", annual_lab = "% Annuals", grass_lab = "% Grasses", yr_axis = TRUE) +
  plot_guild(obs_guild_tbl, "pleasantonridge", yr_axis = TRUE) +
  plot_guild(obs_guild_tbl, "sunol", yr_axis = TRUE) +
  plot_guild(obs_guild_tbl, "swanton", yr_axis = TRUE) +
  plot_guild(obs_guild_tbl, "ucsc", yr_axis = TRUE) +
  plot_guild(obs_guild_tbl, "vascocaves", yr_axis = TRUE) +
  plot_layout(nrow = 2)

if (.fig_save) {
  ggsave(
    plot = obs_guild_landsc_gg,
    filename = str_c(.path$out_fig, "fig-slide-guild-obs.png"),
    width = 9.32 * 1.75,
    height = 3.74 * 1.75
  )
}

## permanova
response <- guild_niche_tbl %>%
  filter(group =="Origin") %>% 
  select(tmp_occ_median, ppt_occ_median)
predict <- guild_niche_tbl %>%
  filter(group =="Origin") %>% 
  select(guild)
vegan::adonis2(response ~ guild, data = predict)

response <- guild_niche_tbl %>%
  filter(group =="Life history") %>% 
  select(tmp_occ_median, ppt_occ_median)
predict <- guild_niche_tbl %>%
  filter(group =="Life history") %>% 
  select(guild)
vegan::adonis2(response ~ guild, data = predict)

response <- guild_niche_tbl %>%
  filter(group =="Functional group") %>% 
  filter(guild %in% c("Forb", "Grass")) %>% 
  select(tmp_occ_median, ppt_occ_median)
predict <- guild_niche_tbl %>%
  filter(group =="Functional group") %>% 
  filter(guild %in% c("Forb", "Grass")) %>% 
  select(guild)
vegan::adonis2(response ~ guild, data = predict)
