plot_niche_thin <- function(dat_niche, dat_niche_thin) {
  gg_niche_thin <- plot_niche_thin_var(dat_niche, dat_niche_thin, var = "tmp") +
    plot_niche_thin_var(dat_niche, dat_niche_thin, var = "ppt")
  return(gg_niche_thin)
}

plot_niche_thin_var <- function(dat_niche, dat_niche_thin, var) {
  df_niche_compare <- inner_join(
    dat_niche %>%
      select(species, full = !!sym(str_c(var, "_occ_median"))),
    dat_niche_thin %>%
      select(species, thin = !!sym(str_c(var, "_occ_median"))),
    by = "species"
  )
  var_lab <- case_when(
    var == "tmp" ~ "temperature (°C)",
    var == "ppt" ~ "precipitation (mm)"
  )
  p <-
    ggplot(df_niche_compare, aes(full, thin)) +
    geom_point(alpha = .5) +
    geom_smooth(method = "lm", formula = y ~ x - 1, se = FALSE, color = "red", lty = "dashed") +
    ggpubr::stat_cor(
      aes(label = paste(after_stat(r.label), after_stat(p.label), sep = "*`,`~")),
      p.accuracy = 0.05,
      color = "red"
    ) +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    labs(
      x = str_c("Full data median ", var_lab),
      y = str_c("Thinned data median ", var_lab)
    ) +
    theme(
      aspect.ratio = 1,
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )
}
