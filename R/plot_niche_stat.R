plot_niche_stat <- function(dat_niche, var) {
  gg_niche_stat <-
    dat_niche %>%
    filter(occ_n > 100) %>%
    select(
      !!sym(str_c(var, ("_occ_mean"))),
      !!sym(str_c(var, ("_occ_median"))),
      !!sym(str_c(var, ("_occ_q05"))),
      !!sym(str_c(var, ("_occ_q95")))
    ) %>%
    GGally::ggpairs(
      lower = list(continuous = GGally::wrap("points", alpha = 0.2)),
      # title = "Species temperature niche (°C)",
      columnLabels = c("Mean (°C)", "Median (°C)", "Lower limit (5%, °C)", "Upper limit (95%, °C)"),
      switch = "both"
    ) +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside"
    )

  return(gg_niche_stat)
}
