#' @export
plot_niche_subset <- function(dat_niche, dat_niche_subset, type = "thin") {
  gg_niche_subset <- plot_niche_subset_var(dat_niche, dat_niche_subset, var = "tmp", type = type) +
    plot_niche_subset_var(dat_niche, dat_niche_subset, var = "ppt", type = type)
  return(gg_niche_subset)
}

plot_niche_subset_var <- function(dat_niche, dat_niche_subset, var, type = "thin") {
  df_niche_compare <- inner_join(
    dat_niche %>%
      filter(!is.na(occ_n)) %>%
      select(species, full = !!sym(str_c(var, "_occ_median"))),
    dat_niche_subset %>%
      filter(!is.na(occ_n)) %>%
      select(species, subset = !!sym(str_c(var, "_occ_median"))),
    by = "species"
  )
  var_lab <- case_when(
    var == "tmp" ~ "temperature (°C)",
    var == "ppt" ~ "precipitation (mm)"
  )

  p <-
    ggplot(df_niche_compare, aes(full, subset)) +
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
      y = str_c(
        case_when(
          type == "thin" ~ "Thinned",
          type == "early" ~ "Early"
        ),
        " data median ", var_lab
      )
    ) +
    theme(
      aspect.ratio = 1,
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )

  return(p)
}
