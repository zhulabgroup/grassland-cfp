#' @export
plot_trait_cwd <- function(dat_trait_cwd,
                           cool_species = "Danthonia californica",
                           warm_species = "Stipa pulchra") {
  df <- dat_trait_cwd %>%
    mutate(species_type = case_when(
      species == cool_species ~ "cool",
      species == warm_species ~ "warm",
      TRUE ~ "other"
    ))

  tmp_cwd_gg <-
    ggplot(
      data = df %>%
        filter(
          tmp > quantile(tmp, .0001, na.rm = T), tmp < quantile(tmp, .9999, na.rm = T), # remove extreme climate points for plotting
          cwd > quantile(cwd, .0001, na.rm = T), cwd < quantile(cwd, .9999, na.rm = T)
        ),
      mapping = aes(tmp, cwd)
    ) +
    geom_point(
      aes(color = species_type, shape = species_type, alpha = species_type)
    ) +
    scale_color_manual(values = c(cool = "blue", other = gray(.5), warm = "red")) +
    scale_shape_manual(values = c(cool = 19, other = 20, warm = 19)) +
    scale_alpha_manual(values = c(cool = .6, other = .01, warm = .6)) +
    labs(
      x = "Mean annual temperature (°C)",
      y = "Annual climate water deficit (mm)"
    ) +
    guides(color = "none", shape = "none", alpha = "none")

  ppt_cwd_gg <-
    ggplot(
      data = df %>%
        filter(
          ppt > quantile(ppt, .0001, na.rm = T), ppt < quantile(ppt, .9999, na.rm = T), # remove extreme climate points for plotting
          cwd > quantile(cwd, .0001, na.rm = T), cwd < quantile(cwd, .9999, na.rm = T)
        ),
      mapping = aes(ppt, cwd)
    ) +
    geom_point(
      aes(color = species_type, shape = species_type, alpha = species_type)
    ) +
    scale_color_manual(values = c(cool = "blue", other = gray(.5), warm = "red")) +
    scale_shape_manual(values = c(cool = 19, other = 20, warm = 19)) +
    scale_alpha_manual(values = c(cool = .6, other = .01, warm = .6)) +
    labs(
      x = "Annual precipitation (mm)",
      y = "Annual climate water deficit (mm)"
    ) +
    guides(color = "none", shape = "none", alpha = "none")

  tmp_ppt_gg <-
    ggplot(
      data = df %>%
        filter(
          ppt > quantile(ppt, .0001, na.rm = T), ppt < quantile(ppt, .9999, na.rm = T), # remove extreme climate points for plotting
          tmp > quantile(tmp, .0001, na.rm = T), tmp < quantile(tmp, .9999, na.rm = T)
        ),
      mapping = aes(tmp, ppt)
    ) +
    geom_point(
      aes(color = species_type, shape = species_type, alpha = species_type)
    ) +
    scale_color_manual(values = c(cool = "blue", other = gray(.5), warm = "red")) +
    scale_shape_manual(values = c(cool = 19, other = 20, warm = 19)) +
    scale_alpha_manual(values = c(cool = .6, other = .01, warm = .6)) +
    labs(
      x = "Mean annual temperature (°C)",
      y = "Annual precipitation (mm)"
    ) +
    guides(color = "none", shape = "none", alpha = "none")

  # combine panels
  gg_trait_cwd <- ppt_cwd_gg + tmp_cwd_gg + tmp_ppt_gg +
    plot_annotation(tag_levels = "A") +
    plot_layout(design = "
  ABC
  ")

  return(gg_trait_cwd)
}
