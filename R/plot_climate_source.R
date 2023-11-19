#' @export
plot_climate_source <- function(dat_trait_chelsa, dat_trait_prism, dat_trait_terraclim) {
  clim_comp_tbl <- bind_rows(
    dat_trait_chelsa %>% mutate(source = "CHELSA"),
    dat_trait_prism %>% mutate(source = "PRISM"),
    dat_trait_terraclim %>% mutate(source = "TerraClim")
  ) %>%
    gather(key = "variable", value = "value", -key, -species, -source) %>%
    spread(key = "source", value = "value") %>%
    filter(!variable %in% c("vpd", "cwd"))

  chelsa_prism_gg <- ggplot(clim_comp_tbl, aes(CHELSA, PRISM)) +
    geom_hex(bins = 100) +
    viridis::scale_fill_viridis() +
    geom_smooth(method = "lm", formula = y ~ x - 1, se = FALSE, color = "red", lty = "dashed") +
    ggpubr::stat_cor(
      aes(label = str_c(after_stat(r.label), after_stat(p.label), sep = "*`,`~")),
      p.accuracy = 0.05,
      color = "red"
    ) +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    facet_wrap(
      . ~ factor(
        variable,
        levels = c("tmp", "ppt"),
        labels = c(
          "Mean annual temperature (°C)",
          "Annual precipitation (mm)"
        )
      ),
      scales = "free"
    ) +
    labs(
      x = "",
      y = "PRISM",
      # fill = "Occurrence record"
    ) +
    guides(fill = "none") +
    theme(
      aspect.ratio = 1, # can't do + coord_fixed() with facet_wrap(scales = "free")
      strip.background = element_blank(),
      strip.text.x = element_text(size = 12)
    )

  chelsa_terraclim_gg <- ggplot(clim_comp_tbl, aes(CHELSA, TerraClim)) +
    geom_hex(bins = 100) +
    viridis::scale_fill_viridis() +
    geom_smooth(method = "lm", formula = y ~ x - 1, se = FALSE, color = "red", lty = "dashed") +
    ggpubr::stat_cor(
      aes(label = str_c(after_stat(r.label), after_stat(p.label), sep = "*`,`~")),
      p.accuracy = 0.05,
      color = "red"
    ) +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    facet_wrap(
      . ~ factor(
        variable,
        levels = c("tmp", "ppt"),
        labels = c(
          "Mean annual temperature (°C)",
          "Annual precipitation (mm)"
        )
      ),
      scales = "free"
    ) +
    labs(
      x = "CHELSA",
      y = "TerraClimate",
      # fill = "Occurrence record"
    ) +
    guides(fill = "none") +
    theme(
      aspect.ratio = 1, # can't do + coord_fixed() with facet_wrap(scales = "free")
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )

  clim_comp_gg <- chelsa_prism_gg / chelsa_terraclim_gg

  return(clim_comp_gg)
}
