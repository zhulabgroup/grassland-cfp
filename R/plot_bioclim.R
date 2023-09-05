plot_bioclim <- function(dat_trait_bioclim, var) {
  if (var == "tmp") {
    dat_trait_bioclim_sub <- dat_trait_bioclim %>%
      select(
        bio1, # Annual Mean Temperature
        bio8, # Mean Temperature of Wettest Quarter
        bio9, # Mean Temperature of Driest Quarter
        bio10, # Mean Temperature of Warmest Quarter
        bio11 # Mean Temperature of Coldest Quarter
      )
    unit <- "(°C)"
  }

  if (var == "ppt") {
    dat_trait_bioclim_sub <- dat_trait_bioclim %>%
      select(
        bio12, # Annual Precipitation
        bio13, # Precipitation of Wettest Month
        bio16, # Precipitation of Wettest Quarter
        bio19 # Precipitation of Coldest Quarter
      )
    unit <- "(mm)"
  }

  names(dat_trait_bioclim_sub) <- toupper(names(dat_trait_bioclim_sub)) %>%
    str_c(unit, sep = " ")

  gg_bioclim <-
    dat_trait_bioclim_sub %>%
    GGally::ggpairs(
      lower = list(
        continuous = "hexbin",
        combo = "facethist", discrete = "facetbar", na = "na"
      )
    ) +
    scale_fill_viridis_c() +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside"
    )

  return(gg_bioclim)
}

ggally_hexbin <- function(data, mapping, ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_hex(...)
  p
}
