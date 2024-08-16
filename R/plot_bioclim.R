#' @export
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
      ),
      upper = list(
        continuous = "corrlabel"
      )
    ) +
    scale_fill_viridis_c() +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside"
    )

  return(gg_bioclim)
}

#' @export
ggally_hexbin <- function(data, mapping, ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_hex(...)

  return(p)
}

#' @export
ggally_corrlabel <- function(data, mapping, ...) {
  # Extract the data
  x <- GGally::eval_data_col(data, mapping$x)
  y <- GGally::eval_data_col(data, mapping$y)

  # Calculate correlation and p-value
  cor_test <- cor.test(x, y)
  r_value <- cor_test$estimate
  p_value <- cor_test$p.value

  # Format the p-value using tidy_p_value or similar
  p_value_label <- tidy_p_value(p_value)

  # Create the label text
  label1 <- paste0("italic(R) == ", round(r_value, 3))
  label2 <- p_value_label

  # Return as a plot
  ggplot() +
    geom_text(aes(x = 0, y = 0.25, label = label1), parse = T, col = "black", ...) +
    geom_text(aes(x = 0, y = -0.25, label = label2), parse = T, col = "black", ...) +
    ylim(-1, 1)
}
