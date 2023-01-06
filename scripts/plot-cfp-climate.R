plot_cfp_cc <- function(param) {
  trend_ras <- raster::stack(str_c(.path$cli_chelsa_cfp_annual, param, "_trend.nc"))[[1]] # slope
  sig_ras <- raster::stack(str_c(.path$cli_chelsa_cfp_annual, param, "_trend.nc"))[[3]] # p value
  trend_df <- trend_ras %>%
    raster::as.data.frame(xy = T) %>%
    cbind(sig_ras %>% raster::as.data.frame(xy = F)) %>%
    select(lon = x, lat = y, trend = X1, p = X3) %>%
    mutate(param = param) %>%
    mutate(trend_sig = case_when(p < 0.05 ~ trend))

  if (param == "tas") {
    title <- "Trend in mean temperature"
    unit <- expression("°C/year")
    highcol <- "red"
    lowcol <- "blue"
    midcol <- "yellow"
  }
  if (param == "vpd") {
    title <- "Trend in maximum vapor pressure deficit"
    unit <- expression("Pa")
    highcol <- "red"
    lowcol <- "blue"
    midcol <- "white"
  }
  if (param == "pr") {
    title <- "Trend in total precipitation"
    unit <- expression("mm/year")
    highcol <- "#1eb196"
    lowcol <- "#b18b38"
    midcol <- "#f1db95"
  }

  p <- ggplot(trend_df) +
    geom_tile(aes(x = lon, y = lat, fill = trend_sig), alpha = 1) +
    geom_tile(aes(x = lon, y = lat, fill = trend), alpha = 0.75) +
    scale_fill_gradient2(low = lowcol, mid = midcol, high = highcol, midpoint = 0, na.value = NA) +
    geom_sf(
      data = rnaturalearth::ne_states(
        country = c("Mexico", "United States of America"),
        returnclass = "sf"
      ),
      fill = NA,
      color = gray(.25)
    ) +
    theme_void() +
    labs(fill = unit) +
    ggtitle(title) +
    coord_sf(xlim = c(-126, -114), ylim = c(28, 44)) +
    theme(legend.position = "bottom")

  return(p)
}

cfp_clim_gg <-
  plot_cfp_cc(param = "tas") +
  plot_cfp_cc(param = "pr") +
  # plot_cfp_cc(param="vpd") +
  plot_layout(ncol = 2, nrow = 1)
