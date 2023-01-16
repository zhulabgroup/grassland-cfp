# report stats
sum_cfp_cc_func <- function(param) {
  trend_ras <- raster::stack(str_c(.path$cli_chelsa_cfp_annual, param, "_trend.nc"))[[1]] # slope
  sig_ras <- raster::stack(str_c(.path$cli_chelsa_cfp_annual, param, "_trend.nc"))[[3]] # p value
  trend_df <- trend_ras %>%
    raster::as.data.frame(xy = T) %>%
    cbind(sig_ras %>% raster::as.data.frame(xy = F)) %>%
    select(lon = x, lat = y, trend = X1, p = X3) %>%
    drop_na(trend) %>%
    summarise(
      mean = mean(trend, na.rm = T),
      median = median(trend, na.rm = T),
      se = sd(trend, na.rm = T) / sqrt(n()),
      lower = quantile(trend, 0.025, na.rm = T),
      upper = quantile(trend, 0.975, na.rm = T),
      sig_pos_percentage = sum(p < 0.05 & trend > 0) / n(),
      sig_neg_percentage = sum(p < 0.05 & trend < 0) / n()
    ) %>%
    mutate(param = param)
}
sum_cfp_cc <- bind_rows(sum_cfp_cc_func("tas"), sum_cfp_cc_func("pr"))

# plot
plot_cfp_cc <- function(param) {
  trend_ras <- raster::stack(str_c(.path$cli_chelsa_cfp_annual, param, "_trend.nc"))[[1]] # slope
  sig_ras <- raster::stack(str_c(.path$cli_chelsa_cfp_annual, param, "_trend.nc"))[[3]] # p value
  trend_df <- trend_ras %>%
    raster::as.data.frame(xy = T) %>%
    cbind(sig_ras %>% raster::as.data.frame(xy = F)) %>%
    select(lon = x, lat = y, trend = X1, p = X3) %>%
    # mutate(param = param) %>%
    mutate(`significant trend` = case_when(p < 0.05 ~ trend)) %>%
    select(-p) %>%
    gather(key = "group", value = "value", -lon, -lat)

  if (param == "tas") {
    title <- "Trend in mean temperature"
    unit <- expression("°C year"^-1)
    highcol <- "red"
    lowcol <- "blue"
    midcol <- "yellow"
  }
  if (param == "vpd") {
    title <- "Trend in maximum vapor pressure deficit"
    unit <- expression("Pa"^-1)
    highcol <- "red"
    lowcol <- "blue"
    midcol <- "white"
  }
  if (param == "pr") {
    title <- "Trend in total precipitation"
    unit <- expression("mm year"^-1)
    highcol <- "#00cfa9"
    lowcol <- "#e9a000"
    midcol <- "white" # "#f1db95"
  }

  p <- ggplot(trend_df) +
    geom_tile(aes(x = lon, y = lat, fill = value), alpha = 1) +
    facet_wrap(. ~ group, ncol = 1) +
    scale_fill_gradient2(low = lowcol, mid = midcol, high = highcol, midpoint = 0, na.value = NA) +
    geom_sf(
      data = rnaturalearth::ne_states(
        country = c("Mexico", "United States of America"),
        returnclass = "sf"
      ),
      fill = NA,
      color = gray(.25)
    ) +
    labs(fill = unit) +
    # ggtitle(title) +
    coord_sf(xlim = c(-126, -114), ylim = c(28, 44)) +
    scale_x_continuous(breaks = c(-125, -120, -115)) +
    scale_y_continuous(breaks = c(30, 35, 40)) +
    labs(x = "Longitude", y = "Latitude") +
    theme(legend.position = "bottom")

  return(p)
}

cfp_clim_gg <-
  plot_cfp_cc(param = "tas") +
  plot_cfp_cc(param = "pr") +
  # plot_cfp_cc(param="vpd") +
  plot_layout(ncol = 2, nrow = 1) +
  plot_annotation(tag_levels = "A")

# save figure file
if (.fig_save) {
  ggsave(
    plot = cfp_clim_gg,
    filename = str_c(.path$out_fig, "fig-cfp-climate.png"),
    width = 6,
    height = 9
  )
}
