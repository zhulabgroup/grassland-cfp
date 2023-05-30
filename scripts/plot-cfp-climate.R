# report stats
cfp_sf <- st_read(.path$geo_cfp, quiet = TRUE) %>%
  filter(
    NAME == "California Floristic Province",
    Type == "hotspot area"
  )
cfp_bbox <- sf::st_bbox(cfp_sf)

chelsa_ras <- terra::rast(c(
  str_c(.path$cli_chelsa, "bio1.tif"),
  str_c(.path$cli_chelsa, "bio12.tif")
))
names(chelsa_ras) <- c("tmp", "ppt")
chelsa_cfp_df <- chelsa_ras %>%
  terra::crop(cfp_sf) %>%
  terra::mask(cfp_sf) %>%
  as.data.frame() %>%
  summarise(
    t_min = min(tmp, na.rm = T),
    t_max = max(tmp, na.rm = T),
    p_min = min(ppt, na.rm = T),
    p_max = max(ppt, na.rm = T)
  )

sum_cfp_cc_func <- function(param) {
  ras <- terra::rast(str_c(.path$cli_chelsa_cfp_annual, param, "_trend.nc"))
  trend_ras <- ras[[1]]
  names(trend_ras) <- "trend"
  sig_ras <- ras[[3]]
  names(sig_ras) <- "p"
  trend_df <- trend_ras %>%
    terra::as.data.frame(xy = T) %>%
    cbind(sig_ras %>% terra::as.data.frame(xy = F)) %>%
    select(lon = x, lat = y, trend, p) %>%
    drop_na(trend) %>%
    summarise(
      mean = mean(trend, na.rm = T),
      median = median(trend, na.rm = T),
      se = sd(trend, na.rm = T) / sqrt(n()),
      lower = quantile(trend, 0.025, na.rm = T),
      upper = quantile(trend, 0.975, na.rm = T),
      sig_pos_percentage = sum(p <= 0.05 & trend > 0) / n() * 100,
      sig_neg_percentage = sum(p <= 0.05 & trend < 0) / n() * 100
    ) %>%
    mutate(param = param)
}
sum_cfp_cc <- bind_rows(sum_cfp_cc_func("tas"), sum_cfp_cc_func("pr"))

# plot
plot_cfp_cc <- function(param) {
  ras <- terra::rast(str_c(.path$cli_chelsa_cfp_annual, param, "_trend.nc"))
  trend_ras <- ras[[1]]
  names(trend_ras) <- "trend"
  sig_ras <- ras[[3]]
  names(sig_ras) <- "p"
  trend_df <- trend_ras %>%
    terra::as.data.frame(xy = T) %>%
    cbind(sig_ras %>% terra::as.data.frame(xy = F)) %>%
    select(lon = x, lat = y, trend, p) %>%
    mutate(rank = rank(trend, na.last = "keep")) %>%
    mutate(
      trend_sig = case_when(p <= 0.05 ~ trend),
      rank_sig = case_when(p <= 0.05 ~ rank)
    ) %>%
    arrange(trend)

  find_rank <- function(df, value) {
    rank <- rep(NA, length(value))
    for (i in 1:length(value)) {
      rank[i] <- which(df$trend >= value[i]) %>% min()
    }
    return(rank)
  }

  if (param == "tas") {
    title <- "Trend in mean temperature"
    unit <- expression("°C yr"^-1)
    colors <- c("blue", "white", "yellow", "red")
    colorbreaks <- c(min(trend_df$trend, na.rm = T), 0, 0.01, max(trend_df$trend, na.rm = T))
    values <- find_rank(trend_df, colorbreaks)
    values <- (values - min(values)) / (max(values) - min(values))
    labels <- c(0, 0.02, 0.04)
    breaks <- find_rank(trend_df, labels)
  }
  # if (param == "vpd") {
  #   title <- "Trend in maximum vapor pressure deficit"
  #   unit <- expression("Pa yr"^-1)
  # }
  if (param == "pr") {
    title <- "Trend in total precipitation"
    unit <- expression("mm yr"^-1)
    colors <- c("#b18b38", "#f1db95", "#FFFFFF", "#8FD8CB", "#1eb196")
    colorbreaks <- c(min(trend_df$trend, na.rm = T), -5, 0, 1, max(trend_df$trend, na.rm = T))
    values <- find_rank(trend_df, colorbreaks)
    values <- (values - min(values)) / (max(values) - min(values))
    labels <- c(-10, -5, -2, 0)
    breaks <- find_rank(trend_df, labels)
  }

  p <- ggplot() +
    geom_tile(data = trend_df, aes(x = lon, y = lat, fill = rank), alpha = 1) +
    geom_contour(
      data = trend_df %>% mutate(p = ifelse(is.na(p), 999, p)),
      aes(x = lon, y = lat, z = p),
      color = "black", linewidth = 0.25,
      breaks = c(0.05)
    ) +
    scale_fill_gradientn(
      colours = colors,
      values = values,
      na.value = NA,
      breaks = breaks,
      labels = labels
    ) +
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
    filename = str_c(.path$out_fig, "fig-supp-cfp-climate.png"),
    width = 9,
    height = 9 * .618,
    device = png, type = "cairo"
  )
}
