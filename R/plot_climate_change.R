plot_climate_change_cfp <- function(indir = "alldata/intermediate/background/cfp_annual/") {
  gg_cfp_cc <-
    plot_cfp_cc_param(indir = indir, param = "tas") +
    plot_cfp_cc_param(indir = indir, param = "pr") +
    plot_layout(ncol = 2, nrow = 1) +
    plot_annotation(tag_levels = "A")

  return(gg_cfp_cc)
}

plot_cfp_cc_param <- function(indir, param) {
  ras <- terra::rast(str_c(indir, param, "_trend.nc"))
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

find_rank <- function(df, value) {
  rank <- rep(NA, length(value))
  for (i in 1:length(value)) {
    rank[i] <- which(df$trend >= value[i]) %>% min()
  }
  return(rank)
}
