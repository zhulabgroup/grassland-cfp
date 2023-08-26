plot_data_availability <- function(df_data_avail) {
  gg_data_avail <- ggplot() +
    geom_segment(
      data = df_data_avail %>%
        group_by(sitename) %>%
        summarise(min = min(year), max = max(year)),
      aes(x = min, xend = max, y = sitename, yend = sitename), linewidth = 3, col = "dark green", alpha = 0.4
    ) +
    geom_point(data = df_data_avail, aes(x = year, y = sitename, cex = count), pch = 19) +
    scale_size("Sample size", range = c(2, 4), breaks = c(10, 50, 100)) +
    xlab("") +
    ylab("") +
    scale_y_discrete(limits = rev)

  return(gg_data_avail)
}
