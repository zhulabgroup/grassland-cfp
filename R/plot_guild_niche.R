#' @export
plot_guild_niche <- function(dat_guild_niche) {
  gg_guild_niche <-
    plot_guild_niche_group(dat_guild_niche, "Origin", "Set1") +
    plot_guild_niche_group(dat_guild_niche, "Life history", "Set2") +
    plot_guild_niche_group(dat_guild_niche, "Functional group", "Accent") +
    plot_annotation(tag_levels = "A") +
    plot_layout(design = "
  ABC
  ")

  return(gg_guild_niche)
}

plot_guild_niche_group <- function(dat_guild_niche, g, pal) {
  p <- ggplot(data = dat_guild_niche %>%
    filter(guild != "DUMMY") %>%
    filter(guild != "Unknown") %>%
    filter(group == g)) +
    geom_point(
      aes(
        x = tmp,
        y = ppt,
        color = guild
      ),
      alpha = 0.5
    ) +
    stat_ellipse(
      aes(
        x = tmp,
        y = ppt,
        color = guild
      )
    ) +
    scale_color_brewer(palette = pal) +
    labs(x = "Mean annual temperature (°C)", y = "Annual precipitation (mm)") +
    theme(
      legend.position = c(0.25, 0.15),
      legend.background = element_rect(fill = "transparent"),
      legend.title = element_blank()
    )

  return(p)
}
