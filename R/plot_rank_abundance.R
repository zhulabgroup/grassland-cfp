plot_rank_abundance <- function(dat_rank) {
  out <- list(
    obs = plot_rank_abundance_obs(dat_rank$obs),
    exp = plot_rank_abundance_exp(dat_rank$exp)
  )

  return(out)
}

plot_rank_abundance_obs <- function(dat_rank_obs) {
  obs_rank_gg <- ggplot(dat_rank_obs) +
    geom_line(aes(x = rank, y = median, col = year, group = year), alpha = 1) +
    # geom_ribbon(aes(x = rank, ymin = lower, ymax = upper, fill = year, group = year), alpha = 0.25) +
    facet_wrap(. ~ site) +
    scale_color_viridis_c() +
    scale_fill_viridis_c() +
    facet_wrap(. ~ site,
      scales = "free",
      labeller = read_site_name() %>% as_labeller()
    ) +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside",
      axis.text = element_text(size = 8),
      strip.text = element_text(hjust = 0)
    ) +
    labs(
      x = "Rank",
      y = "Abundance",
      col = "Year"
    ) +
    guides(fill = "none")

  return(obs_rank_gg)
}
plot_rank_abundance_exp <- function(dat_rank_exp) {
  exp_rank_gg <- ggplot(dat_rank_exp) +
    geom_line(aes(x = rank, y = median, col = treat_T, group = treat_T), alpha = 1) +
    geom_ribbon(aes(x = rank, ymin = lower, ymax = upper, fill = treat_T, group = treat_T), alpha = 0.25) +
    facet_wrap(. ~ site) +
    facet_wrap(. ~ phaseyear,
      scales = "free",
      nrow = 3,
      drop = FALSE
    ) +
    scale_color_manual(values = c("black", "red")) +
    scale_fill_manual(values = c("black", "red")) +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside",
      axis.text = element_text(size = 8),
      strip.text = element_text(hjust = 0),
      legend.position = "none"
    ) +
    labs(
      x = "Rank",
      y = "Abundance",
      col = "Warming treatment"
    )

  return(exp_rank_gg)
}
