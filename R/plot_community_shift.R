#' Plot Community Shift
#'
#' This function creates plots showing community shifts measured by CTI and CPI in the climate niche space across observational sites and in the JRGCE warming experiment.
#'
#' @param dat_shift A list that contains community shift data.
#' @param dat_niche A data frame containing species climate niche.
#'
#' @return A list containing ggplot objects of different plots, including community shifts across observational sites, community shifts in the JRGCE warming experiment, comparison of community shifts between observations and experiments, communities in the climate niche space, and a combined plot.
#' @examples
#' \dontrun{
#' p_community_shift <- plot_community_shift(dat_shift, dat_niche)
#' }
#' @export
plot_community_shift <- function(dat_shift, dat_niche) {
  p_obs <- plot_community_shift_obs(obs_tbl = dat_shift$obs)
  p_exp <- plot_community_shift_exp(exp_tbl = dat_shift$exp)
  p_compare <- plot_community_shift_compare(dat_shift)
  p_niche <- plot_community_in_niche_space(dat_shift, dat_niche)

  p_compare_rect <- plot_add_rect(p_compare, p_exp, col = "orange", x_adj = 0.025, y_adj = 0.05, label = "D")
  p_niche_rect <- plot_add_rect(p_niche, p_compare, col = "black", x_adj = 0.1, y_adj = 0.05, label = "B")

  p_combined <- p_niche_rect + p_compare_rect + p_obs + p_exp +
    plot_annotation(tag_levels = "A") +
    plot_layout(design = "
  AABB
  AABB
  CCCC
  CCCC
  DDDD
  ")

  out <- list(
    obs = p_obs,
    exp = p_exp,
    compare = p_compare,
    niche = p_niche,
    combined = p_combined
  )

  return(out)
}

plot_community_shift_obs <- function(obs_tbl) {
  p_obs <- ggplot() +
    geom_point(
      data = obs_tbl$sum_fine,
      aes(x = m_CTI, y = m_CPI, col = year),
      alpha = 0.6
    ) +
    geom_segment(
      data = obs_tbl$shift,
      aes(
        x = CTI_start, xend = CTI_end, y = CPI_start, yend = CPI_end,
        alpha = significance
      ),
      arrow = arrow(length = unit(0.2, "cm")),
      linewidth = 0.8
    ) +
    scale_alpha_manual(values = c("sig" = 1, "ns" = 0.5)) +
    scale_color_viridis_c(
      option = "magma"
    ) +
    xlab("CTI (°C)") +
    ylab("CPI (mm)") +
    labs(
      alpha = "Significance",
      col = "Year"
    ) +
    theme(strip.text.x = element_text(hjust = 0)) +
    theme(legend.position = "bottom") +
    guides(color = guide_colorbar(barwidth = 10)) +
    facet_wrap(. ~ site %>% plot_site_name(with_letter = F), nrow = 2)

  return(p_obs)
}

plot_community_shift_exp <- function(exp_tbl) {
  warm_tbl <- read_warm_treatment()

  p_exp <- ggplot() +
    geom_rect( # warming phrases
      data = warm_tbl,
      aes(fill = tag),
      xmin = -Inf, xmax = Inf,
      ymin = -Inf, ymax = Inf,
      alpha = 0.5
    ) +
    scale_fill_gradient(low = "antiquewhite", high = "orange") +
    geom_point(
      data = exp_tbl$sum_fine,
      aes(x = m_CTI, y = m_CPI, col = treatment)
    ) +
    scale_color_manual(values = c("Ambient" = "black", "Warming" = "red")) +
    geom_segment(
      data = exp_tbl$shift,
      aes(
        x = CTI_, xend = CTIT, y = CPI_, yend = CPIT,
        group = phase,
        alpha = significance
      ),
      arrow = arrow(length = unit(0.2, "cm")),
      linewidth = 0.8
    ) +
    scale_alpha_manual(values = c("sig" = 1, "ns" = 0.25)) +
    facet_wrap(. ~ phase,
      nrow = 1
    ) +
    xlab("CTI (°C)") +
    ylab("CPI (mm)") +
    labs(
      alpha = "Significance",
      col = "Treatment"
    ) +
    guides(
      fill = "none"
    ) +
    theme(strip.text.x = element_text(hjust = 0)) +
    theme(legend.position = "bottom")

  return(p_exp)
}

plot_community_shift_compare <- function(dat_shift) {
  ## compare exp and obs
  df_all_shift <- bind_rows(
    dat_shift$obs$shift %>%
      select(site, CTI0 = CTI_start, CTI1 = CTI_end, CPI0 = CPI_start, CPI1 = CPI_end, significance) %>%
      mutate(group = "observation"),
    dat_shift$exp$shift %>%
      mutate(site = paste(site, year, sep = "_")) %>%
      select(site, CTI0 = CTI_, CTI1 = CTIT, CPI0 = CPI_, CPI1 = CPIT, significance) %>%
      mutate(group = "experiment")
  ) %>%
    mutate(group = factor(group,
      levels = c("observation", "experiment"),
      labels = c("Observation", "Experiment")
    ))

  cpi_to_cti <- df_all_shift %>%
    filter(significance == "sig") %>%
    mutate(slope = (CPI1 - CPI0) / (CTI1 - CTI0)) %>%
    mutate(scale_factor = 0.1) %>%
    mutate(slope = slope * scale_factor) %>%
    group_by(group) %>%
    summarise(
      mean = mean(slope),
      se = sd(slope) / sqrt(n()),
      scale_factor = unique(scale_factor)
    ) %>%
    ungroup()

  cti_to_cpi <- df_all_shift %>%
    filter(significance == "sig") %>%
    mutate(slope = (CTI1 - CTI0) / (CPI1 - CPI0)) %>%
    mutate(scale_factor = 10) %>%
    mutate(slope = slope * scale_factor) %>%
    group_by(group) %>%
    summarise(
      mean = mean(slope),
      se = sd(slope) / sqrt(n()),
      scale_factor = unique(scale_factor)
    ) %>%
    ungroup()

  p_compare <- ggplot(df_all_shift) +
    geom_segment(
      aes(
        x = CTI0, xend = CTI1, y = CPI0, yend = CPI1,
        group = site, col = group,
        alpha = significance
      ),
      arrow = arrow(length = unit(0.2, "cm")),
      linewidth = 1
    ) +
    scale_color_manual(values = c("Experiment" = "#e28a2b", "Observation" = "#384c6b")) +
    scale_alpha_manual(values = c("sig2" = 1, "sig1" = 0.25, "ns" = 0.25)) +
    guides(alpha = "none") +
    xlab("Community Temperature Index (CTI, °C)") +
    ylab("Community Precipitation Index (CPI, mm)") +
    theme(
      legend.title = element_blank(),
      legend.position = c(.2, .2),
      legend.text = element_text(size = rel(0.8))
    )

  return(p_compare)
}

plot_community_in_niche_space <- function(dat_shift, dat_niche) {
  df_all_sum <- bind_rows(
    dat_shift$obs$sum_coarse %>% mutate(group = "observation"),
    dat_shift$exp$sum_coarse %>% mutate(group = "experiment")
  ) %>%
    mutate(group = factor(group,
      levels = c("observation", "experiment"),
      labels = c("Observation", "Experiment")
    ))

  p_niche <- ggplot() +
    geom_point(
      data = dat_niche,
      aes(
        x = tmp_occ_median,
        y = ppt_occ_median
      ),
      col = gray(.75),
      alpha = 0.5
    ) +
    geom_point(
      data = df_all_sum,
      aes(x = m_CTI, y = m_CPI, col = group),
      size = 2.5, alpha = 0.75
    ) +
    scale_color_manual(values = c("Experiment" = "#e28a2b", "Observation" = "#384c6b")) +
    labs(x = "Mean annual temperature (°C)", y = "Annual precipitation (mm)") +
    theme(
      legend.title = element_blank(),
      legend.position = c(.2, .2),
      legend.text = element_text(size = rel(0.8))
    )

  return(p_niche)
}

plot_add_rect <- function(p_big, p_small, col = "orange", x_adj = 0.025, y_adj = 0.05, label = "D") {
  p_rect <- p_big +
    geom_rect(
      aes(
        xmin = layer_scales(p_small)$x$range$range[1],
        xmax = layer_scales(p_small)$x$range$range[2],
        ymin = layer_scales(p_small)$y$range$range[1],
        ymax = layer_scales(p_small)$y$range$range[2]
      ),
      fill = NA,
      linetype = 3,
      color = col
    ) +
    geom_text(
      aes(
        x = layer_scales(p_small)$x$range$range[2] + x_adj,
        y = layer_scales(p_small)$y$range$range[2] + y_adj
      ),
      label = label,
      hjust = 0,
      vjust = 0,
      color = col
    )

  return(p_rect)
}
