#' @export
plot_effect_size <- function(df_index_change_exp) {
  df <- df_index_change_exp %>%
    mutate(factor = factor(trt,
      levels = c("Warming", "Watering", "Drought"),
      labels = c("temperature", "precipitation", "precipitation")
    )) %>%
    mutate(direction = factor(trt,
      levels = c("Warming", "Watering", "Drought"),
      labels = c("Increased", "Increased", "Decreased")
    )) %>%
    arrange(factor, direction) %>%
    mutate(manipulation = str_c(direction, factor, sep = " ")) %>%
    mutate(manipulation = factor(manipulation, levels = (.) %>% pull(manipulation) %>% unique())) %>%
    mutate(exp = exp %>% toupper()) %>%
    arrange(exp, grp) %>%
    mutate(expgrp = str_c(exp, grp, sep = "\n")) %>%
    mutate(expgrp = factor(expgrp, levels = (.) %>% pull(expgrp) %>% unique())) %>%
    mutate(sig = case_when(
      str_detect(sig, "\\*") ~ "sig",
      TRUE ~ sig
    ))

  p <- df %>%
    ggplot() +
    geom_point(
      aes(
        x = manipulation,
        y = estimate,
        group = interaction(exp, grp)
      ),
      position = position_dodge(width = 0.6),
      col = NA
    ) +
    geom_rect( # warming phrases
      data = data.frame(
        start = c(0.5, 1.5, 2.5),
        end = c(1.5, 2.5, 3.5),
        tag = c("warming", "water", "drought")
      ),
      aes(xmin = start, xmax = end, fill = tag),
      ymin = -Inf, ymax = Inf, alpha = 1,
      show.legend = F
    ) +
    scale_fill_manual(values = c("warming" = "#FFD580", "water" = "#F0F8FF", "drought" = "#FFFFE0")) +
    geom_point(
      aes(
        x = manipulation,
        y = estimate,
        group = interaction(exp, grp)
      ),
      position = position_dodge(width = 0.6)
    ) +
    geom_errorbar(
      aes(
        x = manipulation,
        ymin = estimate - 1.95 * std.error,
        ymax = estimate + 1.95 * std.error,
        group = interaction(exp, grp),
        alpha = sig
      ),
      width = 0,
      position = position_dodge(width = 0.6)
    ) +
    ggrepel::geom_label_repel(
      aes(
        x = manipulation,
        y = estimate,
        label = expgrp,
        group = interaction(exp, grp),
        alpha = sig
      ),
      position = position_dodge(width = 0.6),
      size = 3,
      color = "black",
      fill = NA,
      min.segment.length = 0,
      max.overlaps = Inf,
      label.padding = unit(.25, "lines"),
      label.size = NA,
      show.legend = FALSE
    ) +
    scale_alpha_manual(values = c("ns" = 0.5, "sig" = 1)) +
    scale_x_discrete(expand = expansion(add = c(0, 0))) +
    facet_wrap(. ~ index,
      ncol = 1,
      scales = "free_y",
      strip.position = "left",
      labeller = labeller(index = c(
        cti = "Community Temperature Index\n(CTI, °C)",
        cpi = "Community Precipitation Index\n(CPI, mm)"
      ))
    ) +
    geom_hline(yintercept = 0, lty = 2) +
    labs(
      x = "Manipulation",
      y = NULL,
      alpha = "Significance"
    ) +
    theme(
      legend.position = "bottom",
      strip.placement = "outside"
    )

  return(p)
}
