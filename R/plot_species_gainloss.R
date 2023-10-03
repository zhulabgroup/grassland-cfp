plot_species_gainloss <- function(dat_niche, dat_gainloss) {
  p_obs <- plot_species_gainloss_obs(obs_tbl = dat_gainloss$obs, dat_niche)
  p_exp <- plot_species_gainloss_exp(exp_tbl = dat_gainloss$exp, dat_niche)

  p_combined <-
    p_obs$circle +
    p_exp$circle +
    p_obs$box +
    p_exp$box +
    plot_layout(design = "
  AAAAC
  AAAAC
  AAAAD
  BBBBD
") +
    plot_annotation(tag_levels = "A")

  out <- list(
    obs = p_obs,
    exp = p_exp,
    combined = p_combined
  )
  return(out)
}

plot_species_gainloss_obs <- function(obs_tbl, dat_niche) {
  obs_gainloss_tbl <- obs_tbl %>%
    mutate(species_sep = str_split(species, pattern = " ")) %>%
    rowwise() %>%
    mutate(species_short = str_c(str_sub(species_sep[1], 1, 3), str_sub(species_sep[2], 1, 3))) %>%
    select(-species_sep)

  obs_gainloss_eg1 <- obs_gainloss_tbl %>%
    group_by(species, change) %>%
    summarize(n = n()) %>%
    filter(change != "no clear change") %>%
    group_by(change) %>%
    arrange(desc(n)) %>%
    slice(1)

  obs_gainloss_eg2 <- obs_gainloss_tbl %>%
    group_by(species, complete_change) %>%
    summarize(n = n()) %>%
    filter(!is.na(complete_change)) %>%
    group_by(complete_change) %>%
    arrange(desc(n)) %>%
    slice(1)

  obs_gainloss_tbl_long <- obs_gainloss_tbl %>%
    pivot_longer(cols = tmp:ppt, names_to = "variable", values_to = "value") %>%
    mutate(variable = factor(variable,
      levels = c("tmp", "ppt")
    ))

  obs_gainloss_summ_gg <-
    ggpubr::ggboxplot(
      data = obs_gainloss_tbl_long %>%
        mutate(change = case_when(
          change == "increase" ~ "Increase",
          change == "decrease" ~ "Decrease",
          change == "no clear change" ~ "No change"
        )) %>%
        mutate(change = factor(change, levels = c("Increase", "Decrease", "No change"))),
      x = "change",
      col = "change",
      y = "value"
    ) +
    scale_color_manual(values = c(Increase = "dark green", `No change` = "lightgray", Decrease = "dark orange")) +
    ggpubr::stat_compare_means(
      method = "wilcox.test",
      label = "p.signif",
      hide.ns = FALSE,
      comparisons = list(c("Increase", "Decrease"), c("Increase", "No change"), c("Decrease", "No change")),
      size = 3
    ) + # Add pairwise comparisons p-value
    # ggpubr::stat_compare_means() +    # Add global p-value
    facet_wrap(. ~ variable,
      ncol = 1,
      scales = "free_y",
      strip.position = "left",
      labeller = labeller(variable = c(
        tmp = "Mean annual temperature (°C)",
        ppt = "Annual precipitation (mm)"
      ))
    ) +
    guides(col = "none") +
    labs(
      x = NULL,
      y = NULL
    ) +
    ggthemes::theme_few() +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside"
    ) +
    theme(
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 8)
    )

  obs_gainloss_main_gg <-
    ggplot() +
    geom_point(
      data = dat_niche,
      aes(
        x = tmp_occ_median,
        y = ppt_occ_median
      ), color = "gray", alpha = 0
    ) +
    geom_point(
      data = obs_gainloss_tbl,
      aes(
        x = tmp,
        y = ppt,
        size = dominance,
        color = change
      ), alpha = 1, pch = 21, fill = NA
    ) +
    geom_point(
      data = obs_gainloss_tbl %>% filter(change == "increase" | change == "decrease") %>% filter(!is.na(complete_change)),
      aes(
        x = tmp,
        y = ppt,
        size = dominance,
        color = change,
        fill = complete_change,
      ), alpha = 0.75, pch = 21
    ) +
    scale_color_manual(values = c(increase = "dark green", `no clear change` = "lightgray", decrease = "dark orange")) +
    scale_fill_manual(values = c(new = "dark green", lost = "dark orange")) +
    labs(x = "Mean annual temperature (°C)", y = "Annual precipitation (mm)") +
    guides(
      fill = "none",
      size = "none",
      color = "none"
    ) +
    theme(axis.text = element_text(size = 8)) +
    facet_wrap(. ~ site %>% plot_site_name(with_letter = F),
      nrow = 3
    ) +
    theme(strip.text = element_text(hjust = 0))

  obs_gainloss_supp_gg <- obs_gainloss_main_gg +
    ggrepel::geom_text_repel(
      data = obs_gainloss_tbl %>% filter(change == "increase" | change == "decrease") %>% filter(!is.na(complete_change)),
      aes(
        x = tmp,
        y = ppt,
        color = change,
        label = paste0("italic('", species_short, "')")
      ),
      size = 3.88 / 1.68,
      alpha = 1,
      max.overlaps = 100,
      parse = T
    ) +
    facet_wrap(. ~ site %>% plot_site_name(with_letter = F),
      nrow = 3
    ) +
    theme(strip.text = element_text(hjust = 0))

  out <- list(
    box = obs_gainloss_summ_gg,
    circle = obs_gainloss_main_gg,
    circle_detail = obs_gainloss_supp_gg
  )

  return(out)
}

plot_species_gainloss_exp <- function(exp_tbl, dat_niche) {
  exp_gainloss_tbl <- exp_tbl %>%
    mutate(change = factor(change, levels = c("increase", "decrease", "no clear change"))) %>%
    filter(year != 1998) %>%
    mutate(species_sep = str_split(species, pattern = " ")) %>%
    rowwise() %>%
    mutate(species_short = str_c(str_sub(species_sep[1], 1, 3), str_sub(species_sep[2], 1, 3))) %>%
    select(-species_sep) %>%
    mutate(phaseyear = factor(phaseyear,
      levels = c(
        "Phase I: 1999",
        "Phase I: 2000",
        "Phase I: 2001",
        "Phase I: 2002",
        " ",
        "  ",
        "   ",
        "Phase II: 2003",
        "Phase II: 2004",
        "Phase II: 2005",
        "Phase II: 2006",
        "Phase II: 2007",
        "Phase II: 2008",
        "Phase II: 2009",
        "Phase III: 2010",
        "Phase III: 2011",
        "Phase III: 2012",
        "Phase III: 2013",
        "Phase III: 2014",
        "    ",
        "     "
      )
    ))

  exp_gainloss_eg <- exp_gainloss_tbl %>%
    group_by(species, change) %>%
    summarize(n = n()) %>%
    filter(change != "no clear change") %>%
    group_by(change) %>%
    arrange(desc(n)) %>%
    slice(1)

  exp_gainloss_tbl_long <- exp_gainloss_tbl %>%
    pivot_longer(cols = tmp:ppt, names_to = "variable", values_to = "value") %>%
    mutate(variable = factor(variable,
      levels = c("tmp", "ppt")
    ))

  exp_gainloss_summ_gg <-
    ggpubr::ggboxplot(
      data = exp_gainloss_tbl_long %>%
        mutate(change = case_when(
          change == "increase" ~ "Increase",
          change == "decrease" ~ "Decrease",
          change == "no clear change" ~ "No change"
        )) %>%
        mutate(change = factor(change, levels = c("Increase", "Decrease", "No change"))),
      x = "change",
      col = "change",
      y = "value"
    ) +
    scale_color_manual(values = c(Increase = "dark green", `No change` = "lightgray", Decrease = "dark orange")) +
    ggpubr::stat_compare_means(
      method = "wilcox.test",
      label = "p.signif",
      hide.ns = FALSE,
      comparisons = list(c("Increase", "Decrease"), c("Increase", "No change"), c("Decrease", "No change")),
      size = 3
    ) + # Add pairwise comparisons p-value
    # ggpubr::stat_compare_means() +    # Add global p-value
    facet_wrap(. ~ variable,
      ncol = 1,
      scales = "free_y",
      strip.position = "left",
      labeller = labeller(variable = c(
        tmp = "Mean annual temperature (°C)",
        ppt = "Annual precipitation (mm)"
      ))
    ) +
    guides(col = "none") +
    labs(
      x = NULL,
      y = NULL
    ) +
    ggthemes::theme_few() +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside"
    ) +
    theme(
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 8)
    )

  exp_gainloss_main_gg <-
    ggplot() +
    geom_point(
      data = dat_niche,
      aes(
        x = tmp_occ_median,
        y = ppt_occ_median
      ), color = "gray", alpha = 0
    ) +
    geom_point(
      data = exp_gainloss_tbl %>% filter(year >= 2010),
      aes(
        x = tmp,
        y = ppt,
        size = dominance,
        color = change
      ), alpha = 1, pch = 21, fill = NA
    ) +
    geom_point(
      data = exp_gainloss_tbl %>% filter(change == "increase" | change == "decrease") %>% filter(!is.na(complete_change)) %>% filter(year >= 2010),
      aes(
        x = tmp,
        y = ppt,
        size = dominance,
        color = change,
        fill = complete_change,
      ), alpha = 0.75, pch = 21
    ) +
    scale_color_manual(values = c(increase = "dark green", `no clear change` = "lightgray", decrease = "dark orange")) +
    scale_fill_manual(values = c(new = "dark green", lost = "dark orange")) +
    labs(x = "Mean annual temperature (°C)", y = "Annual precipitation (mm)") +
    guides(
      fill = "none",
      size = "none",
      color = "none"
    ) +
    facet_wrap(. ~ phaseyear,
      nrow = 1
    ) +
    theme(axis.text = element_text(size = 8)) +
    theme(strip.text = element_text(hjust = 0))

  exp_gainloss_supp_gg <- ggplot() +
    geom_point(
      data = dat_niche,
      aes(
        x = tmp_occ_median,
        y = ppt_occ_median
      ), color = "gray", alpha = 0
    ) +
    geom_point(
      data = exp_gainloss_tbl,
      aes(
        x = tmp,
        y = ppt,
        size = dominance,
        color = change
      ), alpha = 1, pch = 21, fill = NA
    ) +
    geom_point(
      data = exp_gainloss_tbl %>% filter(change == "increase" | change == "decrease") %>% filter(!is.na(complete_change)),
      aes(
        x = tmp,
        y = ppt,
        size = dominance,
        color = change,
        fill = complete_change,
      ), alpha = 0.75, pch = 21
    ) +
    scale_color_manual(values = c(increase = "dark green", `no clear change` = "lightgray", decrease = "dark orange")) +
    scale_fill_manual(values = c(new = "dark green", lost = "dark orange")) +
    labs(x = "Mean annual temperature (°C)", y = "Annual precipitation (mm)") +
    guides(size = "none", color = "none", fill = "none") +
    facet_wrap(. ~ phaseyear,
      nrow = 3,
      drop = FALSE
    ) +
    ggrepel::geom_text_repel(
      data = exp_gainloss_tbl %>% filter(change != "no clear change"),
      aes(
        x = tmp,
        y = ppt,
        color = change,
        label = paste0("italic('", species_short, "')")
      ),
      size = 3.88 / 1.68,
      alpha = 1,
      max.overlaps = 100,
      parse = T
    ) +
    theme(strip.text = element_text(hjust = 0))

  out <- list(
    box = exp_gainloss_summ_gg,
    circle = exp_gainloss_main_gg,
    circle_detail = exp_gainloss_supp_gg
  )

  return(out)
}
