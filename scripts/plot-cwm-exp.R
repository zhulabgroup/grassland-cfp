# plot CWM (community weighted mean, or sd, etc.) like CTI and CPI
# define function to plot CWM for experimental data
plot_cwm_exp <- function(data, cwm_lab, trt_lab, trt_col) {
  ggplot(data, aes(x = year, y = com_idx, col = treat, group = interaction(treat, year))) +
    geom_boxplot() +
    ggpubr::stat_compare_means(
      aes(group = treat),
      method = "wilcox.test",
      label = "p.signif", hide.ns = FALSE
    ) +
    labs(
      x = "Year", y = cwm_lab,
      color = trt_lab
    ) +
    scale_color_manual(values = c("black", trt_col))
}

plot_cwm_jrgce <- function(cwm_tag = "mean_cti", trt_tag = "tmp_all") {
  # wrapper function to plot CWM in JRGCE
  # filter JRGCE data
  jrgce_tbl <- exp_tbl %>%
    filter(site == "jrgce")

  # filter data by treatment tag
  if (trt_tag == "tmp_only") { # only ambient vs. elevated tmp treat
    jrgce_tbl <- jrgce_tbl %>%
      filter(treat %in% c("____", "T___"))
  } else if (trt_tag == "tmp_all") { # all tmp treat
    jrgce_tbl <- jrgce_tbl %>%
      mutate(treat = str_sub(treat, start = 1L, end = 1L))
  } else if (trt_tag == "ppt_only") { # only ambient vs. elevated ppt treat
    jrgce_tbl <- jrgce_tbl %>%
      filter(treat %in% c("____", "_P__"))
  } else if (trt_tag == "ppt_all") { # all ppt treat
    jrgce_tbl <- jrgce_tbl %>%
      mutate(treat = str_sub(treat, start = 2L, end = 2L))
  } else {
    stop("Unknown treatment tag")
  }

  # filter data by response (CWM) tag
  jrgce_tbl <- jrgce_tbl %>%
    select(year, treat,
      com_idx = switch(cwm_tag,
        "mean_cti" = "tmp_com_mean",
        "sd_cti" = "tmp_com_sd",
        "mean_cpi" = "ppt_com_mean",
        "sd_cpi" = "ppt_com_sd",
        "mean_cvi" = "vpd_com_mean",
        "sd_cvi" = "vpd_com_sd"
      )
    )

  # make plot
  plot_cwm_exp(jrgce_tbl,
    cwm_lab = switch(cwm_tag,
      "mean_cti" = "Mean CTI (°C)",
      "sd_cti" = "Std dev CTI (°C)",
      "mean_cpi" = "Mean CPI (mm)",
      "sd_cpi" = "Std dev CPI (mm)",
      "mean_cvi" = "Mean CVI (Pa)",
      "sd_cvi" = "Std dev VPD (Pa)"
    ),
    trt_lab = str_sub(trt_tag, 1L, 3L) %>%
      switch(
        "tmp" = "Warming treatment",
        "ppt" = "Watering treatment"
      ),
    trt_col = str_sub(trt_tag, 1L, 3L) %>%
      switch(
        "tmp" = "red",
        "ppt" = "blue"
      )
  )
}

plot_cwm_mclexp <- function(cwm_tag = "mean_cti", trt_tag = "watering") {
  # wrapper function to plot CWM in McLaughlin Water Experiment
  # filter mclexp data
  mclexp_tbl <- exp_tbl %>%
    filter(site == "mclexp") %>%
    separate(treat, c("treat", "soil"), sep = 2)

  # filter data by treatment tag
  if (trt_tag == "watering") {
    mclexp_tbl <- filter(mclexp_tbl, treat %in% c("_X", "WX"))
  } else if (trt_tag == "drought") {
    mclexp_tbl <- filter(mclexp_tbl, treat %in% c("X_", "XD"))
  } else {
    stop("Unknown treatment tag")
  }

  # filter data by response (CWM) tag
  mclexp_tbl <- mclexp_tbl %>%
    select(year, treat, soil,
      com_idx = switch(cwm_tag,
        "mean_cti" = "tmp_com_mean",
        "sd_cti" = "tmp_com_sd",
        "mean_cpi" = "ppt_com_mean",
        "sd_cpi" = "ppt_com_sd",
        "mean_cvi" = "vpd_com_mean",
        "sd_cvi" = "vpd_com_sd"
      )
    )

  # make plot
  plot_cwm_exp(mclexp_tbl,
    cwm_lab = switch(cwm_tag,
      "mean_cti" = "Mean CTI (°C)",
      "sd_cti" = "Std dev CTI (°C)",
      "mean_cpi" = "Mean CPI (mm)",
      "sd_cpi" = "Std dev CPI (mm)",
      "mean_cvi" = "Mean CVI (Pa)",
      "sd_cvi" = "Std dev VPD (Pa)"
    ),
    trt_lab = switch(trt_tag,
      "watering" = "Watering treatment",
      "drought" = "Drought treatment"
    ),
    trt_col = switch(trt_tag,
      "watering" = "blue",
      "drought" = "red"
    )
  ) +
    facet_wrap(~soil, # facets by soil type
      labeller = as_labeller(
        c(
          `S` = "Serpentine soil",
          `N` = "Non-serpentine soil"
        )
      )
    )
}

plot_cwm_scide <- function(cwm_tag = "mean_cti") {
  # wrapper function to plot CWM in SC-IDE Drought Experiment
  # drought is the only treatment, so no treatment tag
  # filter data by site
  scide_tbl <- exp_tbl %>%
    filter(str_detect(site, "scide"))

  # filter data by response (CWM) tag
  scide_tbl <- scide_tbl %>%
    select(site, year, treat,
      com_idx = switch(cwm_tag,
        "mean_cti" = "tmp_com_mean",
        "sd_cti" = "tmp_com_sd",
        "mean_cpi" = "ppt_com_mean",
        "sd_cpi" = "ppt_com_sd",
        "mean_cvi" = "vpd_com_mean",
        "sd_cvi" = "vpd_com_sd"
      )
    )

  # make plot
  plot_cwm_exp(scide_tbl,
    cwm_lab = switch(cwm_tag,
      "mean_cti" = "Mean CTI (°C)",
      "sd_cti" = "Std dev CTI (°C)",
      "mean_cpi" = "Mean CPI (mm)",
      "sd_cpi" = "Std dev CPI (mm)",
      "mean_cvi" = "Mean CVI (Pa)",
      "sd_cvi" = "Std dev VPD (Pa)"
    ),
    trt_lab = "Drought treatment",
    trt_col = "red"
  ) +
    facet_wrap(~site, # facets by site
      labeller = as_labeller(
        c(
          `scide_arboretum` = "Arboretum",
          `scide_marshallfield` = "Marshall Field",
          `scide_ylr` = "Younger Lagoon"
        )
      )
    )
}
