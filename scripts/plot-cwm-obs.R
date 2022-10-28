plot_cwm_obs <- function(cwm_tag = "mean_cti") {
  # function to plot CWM for obs data
  # filter data by response (CWM) tag
  obs_lm_tbl <- obs_tbl %>%
    select(site, year, plot,
      com_idx = switch(cwm_tag,
        "mean_cti" = "tmp_com_mean",
        "sd_cti" = "tmp_com_sd",
        "mean_cpi" = "ppt_com_mean",
        "sd_cpi" = "ppt_com_sd",
        "mean_cvi" = "vpd_com_mean",
        "sd_cvi" = "vpd_com_sd"
      )
    ) %>%
    group_by(site) %>%
    nest() %>%
    mutate( # lm and p value
      p_val = map(data, ~ lm(com_idx ~ year, data = .)) %>%
        map_dbl(~ broom::glance(.) %>% pull(p.value))
    ) %>%
    unnest(cols = data)

  # make plot
  cwm_lab <- switch(cwm_tag,
    "mean_cti" = "Mean CTI (°C)",
    "sd_cti" = "Std dev CTI (°C)",
    "mean_cpi" = "Mean CPI (mm)",
    "sd_cpi" = "Std dev CPI (mm)",
    "mean_cvi" = "Mean CVI (Pa)",
    "sd_cvi" = "Std dev VPD (Pa)"
  )

  site_lab <- c(
    angelo = "Angelo Coast",
    carrizo = "Carrizo Plain",
    elkhorn = "Elkhorn Slough",
    jasper = "Jasper Ridge",
    mclann = "McLaughlin Annual",
    mclserp = "McLaughlin Serpentine",
    morganterritory = "Morgan Territory",
    pleasantonridge = "Pleasanton Ridge",
    sunol = "Sunol",
    swanton = "Swanton Ranch",
    ucsc = "UC Santa Cruz",
    vascocaves = "Vasco Caves"
  )

  ggplot(obs_lm_tbl, aes(year, com_idx)) +
    geom_boxplot(aes(group = year)) +
    geom_smooth( # add lm trend line when significant
      data = . %>% filter(p_val < 0.05),
      method = "lm", se = FALSE,
      color = "red"
    ) +
    facet_wrap(~site, scales = "free_y", labeller = as_labeller(site_lab)) +
    labs(
      x = "Year", y = cwm_lab
    )
}
