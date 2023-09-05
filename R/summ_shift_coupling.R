summ_shift_coupling <- function(dat_shift, ratio) {
  df_all_shift <- bind_rows(
    dat_shift$obs$shift %>%
      select(site, CTI0 = CTI_start, CTI1 = CTI_end, CPI0 = CPI_start, CPI1 = CPI_end, significance) %>%
      mutate(group = "observation"),
    dat_shift$exp$shift %>%
      # filter(phase == "Phase III") %>%
      mutate(site = paste(site, year, sep = "_")) %>%
      select(site, CTI0 = CTI_, CTI1 = CTIT, CPI0 = CPI_, CPI1 = CPIT, significance) %>%
      mutate(group = "experiment")
  ) %>%
    mutate(group = factor(group,
      levels = c("observation", "experiment"),
      labels = c("Observation", "Experiment")
    ))
  # filter(significance == "sig") %>%

  df1 <- df_all_shift %>%
    mutate(slope = (CPI1 - CPI0) / (CTI1 - CTI0)) %>%
    mutate(scale_factor = 0.1) %>%
    mutate(ratio = "cpi_to_cti")

  df2 <- df_all_shift %>%
    # filter(significance == "sig") %>%
    mutate(slope = (CTI1 - CTI0) / (CPI1 - CPI0)) %>%
    mutate(scale_factor = 10) %>%
    mutate(ratio = "cti_to_cpi")

  df <- bind_rows(df1, df2) %>%
    mutate(slope = slope * scale_factor) %>%
    group_by(ratio, group) %>%
    summarise(
      median = median(slope),
      min = min(slope),
      max = max(slope),
      # se = sd(slope) / sqrt(n()),
      scale_factor = unique(scale_factor)
    ) %>%
    ungroup()

  return(df)
}
