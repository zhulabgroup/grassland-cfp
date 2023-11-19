# docu

#' @export
calc_community_shift <- function(dat_index) {
  dat_shift_obs <- calc_community_shift_obs(obs_tbl = dat_index$obs)
  dat_shift_exp <- calc_community_shift_exp(exp_tbl = dat_index$exp)

  out <- list(
    obs = dat_shift_obs,
    exp = dat_shift_exp
  )

  return(out)
}

#' @export
calc_community_shift_obs <- function(obs_tbl) {
  # reshape data
  obs_idx_tbl <- obs_tbl %>%
    dplyr::select(site, year, plot, tmp_com_mean, ppt_com_mean) %>%
    pivot_longer(cols = tmp_com_mean:ppt_com_mean, names_to = "com_idx_name", values_to = "com_idx_value") %>%
    mutate(com_idx_name = factor(com_idx_name,
      levels = c("tmp_com_mean", "ppt_com_mean"),
      labels = c("CTI", "CPI")
    ))

  df_obs_sum_coarse <- obs_idx_tbl %>%
    group_by(site, com_idx_name) %>%
    summarise(
      m = median(com_idx_value),
      se = sd(com_idx_value) / sqrt(n())
    ) %>%
    ungroup() %>%
    pivot_wider(
      id_cols = site,
      names_from = com_idx_name,
      values_from = c("m", "se")
    )

  df_obs_sum_fine <- obs_idx_tbl %>%
    group_by(site, year, com_idx_name) %>%
    summarise(
      m = median(com_idx_value),
      se = sd(com_idx_value) / sqrt(n())
    ) %>%
    ungroup() %>%
    pivot_wider(
      id_cols = c("site", "year"),
      names_from = com_idx_name,
      values_from = c("m", "se")
    )

  df_obs_test <- obs_idx_tbl %>%
    group_by(site, com_idx_name) %>%
    nest() %>%
    mutate(
      map(data, ~ lm(com_idx_value ~ year, data = .)) %>%
        map_df(~ broom::tidy(.) %>%
          filter(term == "year") %>%
          select(p.value)) # ,
    ) %>%
    select(-data) %>%
    ungroup() %>%
    rename(p = p.value) %>%
    pivot_wider(
      id_cols = site,
      names_from = com_idx_name,
      values_from = p
    ) %>%
    mutate(significance = case_when(
      CTI <= 0.05 & CPI <= 0.05 ~ "sig",
      # CTI > 0.05 & CPI > 0.05 ~ "?",
      TRUE ~ "ns"
    )) %>%
    select(-CTI, -CPI)

  df_obs_shift <- obs_idx_tbl %>%
    group_by(site, com_idx_name) %>%
    do(broom::augment(lm(com_idx_value ~ year, data = .))) %>%
    select(site, com_idx_name, year, fitted = .fitted) %>%
    filter(year == min(year) | year == max(year)) %>%
    mutate(year = case_when(
      year == min(year) ~ "start",
      year == max(year) ~ "end"
    )) %>%
    ungroup() %>%
    distinct() %>%
    pivot_wider(
      id_cols = c("site", "year"),
      names_from = com_idx_name,
      values_from = fitted
    ) %>%
    pivot_wider(
      id_cols = site,
      names_from = year,
      values_from = c("CTI", "CPI")
    ) %>%
    left_join(df_obs_test, by = "site")


  out <- list(
    sum_coarse = df_obs_sum_coarse,
    sum_fine = df_obs_sum_fine,
    shift = df_obs_shift
  )
  return(out)
}

#' @export
calc_community_shift_exp <- function(exp_tbl) {
  jrgce_tbl <- exp_tbl %>%
    filter(site == "jrgce", year >= 1999) %>%
    mutate(treat_T = str_sub(treat, start = 1L, end = 1L)) %>%
    select(site, year, plot, treat_T, tmp_com_mean, ppt_com_mean) %>%
    pivot_longer(cols = tmp_com_mean:ppt_com_mean, names_to = "com_idx_name", values_to = "com_idx_value") %>%
    mutate(com_idx_name = factor(com_idx_name,
      levels = c("tmp_com_mean", "ppt_com_mean"),
      labels = c("CTI", "CPI")
    ))

  df_exp_sum_coarse <- jrgce_tbl %>%
    mutate(phase = case_when(
      year <= 2002 ~ "Phase I",
      year >= 2010 ~ "Phase III",
      TRUE ~ "Phase II"
    )) %>%
    group_by(site, phase, com_idx_name) %>%
    summarise(
      m = median(com_idx_value),
      se = sd(com_idx_value) / sqrt(n())
    ) %>%
    ungroup() %>%
    pivot_wider(
      id_cols = c("site", "phase"),
      names_from = com_idx_name,
      values_from = c("m", "se")
    )

  df_exp_sum_fine <- jrgce_tbl %>%
    mutate(phase = case_when(
      year <= 2002 ~ "Phase I",
      year >= 2010 ~ "Phase III",
      TRUE ~ "Phase II"
    )) %>%
    group_by(site, phase, year, treat_T, com_idx_name) %>%
    summarise(
      m = median(com_idx_value),
      se = sd(com_idx_value) / sqrt(n())
    ) %>%
    ungroup() %>%
    pivot_wider(
      id_cols = c("site", "phase", "year", "treat_T"),
      names_from = com_idx_name,
      values_from = c("m", "se")
    ) %>%
    mutate(treatment = case_when(
      treat_T == "_" ~ "ambient",
      treat_T == "T" ~ "warming"
    )) %>%
    mutate(treatment = factor(treatment,
      levels = c("ambient", "warming"),
      labels = c("Ambient", "Warming")
    ))

  df_exp_test <- jrgce_tbl %>%
    mutate(phase = case_when(
      year <= 2002 ~ "Phase I",
      year >= 2010 ~ "Phase III",
      TRUE ~ "Phase II"
    )) %>%
    rename(
      trt = treat_T,
      value = com_idx_value
    ) %>%
    group_by(phase, com_idx_name) %>%
    nest() %>%
    mutate(test_index_change_model(dat_model = data[[1]], option = "exp") %>%
      test_change_summ()) %>%
    ungroup() %>%
    select(-data, -estimate, -sig) %>%
    pivot_wider(
      id_cols = c("phase"),
      names_from = com_idx_name,
      values_from = p.value
    ) %>%
    mutate(significance = case_when(
      CTI <= 0.05 & CPI <= 0.05 ~ "sig",
      # CTI > 0.05 & CPI > 0.05 ~ "?",
      TRUE ~ "ns"
    )) %>%
    select(-CTI, -CPI)

  df_exp_shift <- jrgce_tbl %>%
    mutate(phase = case_when(
      year <= 2002 ~ "Phase I",
      year >= 2010 ~ "Phase III",
      TRUE ~ "Phase II"
    )) %>%
    group_by(site, phase, year, treat_T, com_idx_name) %>%
    summarise(m = median(com_idx_value)) %>%
    ungroup() %>%
    mutate(group_metric = paste0(com_idx_name, treat_T)) %>%
    select(-com_idx_name, -treat_T) %>%
    pivot_wider(
      id_cols = c("site", "phase", "year"),
      names_from = group_metric,
      values_from = m
    ) %>%
    left_join(df_exp_test, by = c("phase"))

  out <- list(
    sum_coarse = df_exp_sum_coarse,
    sum_fine = df_exp_sum_fine,
    shift = df_exp_shift
  )
  return(out)
}
