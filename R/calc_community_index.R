#' @export
# calculate CWM (community weighted mean, or sd, etc.) like CTI and CPI
calc_community_index <- function(dat_niche, dat_community) {
  # observational data and CWM
  obs_tbl <- dat_community$obs %>%
    inner_join(dat_niche, by = "species") %>%
    group_by(site, year, plot) %>% # w/o treatment for obs
    calc_community_weighted_mean() %>%
    ungroup()

  # experimental data and CWM
  exp_tbl <- dat_community$exp %>%
    inner_join(dat_niche, by = "species") %>%
    group_by(site, year, plot, treat) %>% # w/ treatment for exp
    calc_community_weighted_mean() %>%
    ungroup()

  out <- list(
    obs = obs_tbl,
    exp = exp_tbl
  )

  return(out)
}

#' @export
# define summarize CWM function
calc_community_weighted_mean <- function(df) {
  df_mean <- select(df, -guild, -abund_type, -occ_n) %>%
    gather(key = "var", value = "value", -species, -abund, -group_vars(.)) %>%
    filter(str_detect(var, "_occ_median")) %>%
    group_by(var, .add = TRUE) %>%
    summarise(value = sum(abund * value) / sum(abund)) %>%
    ungroup() %>%
    mutate(var = str_remove(var, "_occ_median")) %>%
    mutate(var = factor(var, levels = c("tmp", "ppt", "vpd", "cwd"))) %>%
    arrange(var) %>%
    mutate(var = str_c(var, "_com_mean")) %>%
    mutate(var = factor(var, levels = unique(var))) %>%
    spread(key = "var", value = "value")

  df_var <- select(df, -guild, -abund_type, -occ_n) %>%
    gather(key = "var", value = "value", -species, -abund, -group_vars(df)) %>%
    filter(str_detect(var, "_occ_median")) %>%
    group_by(var, .add = TRUE) %>%
    summarise(value1 = sum(abund * value^2) / sum(abund)) %>%
    ungroup() %>%
    mutate(var = str_remove(var, "_occ_median")) %>%
    mutate(var = factor(var, levels = c("tmp", "ppt", "vpd", "cwd"))) %>%
    arrange(var) %>%
    left_join(df_mean %>%
      gather(key = "var", value = "value", -group_vars(df)) %>%
      mutate(var = str_remove(var, "_com_mean")) %>%
      mutate(value2 = value^2) %>%
      select(-value)) %>%
    mutate(value = value1 - value2) %>%
    select(-value1, -value2) %>%
    mutate(var = str_c(var, "_com_var")) %>%
    bind_rows(mutate(.,
      value = sqrt(value),
      var = str_replace(var, "_var", "_sd")
    )) %>%
    mutate(var = factor(var, levels = unique(var))) %>%
    spread(key = "var", value = "value")

  df_all <- left_join(df_mean, df_var, by = group_vars(df))

  return(df_all)
}
