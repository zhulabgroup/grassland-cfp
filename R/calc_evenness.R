#' @export
calc_evenness <- function(dat_community) {
  out <- list(
    obs = calc_evenness_obs(dat_community$obs),
    exp = calc_evenness_exp(dat_community$exp)
  )
  return(out)
}

#' @export
calc_evenness_obs <- function(dat_community_obs) {
  df_obs_even <- dat_community_obs %>%
    filter(guild != "DUMMY") %>%
    left_join(
      group_by(., site, year, plot) %>%
        summarise(sum_abund = sum(abund)),
      by = c("site", "year", "plot")
    ) %>%
    mutate(dominance = abund / sum_abund) %>%
    group_by(site, year, plot) %>%
    summarise(
      h = vegan::diversity(abund),
      spn = n()
    ) %>%
    mutate(even = h / log(spn)) %>%
    ungroup() # %>%
  # group_by(site) %>%
  # nest() %>%
  # mutate(
  #   p_val = map(data, ~ lm(even ~ year, data = .)) %>%
  #     map_dbl(~ broom::glance(.) %>% pull(p.value))
  # ) %>%
  # unnest(cols = data)

  return(df_obs_even)
}

#' @export
calc_evenness_exp <- function(dat_community_exp) {
  plot_treat <- dat_community_exp %>%
    filter(site == "jrgce") %>%
    filter(year == 1999) %>%
    mutate(treat_T = str_sub(treat, start = 1L, end = 1L)) %>%
    distinct(plot, treat_T)

  df_exp_even <- dat_community_exp %>%
    filter(site == "jrgce") %>%
    filter(year > 1998) %>%
    filter(guild != "DUMMY") %>%
    left_join(plot_treat, by = "plot") %>%
    left_join(
      group_by(., treat_T, year, plot) %>%
        summarise(sum_abund = sum(abund)),
      by = c("treat_T", "year", "plot")
    ) %>%
    mutate(dominance = abund / sum_abund) %>%
    group_by(treat_T, year, plot) %>%
    summarise(
      h = vegan::diversity(abund),
      spn = n()
    ) %>%
    mutate(even = h / log(spn)) %>%
    ungroup()

  return(df_exp_even)
}
