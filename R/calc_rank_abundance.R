calc_rank_abundance <- function(dat_community) {
  out <- list(
    obs = calc_rank_abundance_obs(dat_community$obs),
    exp = calc_rank_abundance_exp(dat_community$exp)
  )
  return(out)
}

calc_rank_abundance_obs <- function(dat_community_obs) {
  df_obs_rank <- dat_community_obs %>%
    filter(guild != "DUMMY") %>%
    left_join(
      group_by(., site, year, plot) %>%
        summarise(sum_abund = sum(abund)),
      by = c("site", "year", "plot")
    ) %>%
    mutate(dominance = abund / sum_abund) %>%
    group_by(site, year, plot) %>%
    arrange(site, year, plot, desc(dominance)) %>%
    mutate(rank = row_number()) %>%
    ungroup() %>%
    group_by(site, year, rank) %>%
    summarise(
      median = quantile(abund, 0.5),
      lower = quantile(abund, 0.025),
      upper = quantile(abund, 0.975)
    ) %>%
    ungroup()

  return(df_obs_rank)
}

calc_rank_abundance_exp <- function(dat_community_exp) {
  plot_treat <- dat_community_exp %>%
    filter(site == "jrgce") %>%
    filter(year == 1999) %>%
    mutate(treat_T = str_sub(treat, start = 1L, end = 1L)) %>%
    distinct(plot, treat_T)

  df_exp_rank <- dat_community_exp %>%
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
    arrange(treat_T, year, plot, desc(dominance)) %>%
    mutate(rank = row_number()) %>%
    ungroup() %>%
    mutate(phase = case_when(
      year <= 2002 ~ "Phase I",
      year >= 2010 ~ "Phase III",
      TRUE ~ "Phase II"
    )) %>%
    mutate(phaseyear = paste0(phase, ": ", year)) %>%
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
    )) %>%
    group_by(phaseyear, treat_T, rank) %>%
    summarise(
      median = quantile(abund, 0.5),
      lower = quantile(abund, 0.025),
      upper = quantile(abund, 0.975)
    )

  return(df_exp_rank)
}
