calc_species_gainloss <- function(dat_community, dat_niche) {
  dat_gainloss_obs <- calc_species_gainloss_obs(com_obs = dat_community$obs, dat_niche)
  dat_gainloss_exp <- calc_species_gainloss_exp(com_exp = dat_community$exp, dat_niche)

  dat_gainloss <- list(
    obs = dat_gainloss_obs,
    exp = dat_gainloss_exp
  )

  return(dat_gainloss)
}

calc_species_gainloss_obs <- function(com_obs, dat_niche) {
  site_list <- read_site_info() %>%
    filter(!site %in% c("jrgce", "scide")) %>%
    pull(site)

  obs_gainloss_tbl_list <- vector(mode = "list")
  for (siteoi in site_list) {
    df_trend <- com_obs %>%
      filter(site == siteoi) %>%
      filter(guild != "DUMMY") %>%
      left_join(
        group_by(., year, plot) %>%
          summarise(total = sum(abund)) %>%
          ungroup(),
        by = c("year", "plot")
      ) %>%
      mutate(rel_abun = abund / total) %>%
      select(year, plot, species, abund) %>% # note: did not use rel_abun
      spread(key = "species", value = "abund") %>%
      mutate_if(is.numeric, ~ replace_na(., 0)) %>%
      gather(key = "species", value = "abund", -year, -plot) %>%
      group_by(species) %>%
      nest() %>%
      mutate(
        map(data, ~ lm(abund ~ year, data = .)) %>%
          map_df(~ broom::tidy(.) %>%
            filter(term != "(Intercept)") %>%
            select(estimate, p.value)),
      ) %>%
      unnest(cols = data) %>%
      distinct(species, estimate, p.value) %>%
      mutate(change = case_when(
        (estimate > 0 & p.value <= 0.05) ~ "increase",
        (estimate < 0 & p.value <= 0.05) ~ "decrease",
        TRUE ~ "no clear change"
      )) %>%
      ungroup()

    df_dominance <- com_obs %>%
      filter(site == siteoi) %>%
      filter(guild != "DUMMY") %>%
      group_by(species) %>%
      summarise(abund = sum(abund)) %>%
      ungroup() %>%
      mutate(dominance = abund / sum(abund)) %>%
      select(-abund)

    df_complete <- com_obs %>%
      filter(site == siteoi) %>%
      filter(guild != "DUMMY") %>%
      mutate(period = case_when(
        year %in% (year %>% unique() %>% sort() %>% head(5)) ~ "early",
        year %in% (year %>% unique() %>% sort() %>% tail(5)) ~ "late"
      )) %>%
      filter(!is.na(period)) %>%
      group_by(period, species) %>%
      summarise(abund = sum(abund)) %>%
      spread(key = "species", value = "abund") %>%
      mutate_if(is.numeric, ~ replace_na(., 0)) %>%
      gather(key = "species", value = "abund", -period) %>%
      spread(key = "period", value = "abund") %>%
      mutate(complete_change = case_when(
        (early == 0 & late != 0) ~ "new",
        (early != 0 & late == 0) ~ "lost"
      )) %>%
      select(species, complete_change)

    obs_gainloss_tbl_list[[siteoi]] <- df_trend %>%
      left_join(df_dominance, by = "species") %>%
      left_join(df_complete, by = "species") %>%
      left_join(dat_niche %>%
        select(
          species,
          tmp = tmp_occ_median,
          ppt = ppt_occ_median
        ), by = "species") %>%
      mutate(site = siteoi)

    print(siteoi)
  }

  obs_gainloss_tbl <- bind_rows(obs_gainloss_tbl_list)

  return(obs_gainloss_tbl)
}

calc_species_gainloss_exp <- function(com_exp, dat_niche) {
  plot_treat <- com_exp %>%
    filter(site == "jrgce") %>%
    filter(year == 1999) %>%
    mutate(treat_T = str_sub(treat, start = 1L, end = 1L)) %>%
    distinct(plot, treat_T)

  exp_gainloss_tbl_list <- vector(mode = "list")
  for (yearoi in 1998:2014) {
    df_trend <- com_exp %>%
      filter(site == "jrgce") %>%
      filter(year == yearoi) %>%
      filter(guild != "DUMMY") %>%
      left_join(plot_treat, by = "plot") %>%
      left_join(
        group_by(., treat_T, plot) %>%
          summarise(total = sum(abund)) %>%
          ungroup(),
        by = c("treat_T", "plot")
      ) %>%
      mutate(rel_abun = abund / total) %>%
      select(plot, treat_T, species, abund) %>% # note: did not use rel_abun
      spread(key = "species", value = "abund") %>%
      mutate_if(is.numeric, ~ replace_na(., 0)) %>%
      gather(key = "species", value = "abund", -treat_T, -plot) %>%
      spread(key = "treat_T", value = "abund") %>%
      mutate_if(is.numeric, ~ replace_na(., 0)) %>%
      gather(key = "treat_T", value = "abund", -plot, -species) %>%
      group_by(species) %>%
      nest() %>%
      mutate(
        map(data, ~ lm(abund ~ treat_T, data = .)) %>%
          map_df(~ broom::tidy(.) %>%
            filter(term != "(Intercept)") %>%
            select(estimate, p.value)),
      ) %>%
      unnest(cols = data) %>%
      distinct(species, estimate, p.value) %>%
      mutate(change = case_when(
        (estimate > 0 & p.value <= 0.05) ~ "increase",
        (estimate < 0 & p.value <= 0.05) ~ "decrease",
        TRUE ~ "no clear change"
      )) %>%
      ungroup()

    df_dominance <- com_exp %>%
      filter(site == "jrgce") %>%
      filter(year == yearoi) %>%
      filter(guild != "DUMMY") %>%
      left_join(plot_treat, by = "plot") %>%
      select(plot, species, abund) %>%
      select(species, abund) %>%
      group_by(species) %>%
      summarise(abund = sum(abund)) %>%
      ungroup() %>%
      mutate(dominance = abund / sum(abund)) %>%
      select(-abund)

    df_complete <- com_exp %>%
      filter(site == "jrgce") %>%
      filter(year == yearoi) %>%
      filter(guild != "DUMMY") %>%
      left_join(plot_treat, by = "plot") %>%
      group_by(treat_T, species) %>%
      summarise(abund = sum(abund)) %>%
      spread(key = "species", value = "abund") %>%
      mutate_if(is.numeric, ~ replace_na(., 0)) %>%
      gather(key = "species", value = "abund", -treat_T) %>%
      spread(key = "treat_T", value = "abund") %>%
      mutate(complete_change = case_when(
        (`_` == 0 & T != 0) ~ "new",
        (T != 0 & `_` == 0) ~ "lost"
      )) %>%
      select(species, complete_change)

    exp_gainloss_tbl_list[[yearoi %>% as.character()]] <- df_trend %>%
      left_join(df_dominance, by = "species") %>%
      left_join(df_complete, by = "species") %>%
      left_join(dat_niche %>%
        select(
          species,
          tmp = tmp_occ_median,
          ppt = ppt_occ_median
        ), by = "species") %>%
      mutate(year = yearoi)

    print(yearoi)
  }

  exp_gainloss_tbl <- bind_rows(exp_gainloss_tbl_list) %>%
    mutate(phase = case_when(
      year <= 2002 ~ "Phase I",
      year >= 2010 ~ "Phase III",
      TRUE ~ "Phase II"
    )) %>%
    mutate(phaseyear = paste0(phase, ": ", year))

  return(exp_gainloss_tbl)
}
