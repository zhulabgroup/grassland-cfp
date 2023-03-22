# niche data --------------------------------------------------------------
niche_tbl <- read_rds(.path$sum_niche) %>%
  filter(occ_n > 100 | is.na(occ_n)) %>% # species with many observations and dummy species
  select(species, tmp_occ_median, ppt_occ_median)

# observational data ------------------------------------------------------
site_vec <- c(
  angelo = "Angelo Coast",
  carrizo = "Carrizo Plain",
  elkhorn = "Elkhorn Slough",
  jasper = "Jasper Ridge Serpentine",
  mclann = "McLaughlin Annual",
  mclserp = "McLaughlin Serpentine",
  morganterritory = "Morgan Territory",
  pleasantonridge = "Pleasanton Ridge",
  sunol = "Sunol",
  swanton = "Swanton Ranch",
  ucsc = "UC Santa Cruz",
  vascocaves = "Vasco Caves"
)

obs_gainloss_tbl_list <- vector(mode = "list")
for (siteoi in names(site_vec)) {
  df_trend <- read_rds(.path$com_obs) %>%
    filter(site == siteoi) %>%
    filter(guild != "DUMMY") %>%
    left_join(
      group_by(., year, plot) %>%
        summarise(total = sum(abund)) %>%
        ungroup(),
      by = c("year", "plot")
    ) %>%
    mutate(rel_abun = abund / total) %>%
    select(year, plot, species, abund) %>%
    spread(key = "species", value = "abund") %>%
    mutate_if(is.numeric, ~ replace_na(., 0)) %>%
    gather(key = "species", value = "abund", -year, -plot) %>%
    group_by(species) %>%
    nest() %>%
    mutate(
      map(data, ~ lm(abund ~ year, data = .)) %>%
        map_df(~ broom::tidy(.) %>%
          filter(term == "year") %>%
          select(estimate, p.value)),
    ) %>%
    unnest(cols = data) %>%
    distinct(species, estimate, p.value) %>%
    mutate(change = case_when(
      (estimate > 0 & p.value <= 0.05) ~ "gain",
      (estimate < 0 & p.value <= 0.05) ~ "loss",
      TRUE ~ "no clear change"
    ))

  df_dominance <- read_rds(.path$com_obs) %>%
    filter(site == siteoi) %>%
    filter(guild != "DUMMY") %>%
    group_by(species) %>%
    summarise(abund = sum(abund)) %>%
    ungroup() %>%
    mutate(dominance = abund / sum(abund)) %>%
    select(-abund)

  df_complete <- read_rds(.path$com_obs) %>%
    filter(site == siteoi) %>%
    filter(guild != "DUMMY") %>%
    mutate(period = case_when(
      year %in% (year %>% unique() %>% sort() %>% head(5)) ~ "early",
      year %in% (year %>% unique() %>% sort() %>% tail(5)) ~ "late"
    )) %>%
    filter(!is.na(period)) %>%
    spread(key = "species", value = "abund") %>%
    mutate_if(is.numeric, ~ replace_na(., 0)) %>%
    gather(key = "species", value = "abund", -period) %>%
    spread(key = "period", value = "abund") %>%
    mutate(complete = case_when(
      (early == 0 & late != 0) ~ "recruited",
      (early != 0 & late == 0) ~ "extirpated"
    )) %>%
    select(species, complete)

  obs_gainloss_tbl_list[[siteoi]] <- df_trend %>%
    left_join(df_dominance, by = "species") %>%
    left_join(df_complete, by = "species") %>%
    left_join(niche_tbl, by = "species") %>%
    mutate(site = siteoi)

  print(siteoi)
}
obs_gainloss_tbl <- bind_rows(obs_gainloss_tbl_list)

write_rds(obs_gainloss_tbl, str_c(.path$sum_gainloss, "obs.rds"))

# experimental data -------------------------------------------------------
plot_treat <- read_rds(.path$com_exp) %>%
  filter(site == "jrgce") %>%
  filter(year == 1999) %>%
  mutate(treat_T = str_sub(treat, start = 1L, end = 1L)) %>%
  distinct(plot, treat_T)

exp_gainloss_tbl_list <- vector(mode = "list")
for (yearoi in 1998:2014) {
  df_trend <- read_rds(.path$com_exp) %>%
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
    select(plot, treat_T, species, abund) %>%
    spread(key = "species", value = "abund") %>%
    mutate_if(is.numeric, ~ replace_na(., 0)) %>%
    gather(key = "species", value = "abund", -treat_T, -plot) %>%
    spread(key = "treat_T", value = "abund") %>%
    mutate_if(is.numeric, ~ replace_na(., 0)) %>%
    gather(key = "treat_T", value = "abund", -plot, -species) %>%
    group_by(species) %>%
    nest() %>%
    mutate(
      map(data, ~ wilcox.test(abund ~ treat_T, data = ., conf.int = T)) %>%
        map_df(~ broom::tidy(.) %>%
          select(estimate, p.value)),
    ) %>%
    unnest(cols = data) %>%
    distinct(species, estimate, p.value) %>%
    mutate(change = case_when(
      (estimate < 0 & p.value <= 0.05) ~ "gain",
      (estimate > 0 & p.value <= 0.05) ~ "loss",
      TRUE ~ "no clear change"
    ))

  df_dominance <- read_rds(.path$com_exp) %>%
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

  df_complete <- read_rds(.path$com_exp) %>%
    filter(site == "jrgce") %>%
    filter(year == yearoi) %>%
    filter(guild != "DUMMY") %>%
    left_join(plot_treat, by = "plot") %>%
    spread(key = "species", value = "abund") %>%
    mutate_if(is.numeric, ~ replace_na(., 0)) %>%
    gather(key = "species", value = "abund", -treat_T) %>%
    spread(key = "treat_T", value = "abund") %>%
    mutate(complete = case_when(
      (`_` == 0 & T != 0) ~ "recruited",
      (T != 0 & `_` == 0) ~ "extirpated"
    )) %>%
    select(species, complete)

  exp_gainloss_tbl_list[[yearoi %>% as.character()]] <- df_trend %>%
    left_join(df_dominance, by = "species") %>%
    left_join(df_complete, by = "species") %>%
    left_join(niche_tbl, by = "species") %>%
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

write_rds(exp_gainloss_tbl, str_c(.path$sum_gainloss, "exp.rds"))
