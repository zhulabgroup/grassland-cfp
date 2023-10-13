tidy_table_niche <- function(dat_niche) {
  dat_niche_tidy <- dat_niche %>%
    mutate_at(vars(-species, -occ_n), ~ signif(., digits = 3)) %>%
    select(
      "Species" = species,
      "n" = occ_n,
      "Temperature mean" = tmp_occ_mean,
      "Temperature median" = tmp_occ_median,
      "Temperature lower limit (5%)" = tmp_occ_q05,
      "Temperature upper limit (95%)" = tmp_occ_q95,
      "Precipitation mean" = ppt_occ_mean,
      "Precipitation median" = ppt_occ_median,
      "Precipitation lower limit (5%)" = ppt_occ_q05,
      "Precipitation upper limit (95%)" = ppt_occ_q95
    )

  return(dat_niche_tidy)
}


tidy_table_gainloss <- function(dat_gainloss) {
  dat_gainloss_tidy <- bind_rows(
    dat_gainloss$obs %>%
      mutate(dataset = "observation"),
    dat_gainloss$exp %>%
      mutate(dataset = "experiment")
  ) %>%
    mutate_at(vars(tmp, ppt), ~ signif(., digits = 3)) %>%
    select(dataset, site, year, species, change, complete_change,
      temperature_optima = tmp,
      precipitation_optima = ppt
    ) %>%
    mutate(complete_change = case_when(
      complete_change == "new" ~ "established",
      complete_change == "lost" ~ "extirpated"
    ))

  return(dat_gainloss_tidy)
}
