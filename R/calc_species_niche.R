#' @export
calc_species_niche <- function(dat_trait, add_dummy = T) {
  dat_niche <- dat_trait %>%
    select(key, species, any_of(c("tmp", "ppt", "vpd", "cwd"))) %>%
    gather(key = "var", value = "value", -key, -species) %>%
    group_by(species, var) %>%
    summarise(
      occ_n = n(),
      mean = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      q05 = quantile(value, .05, na.rm = TRUE, names = F),
      q25 = quantile(value, .25, na.rm = TRUE, names = F),
      q75 = quantile(value, .75, na.rm = TRUE, names = F),
      q95 = quantile(value, .95, na.rm = TRUE, names = F)
    ) %>%
    ungroup() %>%
    gather(key = "stats", value = "value", -species, -var, -occ_n) %>%
    mutate(var = factor(var, levels = c("tmp", "ppt", "vpd", "cwd"))) %>%
    mutate(stats = factor(stats, levels = c("mean", "sd", "median", "q05", "q25", "q75", "q95"))) %>%
    arrange(var, stats) %>%
    mutate(var = str_c(var, "_occ_", stats)) %>%
    select(-stats) %>%
    mutate(var = factor(var, levels = unique(var))) %>%
    spread(key = "var", value = "value")

  if (add_dummy) {
    # add genus-only species as dummy and set their niche as average
    Avena_tbl <- dat_niche %>%
      filter(species %in% c(
        "Avena barbata",
        "Avena fatua"
      )) %>%
      summarize(across(-c(species:occ_n), mean))

    Festuca_tbl <- dat_niche %>%
      filter(species %in% c(
        "Festuca bromoides",
        "Festuca perennis"
      )) %>%
      summarize(across(-c(species:occ_n), mean))

    Hypochaeris_tbl <- dat_niche %>%
      filter(species %in% c(
        "Hypochaeris glabra",
        "Hypochaeris radicata"
      )) %>%
      summarize(across(-c(species:occ_n), mean))

    # Raphanus_tbl <- dat_niche %>%
    #   filter(species %in% c(
    #     "Raphanus sativus",
    #     "Raphanus raphanistrum" # this species doesn't exist from GBIF download, so no need to add dummy
    #   )) %>%
    #   summarize(across(-c(species:occ_n), mean))

    dat_niche <- dat_niche %>%
      filter(occ_n > 100) %>% # species with many observations
      add_row(species = "Avena DUMMY", occ_n = NA, Avena_tbl) %>%
      add_row(species = "Festuca DUMMY", occ_n = NA, Festuca_tbl) %>%
      add_row(species = "Hypochaeris DUMMY", occ_n = NA, Hypochaeris_tbl)
  }

  return(dat_niche)
}
