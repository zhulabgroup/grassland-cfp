summ_species_change <- function(dat_gainloss, option) {
  if (option == "obs") {
    obs_gainloss_eg1 <- dat_gainloss$obs %>%
      group_by(species, change) %>%
      summarize(n = n()) %>%
      filter(change != "no clear change") %>%
      group_by(change) %>%
      arrange(desc(n)) %>%
      slice(1) %>%
      select(species, change)

    obs_gainloss_eg2 <- dat_gainloss$obs %>%
      group_by(species, complete_change) %>%
      summarize(n = n()) %>%
      filter(!is.na(complete_change)) %>%
      group_by(complete_change) %>%
      arrange(desc(n)) %>%
      slice(1) %>%
      select(species, change = complete_change)

    df <- bind_rows(obs_gainloss_eg1, obs_gainloss_eg2)
  }

  if (option == "exp") {
    exp_gainloss_eg <- dat_gainloss$exp %>%
      group_by(species, change) %>%
      summarize(n = n()) %>%
      filter(change != "no clear change") %>%
      group_by(change) %>%
      arrange(desc(n)) %>%
      slice(1) %>%
      select(species, change)

    df <- exp_gainloss_eg
  }

  return(df)
}
