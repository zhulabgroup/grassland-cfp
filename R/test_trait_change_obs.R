test_trait_change_obs_all <- function(dat_community_obs, dat_niche) {
  df_test_trait_change_obs <- data.frame(var = c("tmp", "ppt")) %>%
    mutate(var = factor(var, levels = c("tmp", "ppt"))) %>%
    arrange(var) %>%
    rowwise() %>%
    mutate(test_trait_change_obs(dat_community_obs, dat_niche, var)) %>%
    ungroup()

  return(df_test_trait_change_obs)
}


test_trait_change_obs <- function(dat_community, dat_niche, var) {
  # subset data
  dat_lme <- test_trait_change_obs_data(dat_community, dat_niche, var)

  # fit lme
  mod_lme <- test_trait_change_obs_model(dat_lme, var)
  # summarize results

  df_lme_obs <- test_trait_change_summ(mod_lme)

  return(df_lme_obs)
}

test_trait_change_obs_model <- function(dat_lme, var) {
  if (var == "ppt") {
    dat_lme <- dat_lme %>%
      mutate(value = log(value))
  }
  model <- lmerTest::lmer(value ~ year + (1 | site),
    weights = dat_lme %>% pull(weight),
    data = dat_lme
  )

  res <- list(model = model, summary = summary(model))
  return(res)
}

test_trait_change_obs_data <- function(dat_community, dat_niche, var) {
  dat_lme <- dat_community %>%
    select(site, year, plot, species, abund) %>%
    group_by(site, plot, year) %>%
    mutate(weight = abund / sum(abund)) %>% # convert all abundance (absolute or relative) to percentage, such that all plots get equal weight later in lme
    ungroup() %>%
    left_join(
      dat_niche %>%
        select(species, value = !!sym(str_c(var, "_occ_mean"))),
      by = "species"
    )

  return(dat_lme)
}
