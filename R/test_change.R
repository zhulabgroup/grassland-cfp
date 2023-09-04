test_index_change_all <- function(dat_index, option ) {
  df_index_change_exp <- test_index_change_comb(dat_index, option) %>%
    mutate(test_index_change(dat_index, index, grouping, option)) %>%
    select(grouping, everything()) %>%
    unnest(grouping) %>%
    ungroup()

  return(df_index_change_exp)
}

test_trait_change_all <- function(dat_community, dat_niche, option) {
  df_trait_change_exp <- test_trait_change_comb(dat_community, option) %>%
    mutate(test_trait_change(dat_community, dat_niche, trait, grouping, option)) %>%
    select(grouping, everything()) %>%
    unnest(grouping) %>%
    ungroup()

  return(df_trait_change_exp)
}

test_index_change <- function(dat_index, index, grouping, option) {
  # subset data
  dat_lme <- test_index_change_data(dat_index, index, grouping, option)

  # fit lme
  mod_lme <- test_index_change_model(dat_lme, index, option)

  # summarize results
  df_lme <- test_change_summ(mod_lme)

  return(df_lme)
}

test_trait_change <- function(dat_community, dat_niche, trait, grouping, option) {
  # subset data
  dat_lme <- test_trait_change_data(dat_community, dat_niche, trait, grouping, option)

  # fit lme
  mod_lme <- test_trait_change_model(dat_lme, trait, option)
  # summarize results

  df_lme <- test_change_summ(mod_lme)


  return(df_lme)
}
