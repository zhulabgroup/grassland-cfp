#' @export
test_index_change_all <- function(dat_index, option, bonferroni = F) {
  df_index_change <- test_index_change_comb(dat_index, option) %>%
    mutate(summary = test_index_change(dat_index, index, grouping, n_comparison, option, bonferroni)) %>%
    select(grouping, everything()) %>%
    unnest(summary) %>%
    unnest(grouping) %>%
    ungroup()

  return(df_index_change)
}

#' @export
test_trait_change_all <- function(dat_community, dat_niche, option, bonferroni = F) {
  df_trait_change <- test_trait_change_comb(dat_community, option) %>%
    mutate(summary = test_trait_change(dat_community, dat_niche, trait, grouping, n_comparison, option, bonferroni)) %>%
    select(grouping, everything()) %>%
    unnest(summary) %>%
    unnest(grouping) %>%
    ungroup()

  return(df_trait_change)
}

#' @export
test_index_change <- function(dat_index, index, grouping, n_comparison, option, bonferroni = F) {
  # subset data
  dat_model <- test_index_change_data(dat_index, index, grouping, option)

  # fit lme
  model <- test_index_change_model(dat_model, option)

  # summarize results
  df_model <- test_change_summ(model, n_comparison, bonferroni)

  return(df_model)
}

#' @export
test_trait_change <- function(dat_community, dat_niche, trait, grouping, n_comparison, option, bonferroni = F) {
  # subset data
  dat_model <- test_trait_change_data(dat_community, dat_niche, trait, grouping, option)

  # fit lme
  model <- test_trait_change_model(dat_model, option)

  # summarize results
  df_model <- test_change_summ(model, n_comparison, bonferroni)

  return(df_model)
}
