#' Test Community Index Change in All Groups
#'
#' This function statistically tests changes in community indices across all available groups.
#'
#' @param dat_index A list with calculated community indices for observations and experiments.
#' @param option A string to choose between two types of tests. It should be set to "obs" to test changes over time at observational sites, or set of "exp" to test changes under treatments in global change experiments.
#'
#' @return A data frame with the magnitude, uncertainty, and significance level of changes in community index.
#' @examples
#' \dontrun{
#' df_index_change_obs <- test_index_change_all(dat_index = dat_index, option = "obs")
#' df_index_change_obs
#' df_index_change_exp <- test_index_change_all(dat_index = dat_index, option = "exp")
#' df_index_change_exp
#' }
#' @export
test_index_change_all <- function(dat_index, option) {
  df_index_change <- test_index_change_comb(dat_index, option) %>%
    mutate(test_index_change(dat_index, index, grouping, option)) %>%
    select(grouping, everything()) %>%
    unnest(grouping) %>%
    ungroup()

  return(df_index_change)
}

#' Test Species Optima Change in All Groups
#'
#' This function statistically tests changes in species optima across all available groups.
#'
#' @param dat_community A list containing community data from observations and experiments.
#' @param dat_niche A data frame that contains the species climate niche.
#' @param option A string to choose between two types of tests. It should be set to "obs" to test changes over time at observational sites, or set of "exp" to test changes under treatments in global change experiments.
#'
#' @return A data frame with the magnitude, uncertainty, and significance level of changes in species optima.
#' @examples
#' \dontrun{
#' df_optima_change_obs <- test_optima_change_all(dat_community = dat_community, dat_niche = dat_niche, option = "obs")
#' df_optima_change_obs
#' df_optima_change_exp <- test_optima_change_all(dat_community = dat_community, dat_niche = dat_niche, option = "exp")
#' df_optima_change_exp
#' }
#' @export
test_optima_change_all <- function(dat_community, dat_niche, option) {
  df_optima_change <- test_optima_change_comb(dat_community, option) %>%
    mutate(test_optima_change(dat_community, dat_niche, optima, grouping, option)) %>%
    select(grouping, everything()) %>%
    unnest(grouping) %>%
    ungroup()

  return(df_optima_change)
}

test_index_change <- function(dat_index, index, grouping, option) {
  # subset data
  dat_model <- test_index_change_data(dat_index, index, grouping, option)

  # fit lme
  model <- test_index_change_model(dat_model, option)

  # summarize results
  df_model <- test_change_summ(model)

  return(df_model)
}

test_optima_change <- function(dat_community, dat_niche, optima, grouping, option) {
  # subset data
  dat_model <- test_optima_change_data(dat_community, dat_niche, optima, grouping, option)

  # fit lme
  model <- test_optima_change_model(dat_model, option)

  # summarize results
  df_model <- test_change_summ(model)

  return(df_model)
}
