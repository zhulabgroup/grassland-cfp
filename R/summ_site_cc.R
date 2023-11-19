#' @export
summ_site_climate_change <- function(df_cc_site) {
  df <- df_cc_site %>%
    group_by(clim_var) %>%
    summarise(
      mean = mean(estimate),
      min = min(estimate),
      max = max(estimate)
    )

  return(df)
}

#' @export
summ_jrgce_env <- function(df_env) {
  df <- df_env %>%
    group_by(var) %>%
    summarise(
      mean = mean(env),
      min = min(env),
      max = max(env)
    )
  return(df)
}
