#' Calculate Community Index
#'
#' This function calculates community indices including CTI and CPI, by taking the community weighted mean of species climate niches.
#'
#' @param dat_niche A data frame that contains the species climate niche.
#' @param dat_community A list containing community data from observations and experiments.
#'
#' @return A list with calculated community indices for observations and experiments.
#'
#' @examples
#' \dontrun{
#' dat_index <- calc_community_index(dat_niche = dat_niche, dat_community = dat_community)
#' dat_index$obs
#' dat_index$exp
#' }
#' @export
calc_community_index <- function(dat_niche, dat_community) {
  # observational data and CWM
  obs_tbl <- dat_community$obs %>%
    inner_join(dat_niche, by = "species") %>%
    group_by(site, year, plot) %>%
    calc_community_weighted_mean() %>%
    ungroup()

  # experimental data and CWM
  exp_tbl <- dat_community$exp %>%
    inner_join(dat_niche, by = "species") %>%
    group_by(site, year, plot, treat) %>%
    calc_community_weighted_mean() %>%
    ungroup()

  out <- list(
    obs = obs_tbl,
    exp = exp_tbl
  )

  return(out)
}

calc_community_weighted_mean <- function(.) {
  df <- select(., -guild, -abund_type, -occ_n) %>%
    gather(key = "var", value = "value", -species, -abund, -group_vars(.)) %>%
    filter(str_detect(var, "_occ_median")) %>%
    group_by(var, .add = TRUE) %>%
    summarise(value = sum(abund * value) / sum(abund)) %>%
    ungroup() %>%
    mutate(var = str_remove(var, "_occ_median")) %>%
    mutate(var = factor(var, levels = c("tmp", "ppt", "vpd", "cwd"))) %>%
    arrange(var) %>%
    mutate(var = str_c(var, "_com_mean")) %>%
    mutate(var = factor(var, levels = unique(var))) %>%
    spread(key = "var", value = "value")

  return(df)
}
