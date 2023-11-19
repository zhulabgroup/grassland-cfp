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
  # load niche estimates.
  niche_tbl <- dat_niche

  # observational data and CWM
  obs_tbl <- dat_community$obs %>%
    inner_join(niche_tbl, by = "species") %>%
    group_by(site, year, plot) %>%
    calc_community_weighted_mean() %>%
    ungroup()

  # experimental data and CWM
  exp_tbl <- dat_community$exp %>%
    inner_join(niche_tbl, by = "species") %>%
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
  if (!"cwd_occ_median" %in% colnames(.)) {
    df <- summarize(.,
      tmp_com_mean = sum(abund * tmp_occ_median) / sum(abund),
      tmp_com_var = sum(abund * tmp_occ_median^2) / sum(abund) - tmp_com_mean^2,
      ppt_com_mean = sum(abund * ppt_occ_median) / sum(abund),
      ppt_com_var = sum(abund * ppt_occ_median^2) / sum(abund) - ppt_com_mean^2,
      vpd_com_mean = sum(abund * vpd_occ_median) / sum(abund),
      vpd_com_var = sum(abund * vpd_occ_median^2) / sum(abund) - vpd_com_mean^2
    ) %>%
      mutate(
        tmp_com_sd = sqrt(tmp_com_var),
        ppt_com_sd = sqrt(ppt_com_var),
        vpd_com_sd = sqrt(vpd_com_var)
      )
  }

  if ("cwd_occ_median" %in% colnames(.)) {
    df <- summarize(.,
      tmp_com_mean = sum(abund * tmp_occ_median) / sum(abund),
      tmp_com_var = sum(abund * tmp_occ_median^2) / sum(abund) - tmp_com_mean^2,
      ppt_com_mean = sum(abund * ppt_occ_median) / sum(abund),
      ppt_com_var = sum(abund * ppt_occ_median^2) / sum(abund) - ppt_com_mean^2,
      vpd_com_mean = sum(abund * vpd_occ_median) / sum(abund),
      vpd_com_var = sum(abund * vpd_occ_median^2) / sum(abund) - vpd_com_mean^2,
      cwd_com_mean = sum(abund * cwd_occ_median) / sum(abund),
      cwd_com_var = sum(abund * cwd_occ_median^2) / sum(abund) - cwd_com_mean^2
    ) %>%
      mutate(
        tmp_com_sd = sqrt(tmp_com_var),
        ppt_com_sd = sqrt(ppt_com_var),
        vpd_com_sd = sqrt(vpd_com_var),
        cwd_com_sd = sqrt(cwd_com_var)
      )
  }
  return(df)
}
