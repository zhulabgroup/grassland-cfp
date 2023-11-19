#' @export
summ_species_niche <- function(dat_niche, sp = NULL) {
  if (is.null(sp)) {
    df <- dat_niche %>%
      filter(!is.na(occ_n)) %>%
      summarise(
        n = n(),
        tmp_min = min(tmp_occ_median),
        tmp_max = max(tmp_occ_median),
        ppt_min = min(ppt_occ_median),
        ppt_max = max(ppt_occ_median)
      )
  }
  if (!is.null(sp)) {
    df <- dat_niche %>%
      filter(!is.na(occ_n)) %>%
      filter(species == sp) %>%
      select(
        n_occ = occ_n,
        tmp_median = tmp_occ_median,
        tmp_lower = tmp_occ_q05,
        tmp_upper = tmp_occ_q95,
        ppt_median = ppt_occ_median,
        ppt_lower = ppt_occ_q05,
        ppt_upper = ppt_occ_q95,
      )
  }

  return(df)
}
