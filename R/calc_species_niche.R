#' Calculate Species Niche
#'
#' This function calculates a range of statistics to characterize species climate niche,
#' such as mean, standard deviation, median, and quantiles for the temperature, precipitation, and vapor pressure deficit.
#' Optionally, this function can also calculate these statistics for the climatic water deficit (cwd) if provided in the data.
#'
#' @param dat_trait A data frame that contains climatic conditions associated with individuals (i.e., traits in the climate niche space).
#' @param add_dummy A logical argument that determines whether to add dummy species to the output.
#' If `TRUE`, it adds `Avena DUMMY`, `Festuca DUMMY` and `Hypochaeris DUMMY` to the species niche data frame,
#' with their niche values calculated as the average of specific species within their respective genera. Default is `TRUE`.
#'
#' @return A data frame with species as rows and calculated niche statistics as columns.
#' @examples
#' \dontrun{
#' species_niche <- calc_species_niche(dat_trait, add_dummy = TRUE)
#' }
#' @export
calc_species_niche <- function(dat_trait, add_dummy = T) {
  dat_niche <- dat_trait %>%
    select(key, species, tmp, ppt, vpd) %>%
    group_by(species) %>%
    summarize(
      occ_n = n(),
      tmp_occ_mean = mean(tmp, na.rm = TRUE),
      tmp_occ_sd = sd(tmp, na.rm = TRUE),
      tmp_occ_median = median(tmp, na.rm = TRUE),
      tmp_occ_q05 = quantile(tmp, .05, na.rm = TRUE),
      tmp_occ_q25 = quantile(tmp, .25, na.rm = TRUE),
      tmp_occ_q75 = quantile(tmp, .75, na.rm = TRUE),
      tmp_occ_q95 = quantile(tmp, .95, na.rm = TRUE),
      ppt_occ_mean = mean(ppt, na.rm = TRUE),
      ppt_occ_sd = sd(ppt, na.rm = TRUE),
      ppt_occ_median = median(ppt, na.rm = TRUE),
      ppt_occ_q05 = quantile(ppt, .05, na.rm = TRUE),
      ppt_occ_q25 = quantile(ppt, .25, na.rm = TRUE),
      ppt_occ_q75 = quantile(ppt, .75, na.rm = TRUE),
      ppt_occ_q95 = quantile(ppt, .95, na.rm = TRUE),
      vpd_occ_mean = mean(vpd, na.rm = TRUE),
      vpd_occ_sd = sd(vpd, na.rm = TRUE),
      vpd_occ_median = median(vpd, na.rm = TRUE),
      vpd_occ_q05 = quantile(vpd, .05, na.rm = TRUE),
      vpd_occ_q25 = quantile(vpd, .25, na.rm = TRUE),
      vpd_occ_q75 = quantile(vpd, .75, na.rm = TRUE),
      vpd_occ_q95 = quantile(vpd, .95, na.rm = TRUE)
    )

  if ("cwd" %in% colnames(dat_trait)) {
    dat_niche_cwd <- dat_trait %>%
      select(key, species, cwd) %>%
      group_by(species) %>%
      summarize(
        cwd_occ_mean = mean(cwd, na.rm = TRUE),
        cwd_occ_sd = sd(cwd, na.rm = TRUE),
        cwd_occ_median = median(cwd, na.rm = TRUE),
        cwd_occ_q05 = quantile(cwd, .05, na.rm = TRUE),
        cwd_occ_q25 = quantile(cwd, .25, na.rm = TRUE),
        cwd_occ_q75 = quantile(cwd, .75, na.rm = TRUE),
        cwd_occ_q95 = quantile(cwd, .95, na.rm = TRUE)
      )

    dat_niche <- left_join(dat_niche, dat_niche_cwd, by = "species")
  }

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

    dat_niche <- dat_niche %>%
      filter(occ_n > 100) %>% # species with many observations
      add_row(species = "Avena DUMMY", occ_n = NA, Avena_tbl) %>%
      add_row(species = "Festuca DUMMY", occ_n = NA, Festuca_tbl) %>%
      add_row(species = "Hypochaeris DUMMY", occ_n = NA, Hypochaeris_tbl)
  }

  return(dat_niche)
}
