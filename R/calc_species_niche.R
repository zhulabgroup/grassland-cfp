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
    select(key, species, any_of(c("tmp", "ppt", "vpd", "cwd"))) %>%
    gather(key = "var", value = "value", -key, -species) %>%
    group_by(species, var) %>%
    summarise(
      occ_n = n(),
      median = median(value, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    gather(key = "stats", value = "value", -species, -var, -occ_n) %>%
    mutate(var = factor(var, levels = c("tmp", "ppt", "vpd", "cwd"))) %>%
    mutate(stats = factor(stats, levels = c("mean", "sd", "median", "q05", "q25", "q75", "q95"))) %>%
    arrange(var, stats) %>%
    mutate(var = str_c(var, "_occ_", stats)) %>%
    select(-stats) %>%
    mutate(var = factor(var, levels = unique(var))) %>%
    spread(key = "var", value = "value")

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
