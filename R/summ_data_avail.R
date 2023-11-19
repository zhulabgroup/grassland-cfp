#' @export
summ_data_avail <- function(dat_community, type, subset = NULL) {
  df_data_avail <- bind_rows(
    dat_community$exp %>%
      distinct(site, plot, year),
    dat_community$obs %>%
      distinct(site, plot, year)
  ) %>%
    rowwise() %>%
    mutate(site = str_split(site, "_", simplify = T)[1]) %>%
    ungroup() %>%
    filter(!(site == "jrgce" & year == 1998)) %>% # pre-treatment
    filter(!(site == "scide" & year == 2015)) %>% # pre-treatment
    mutate(sitename = read_site_name()[site]) %>%
    filter(!is.na(sitename)) %>%
    group_by(site, sitename, year) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    arrange(count) %>%
    mutate(sitename = factor(sitename, levels = read_site_name()))

  if (type == "short") {
    df_data_avail <- df_data_avail %>%
      group_by(site, sitename) %>%
      summarise(nyear = unique(year) %>% length()) %>%
      arrange(nyear)
  }

  if (!is.null(subset)) {
    if (subset == "obs") {
      df_data_avail <- df_data_avail %>%
        filter(!str_detect(sitename, "Experiment"))
    }
    if (subset == "exp") {
      df_data_avail <- df_data_avail %>%
        filter(str_detect(sitename, "Experiment"))
    }
  }

  return(df_data_avail)
}
