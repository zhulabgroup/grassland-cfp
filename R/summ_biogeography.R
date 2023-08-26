summ_biogeography <- function(dat_occ) {
  v_source <- names(dat_occ)

  ls_df <- vector(mode = "list")
  for (source in v_source) {
    ls_df[[source]] <- data.frame(
      source = source,
      n_occ = nrow(dat_occ[[source]]),
      n_sp = dat_occ[[source]] %>% pull(consolidatedName) %>% unique() %>% length()
    )
  }
  df <- bind_rows(ls_df)

  return(df)
}
