#' @export
tidy_taxonomy_list <- function(dat_tidy_community, outdir = "alldata/input/community/species/taxonomy/", date = NULL) {
  # compile a unique species-guild list from tidy data
  df_tidy_taxa <- bind_rows(
    dat_tidy_community$exp %>% select(species, guild),
    dat_tidy_community$obs %>% select(species, guild)
  ) %>%
    distinct() %>%
    arrange(species, guild)

  # download species name using the taxize package.
  df_tidy_taxa_gbif <- taxize::get_gbifid_(df_tidy_taxa$species) %>%
    as_tibble_col(column_name = "gbif") %>%
    mutate(queryname = df_tidy_taxa$species) %>%
    unnest(cols = c(gbif)) %>%
    select(queryname, everything())

  if (is.null(date)) {
    date <- Sys.Date()
  }
  outfile <- str_c(outdir, "taxize-", date, ".rds")
  write_rds(df_tidy_taxa_gbif, outfile)

  return(outfile)
}
