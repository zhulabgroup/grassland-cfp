tidy_taxonomy_consolidate <- function(dat_community,
                                      consolidation_file = "alldata/input/community/species/consolidation.csv") {
  # species list from all experimental and observational data
  spp_comm_tbl <- bind_rows(
    dat_community$exp %>% select(species),
    dat_community$obs %>% select(species)
  ) %>%
    distinct(species) %>%
    arrange(species)

  # species consolidation list
  spp_consol_tbl <- consolidation_file %>%
    read_csv(col_types = "c")

  # combine
  spp_tbl <- spp_comm_tbl %>%
    filter(!str_detect(species, "DUMMY")) %>%
    rename("query_name" = "species") %>%
    bind_cols("consolidated_name" = as.character(NA)) %>%
    bind_rows(
      spp_consol_tbl %>%
        filter(new_species_name %in% spp_comm_tbl$species) %>%
        rename(
          "query_name" = "old_species_name",
          "consolidated_name" = "new_species_name"
        )
    ) %>%
    mutate(consolidated_name = ifelse(is.na(consolidated_name), query_name, consolidated_name)) %>%
    arrange(query_name) %>%
    distinct(query_name, consolidated_name)

  return(spp_tbl)
}
