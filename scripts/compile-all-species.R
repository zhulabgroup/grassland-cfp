# species list from all experimental and observational data
spp_comm_tbl <- read_rds(.path$com_exp) %>%
  select(species) %>%
  bind_rows(
    read_rds(.path$com_obs) %>%
      select(species)
  ) %>%
  distinct(species) %>%
  arrange(species)

# species consolidation list
spp_consol_tbl <- .path$com_spp %>%
  str_c("consolidation.csv") %>%
  read_csv(col_types = "c")

# combine
spp_tbl <- spp_comm_tbl %>%
  rename("query_name" = "species") %>%
  bind_cols("consolidated_name" = as.character(NA)) %>%
  bind_rows(
    spp_consol_tbl %>%
      rename(
        "query_name" = "old_species_name",
        "consolidated_name" = "new_species_name"
      )
  ) %>%
  filter(!str_detect(query_name, "DUMMY")) %>%
  arrange(query_name) %>%
  distinct(query_name, consolidated_name)

rm(spp_comm_tbl, spp_consol_tbl)
