# compile a unique species-guild list from tidy data
tidy_tax_tbl <- tidy_exp_tbl %>%
  select(species, guild) %>%
  bind_rows(tidy_obs_tbl %>% select(species, guild)) %>%
  distinct() %>%
  arrange(species, guild)

# download species name using the taxize package.
taxize::get_gbifid_(tidy_tax_tbl$species) %>%
  as_tibble_col(column_name = "gbif") %>%
  mutate(queryname = tidy_tax_tbl$species) %>%
  unnest(cols = c(gbif)) %>%
  select(queryname, everything()) %>%
  write_rds(str_c(.path$com_spp, "taxonomy/taxize-2022-09-30.rds"))
