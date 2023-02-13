# import misspelling table
misspelling_tbl <- .path$com_spp %>%
  str_c("misspelling.csv") %>%
  read_csv(col_types = "cccclcd")

# rename species in experimental data
final_exp_tbl <- tidy_exp_tbl %>%
  rename(
    "original_species" = "species",
    "original_guild" = "guild"
  ) %>%
  left_join(
    misspelling_tbl,
    by = c("original_species", "original_guild")
  ) %>%
  filter(keep) %>%
  group_by(site, year, plot, treat, corrected_species, corrected_guild, abund_type) %>%
  summarize(abund = sum(abund)) %>% # combine abundances for same species with old and new names
  ungroup() %>%
  select(
    site, year, plot, treat,
    species = corrected_species, guild = corrected_guild,
    abund, abund_type
  ) %>%
  arrange(site, year, plot, species)

# rename species in observational data
final_obs_tbl <- tidy_obs_tbl %>%
  rename(
    "original_species" = "species",
    "original_guild" = "guild"
  ) %>%
  left_join(
    misspelling_tbl,
    by = c("original_species", "original_guild")
  ) %>%
  filter(keep) %>%
  group_by(site, year, plot, corrected_species, corrected_guild, abund_type) %>%
  summarize(abund = sum(abund)) %>% # combine abundances for same species with old and new names
  ungroup() %>%
  select(
    site, year, plot,
    species = corrected_species, guild = corrected_guild,
    abund, abund_type
  ) %>%
  arrange(site, year, plot, species)
