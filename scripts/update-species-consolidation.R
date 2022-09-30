# update species consolidation table with Morgan Territory manual correct (Justin)
# only run once

# Justin's manual species table
googlesheets4::gs4_deauth()
justin_tbl <- googlesheets4::read_sheet(
  "1ez43lbFMsTJwmkW_23icDabt30ttDhVZ-1vK8Ts_Mck",
  sheet = "Morgan Territory",
  n_max = Inf,
  na = c("", "NA") # guild = "NA" means NA
) %>%
  mutate(species_code = toupper(species_code))

# old master consolidation table
googlesheets4::gs4_deauth()
master_tbl <- googlesheets4::read_sheet(
  "1ez43lbFMsTJwmkW_23icDabt30ttDhVZ-1vK8Ts_Mck",
  sheet = "Consolidation",
  na = c("", "NA")
)

# find out species to consolidate
new_consolidation_tbl <- justin_tbl %>%
  filter(keep, corrected_guild != "DUMMY") %>%
  group_by(corrected_species) %>%
  filter(n() > 1) %>% # if corrected species appears more than once
  select(
    old_species_name = original_species,
    new_species_name = corrected_species
  ) %>%
  ungroup() %>%
  bind_rows(master_tbl) %>%
  distinct() # remove duplicated rows using all variables

write_csv(new_consolidation_tbl, str_c(.path$com_spp, "consolidation.csv"))
rm(list = ls())
