# update species misspelling table with Morgan Territory manual correct (Justin)
# only run once
source("scripts/clean-morganterritory.R")

# Justin's manual species table
googlesheets4::gs4_deauth()
justin_tbl <- googlesheets4::read_sheet(
  "1ez43lbFMsTJwmkW_23icDabt30ttDhVZ-1vK8Ts_Mck",
  sheet = "Morgan Territory",
  n_max = Inf,
  na = c("", "NA") # guild = "NA" means NA
) %>%
  mutate(species_code = toupper(species_code))

# old master misspelling table
master_tbl <- googlesheets4::read_sheet(
  "1ez43lbFMsTJwmkW_23icDabt30ttDhVZ-1vK8Ts_Mck",
  sheet = "Misspelling",
  n_max = Inf,
  na = c("", "NA") # guild = "NA" means NA
)

# Justin's species in addition to master species table
addl_tbl <- left_join(spp_tbl, master_tbl,
  by = c("species" = "original_species", "guild" = "original_guild")
) %>%
  filter(is.na(keep)) %>% # not in misspelling
  select(species_code, original_species = species, guild) %>%
  left_join(justin_tbl) %>%
  mutate(
    comment = as.character(NA),
    `# occurrences in dataset` = as.numeric(NA)
  ) %>%
  select(original_species,
    original_guild = guild,
    corrected_species, corrected_guild,
    keep, comment, `# occurrences in dataset`
  )

# merge to create new misspelling table
new_misspelling_tbl <- bind_rows(master_tbl, addl_tbl) %>%
  arrange(original_species, original_guild)

write_csv(new_misspelling_tbl, str_c(.path$com_spp, "misspelling.csv"))

# check
morganterritory_tbl %>%
  distinct(species, guild) %>%
  left_join(new_misspelling_tbl,
    by = c(
      "species" = "original_species",
      "guild" = "original_guild"
    )
  ) %>%
  filter(is.na(keep)) # should be none, meaning all species in datasheet are accounted for

rm(list = ls())
