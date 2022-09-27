# community data spreadsheet
carrizo_com <- .path$com_raw %>%
  paste0("Carrizo/2022-06-28/Carrizo_data for Joise_2007_2021.xlsx") %>%
  readxl::read_xlsx(
    sheet = 1,
    col_types = c(
      "date", "numeric", "text", "text", "text",
      "numeric", "text", "text", "text", "text",
      "text", "numeric", "text", "text", "text",
      "text", "text", "text", "text", "numeric",
      "numeric", "text"
    ),
    na = "NA"
  ) %>%
  filter(cattle == "Ungrazed") %>% # only ungrazed plots
  select(
    plot = Site, year,
    species_code = SpeciesCode, species_name = Species.Name,
    intercept = Count2 # Updated number of hits, where any species that was present but not hit were given a 1
  ) %>%
  filter(intercept > 0, !is.na(intercept))

# species data
carrizo_spp <- .path$com_raw %>%
  paste0("Carrizo/2022-07-05/Carrizo_global_species list_v2.csv") %>%
  read_csv(col_types = "c") %>%
  mutate(guild = case_when(
    newform == "ia" ~ "EAG", # Invasive (Exotic) Annual Grass
    newform == "ih" ~ "EAF", # Invasive (Exotic) Annual Forb
    newform == "ip" ~ "EPF", # Invasive (Exotic) Perennial Forb
    newform == "na" ~ "NAG", # Native Annual Grass
    newform == "nh" ~ "NAF", # Native Annual Forb
    newform == "np" ~ "NPF", # Native	Perennial	Forb
    newform == "npg" ~ "NPG", # Native Perennial Grass
    newform == "nps" ~ "NPS" # Native Perennial Shrub
  ))

# join community and species data
carrizo_data <- carrizo_com %>%
  filter(
    plot %in% c("EP2", "EP3", "EP4", "EP6", "SC1", "SC2", "SC7", "SC9"),
    # !(species_code == "AMSMEN" & is.na(species_name)),
    !(species_code == "ASTspp" & is.na(species_name)),
    !(species_code == "BARE" & species_name == "Bare ground"),
    !(species_code == "BURROW" & species_name == "Animal burrow"),
    !(species_code == "CRYPTO" & is.na(species_name)),
    # !(species_code == "EUPPOL" & is.na(species_name)),
    !(species_code == "FRESHDIRT" & species_name == "freshly disturbed dirt"),
    !(species_code == "GOPHER" & species_name == "gopher mound"),
    !(species_code == "HOLE" & species_name == "hole in ground"),
    !(species_code == "LITTER" & species_name == "litter"),
    !(species_code == "MOSS" & is.na(species_name)),
    !(species_code == "UNK 11" & is.na(species_name))
  ) %>%
  left_join(carrizo_spp %>% select(species_code = spp.code, species_name = SCIENTIFIC.NAME, guild),
    by = c("species_code", "species_name")
  ) %>%
  mutate(species_name = case_when(
    species_code == "AMSMEN" ~ "Amsinckia menziesii",
    species_code == "EUPPOL" ~ "Chamaesyce polycarpa (Euphorbia polycarpa)",
    TRUE ~ species_name
  )) %>%
  group_by(plot, year, species_code, species_name, guild) %>%
  summarize(abund = sum(intercept)) %>%
  ungroup() %>%
  mutate(site = "carrizo", species_name = str_trim(species_name), abund_type = "point_intercept") %>%
  select(site, year, plot, species = species_name, guild, abund, abund_type) %>%
  arrange(site, year, plot, species)
