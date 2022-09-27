# read species data
eb_spp_tbl <- .path$com_raw %>%
  paste0("Dudney/_VegSpCodeAttributes_2010.xlsx") %>%
  readxl::read_xlsx() %>%
  mutate(
    species_code = toupper(species),
    guild = str_c(
      case_when(
        `native/exotic` == "e" ~ "E", # Exotic
        `native/exotic` == "n" ~ "N", # Native
        TRUE ~ "U" # else is Unknown
      ),
      case_when(
        `annual/perennial` == "a" ~ "A", # Annual
        `annual/perennial` == "p" ~ "P", # Perennial
        TRUE ~ "U" # else is Unknown
      ),
      case_when(
        `forb/grass` == "f" ~ "F", # Forb
        `forb/grass` == "g" ~ "G", # Grass
        `forb/grass` == "n" ~ "R", # Rush
        `forb/grass` == "s" ~ "S", # Shurb
        `forb/grass` == "t" ~ "T", # Tree
        TRUE ~ "U" # else is Unknown
      )
    )
  ) %>%
  select(species_code, species = latin, guild)

# join community data
eb_com_tbl <- .path$com_raw %>%
  paste0("Dudney/EBRPD_2002thru2012_Dec2013_BRXX AVXX updated.csv") %>%
  read_csv(col_types = "cicidc") %>%
  rename(plot = plot.ID, species_code = species) %>%
  group_by(site, year, plot, species_code) %>%
  summarize(hits = n()) %>%
  group_by(site, year, plot) %>%
  mutate(tot_hits = sum(hits)) %>%
  group_by(site, year, plot, species_code) %>%
  summarize(abund = hits / tot_hits) %>%
  left_join(eb_spp_tbl, by = "species_code") %>%
  mutate(abund_type = "point_intercept") %>%
  mutate(species = coalesce(species, species_code)) %>% # if species == NA, replace with species_code
  filter(
    !(species %in% c("bare", "COWPIE", "gopher", "litter", "mosses", "rock", "soil")),
    !str_detect(species, "unknown"),
    is.na(guild) | guild != "UUU"
  ) %>%
  select(site, year, plot, species, guild, abund, abund_type) %>%
  arrange(site, year, plot, species)
