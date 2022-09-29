excel_file <- .path$com_raw %>%
  str_c("MorganTerritory/UCBerkeley Range Lab Morgan Territory all plots transect data 2003-2011.xlsx")

com_tbl <- excel_file %>%
  readxl::read_xlsx(sheet = "MT1-MT16 2003-2011") %>%
  mutate(year = as.integer(year)) %>%
  select(year, plot = `plot ID`, transect, point, species_code = species) %>%
  mutate(species_code = toupper(species_code)) %>%
  group_by(year, plot, species_code) %>%
  summarize(hits = n()) %>% # hits as total counts
  group_by(year, plot) %>%
  mutate(tot_hits = sum(hits)) %>%
  group_by(year, plot, species_code) %>%
  summarize(abund = hits / tot_hits) # rel abundance

spp_tbl <- excel_file %>%
  readxl::read_xlsx(sheet = "Species codes & attributes") %>%
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
        `annual/perennial` == "a, p" ~ "AP", # both Annual and Perennial?
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

plt_tbl <- excel_file %>%
  readxl::read_xlsx(sheet = "Grazed status") %>%
  separate_rows(year, sep = "-") %>% # 2003-2004 and 2010-2011 to separate rows
  select(year, plot = `plot ID`, grazed, type) %>%
  mutate(year = as.integer(year))

morganterritory_tbl <- com_tbl %>%
  left_join(spp_tbl, by = "species_code") %>% # only keep species in species table
  mutate(
    site = "morganterritory",
    abund_type = "point_intercept"
  ) %>%
  right_join(filter(plt_tbl, grazed == "n"), # join non-grazed plots
    by = c("year", "plot")
  ) %>%
  select(site, year, plot, species, guild, abund, abund_type) %>%
  arrange(year, plot, species)
