scide_tbl <- .path$com_raw %>%
  str_c("SantaCruzIDE/IDESpeciesComp_LongFormat_2015_2021.csv") %>%
  read_csv(col_types = c("ciicdicdcc")) %>%
  mutate(
    site = Site %>%
      str_replace(" ", "") %>%
      tolower() %>%
      str_c("scide_", .),
    year = Year,
    plot = str_c(Plot, Block, sep = "-"),
    treat = case_when(
      Treatment == "Ambient" ~ "_",
      Treatment == "Drought" ~ "D"
    ),
    species = Species,
    guild = str_c(
      case_when( # origin
        Origin == "Native" ~ "N",
        Origin == "Non-native" ~ "E", # exotic
        is.na(Origin) ~ "U" # unknown
      ),
      case_when( # lifeform
        Lifeform == "Annual" ~ "AU", # annual unknown
        Lifeform == "Annual forb" ~ "AF",
        Lifeform == "Annual grass" ~ "AG",
        Lifeform == "Annual rush" ~ "AR",
        Lifeform == "N-fixer" ~ "NF",
        Lifeform == "Perennial forb" ~ "PF",
        Lifeform == "Perennial grass" ~ "PG",
        Lifeform == "Perennial sedge" ~ "PS",
        Lifeform == "Rhizomatous forb" ~ "RF",
        Lifeform == "Rhizomatous grass" ~ "RG",
        Lifeform == "Rhizomatous rush" ~ "RR",
        Lifeform %in% c("Shrub", "Shurb") ~ "S",
        is.na(Lifeform) ~ "U" # unknown
      )
    ),
    abund = Cover,
    abund_type = "cover"
  ) %>%
  select(site, year, plot, treat, species, guild, abund, abund_type)
