tidy_all <- function(
    in_dir = "input/community/raw/",
    out_dir = "intermediate/observation-experiment/tidy-community/") {
  # observational sites
  tidy_angelo(in_dir) %>% write_csv(str_c(out_dir, "angelo.csv"))
  tidy_carrizo(in_dir) %>% write_csv(str_c(out_dir, "carrizo.csv"))

  eastbay_tbl <- tidy_eastbay(in_dir)
  parse_pleasantonridge(eastbay_tbl) %>% write_csv(str_c(out_dir, "pleasantonridge.csv"))
  parse_sunol(eastbay_tbl) %>% write_csv(str_c(out_dir, "sunol.csv"))
  parse_vascocaves(eastbay_tbl) %>% write_csv(str_c(out_dir, "vascocaves.csv"))
}

# observation -------------------------------------------------------------

tidy_angelo <- function(base_dir) {
  # community data
  com_tbl <- base_dir %>%
    str_c("Angelo/Angelo_CommCompData.csv") %>%
    read_csv(col_types = cols(.default = "d", Year = "i", Plot = "i", TMT = "c")) %>%
    filter(TMT == "C") %>%
    pivot_longer(-c(Year, Plot, TMT), names_to = "species", values_to = "cover") %>%
    rename_with(tolower)

  # species data
  spp_tbl <- base_dir %>%
    str_c("Angelo/Angelo_spp_guilds.csv") %>%
    read_csv(col_types = "c")

  # combine
  angelo_tbl <- com_tbl %>%
    left_join(spp_tbl, by = "species") %>%
    select("tmt", "plot", "year", "species.name", "cover", "guild") %>%
    filter(
      cover > 0,
      guild != "Bare",
      guild != "Moss",
      guild != "Litter"
    ) %>%
    mutate(site = "angelo", species = str_trim(species.name), abund_type = "cover") %>%
    select(site, year, plot, species, guild, abund = cover, abund_type) %>%
    arrange(site, year, plot, species)

  return(angelo_tbl)
}

tidy_carrizo <- function(base_dir) {
  # community data spreadsheet
  com_tbl <- base_dir %>%
    str_c("Carrizo/2022-06-28/Carrizo_data for Joise_2007_2021.xlsx") %>%
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
  spp_tbl <- base_dir %>%
    str_c("Carrizo/2022-07-05/Carrizo_global_species list_v2.csv") %>%
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
  carrizo_tbl <- com_tbl %>%
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
    left_join(spp_tbl %>% select(species_code = spp.code, species_name = SCIENTIFIC.NAME, guild),
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

  return(carrizo_tbl)
}

tidy_eastbay <- function(base_dir) {
  # community data
  com_tbl <- base_dir %>%
    str_c("Dudney/EBRPD_2002thru2012_Dec2013_BRXX AVXX updated.csv") %>%
    read_csv(col_types = "cicidc") %>%
    rename(plot = plot.ID, species_code = species) %>%
    group_by(site, year, plot, species_code) %>%
    summarize(hits = n()) %>%
    group_by(site, year, plot) %>%
    mutate(tot_hits = sum(hits)) %>%
    group_by(site, year, plot, species_code) %>%
    summarize(abund = hits / tot_hits)

  # species data
  spp_tbl <- base_dir %>%
    str_c("Dudney/_VegSpCodeAttributes_2010.xlsx") %>%
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

  # combine
  eastbay_tbl <- com_tbl %>%
    left_join(spp_tbl, by = "species_code") %>%
    mutate(abund_type = "point_intercept") %>%
    mutate(species = coalesce(species, species_code)) %>% # if species == NA, replace with species_code
    filter(
      !(species %in% c("bare", "COWPIE", "gopher", "litter", "mosses", "rock", "soil")),
      !str_detect(species, "unknown"),
      is.na(guild) | guild != "UUU"
    ) %>%
    select(site, year, plot, species, guild, abund, abund_type) %>%
    arrange(site, year, plot, species)

  return(eastbay_tbl)
}

parse_pleasantonridge <- function(eastbay) {
  eastbay %>%
    filter(site == "PR", plot %in% str_c("PR", 4:9)) %>%
    mutate(site = "pleasantonridge")
}

parse_sunol <- function(eastbay) {
  eastbay %>%
    filter(site == "SU", plot %in% str_c("SU", 1:9)) %>%
    mutate(site = "sunol")
}

parse_vascocaves <- function(eastbay) {
  eastbay %>%
    filter(site == "VC", plot %in% str_c("VC", 1:10)) %>%
    filter(!(year == 2012 & plot %in% c("VC1", "VC8", "VC9"))) %>%
    mutate(site = "vascocaves")
}
