googlesheets4::gs4_deauth()
mt_spp_tbl <- googlesheets4::read_sheet(
  "1ez43lbFMsTJwmkW_23icDabt30ttDhVZ-1vK8Ts_Mck",
  sheet = "Morgan Territory",
  n_max = Inf,
  na = c("", "NA") # guild = "NA" means NA
) %>%
  mutate(species_code = toupper(species_code))

mt_plt_tbl <-
  readxl::read_xlsx(
    "data/community/raw/MorganTerritory/UCBerkeley Range Lab Morgan Territory all plots transect data 2003-2011.xlsx",
    sheet = "Grazed status"
  ) %>%
  separate_rows(year, sep = "-") %>% # 2003-2004 and 2010-2011 to separate rows
  select(year, plot = `plot ID`, grazed, type)

mt_com_tbl <- readxl::read_xlsx(
  "data/community/raw/MorganTerritory/UCBerkeley Range Lab Morgan Territory all plots transect data 2003-2011.xlsx",
  sheet = "MT1-MT16 2003-2011"
) %>%
  select(year, plot = `plot ID`, transect, point, species_code = species) %>%
  mutate(species_code = toupper(species_code)) %>%
  group_by(year, plot, species_code) %>%
  summarize(hits = n()) %>% # hits as total counts
  group_by(year, plot) %>%
  mutate(tot_hits = sum(hits)) %>%
  group_by(year, plot, species_code) %>%
  summarize(abund = hits / tot_hits) %>% # rel abundance
  left_join(mt_spp_tbl, by = "species_code") %>% # only keep species in species table
  filter(keep) %>% # only keep real species (excluding litter, moss, rock, soil, etc.)
  mutate(
    abund_type = "point_intercept",
    site = "morganterritory",
    year = as.integer(year)
  ) %>%
  select(site, year, plot, species = corrected_species, guild = corrected_guild, abund, abund_type) %>%
  right_join( # join plot table
    mt_plt_tbl %>%
      filter(grazed == "n") %>% # non-grazed plots
      mutate(year = as.integer(year)) %>%
      select(year, plot),
    by = c("year", "plot")
  )
