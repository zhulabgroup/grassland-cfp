Jasper_Veg <- read_csv("data/community/raw/Jasper/JR_cover_forJosie.csv",
                       col_types = cols_only(year = "d", species = "c", cover = "d", uniqueID = "c")
) %>%
  pivot_wider(names_from = species, values_from = cover) %>%
  replace(is.na(.), 0) %>%
  pivot_longer(cols = aghe:vumi, names_to = "species", values_to = "cover") %>%
  group_by(uniqueID, year, species) %>%
  summarize(cover = mean(cover))
JR_spp <- read_csv("data/community/raw/Jasper/JR_speciesnames2.csv")
Jasper <- left_join(Jasper_Veg, JR_spp, by = "species")
Jasper_species <- Jasper %>%
  select("uniqueID", "year", "species.name", "cover", "guild") %>%
  mutate(site = "jasper") %>%
  filter(cover > 0) %>%
  ungroup()
jasper_spp_check <- Jasper_species %>%
  group_by(species.name, guild) %>%
  summarise(mean = mean(cover))

jasper_data <- Jasper_species %>%
  as_tibble() %>%
  select(site, year, plot = uniqueID, species = species.name, guild, abund = cover) %>%
  mutate(plot = as.character(plot), species = as.character(species) %>% str_trim(), guild = as.character(guild), abund_type = "cover") %>%
  arrange(site, year, plot, species)
