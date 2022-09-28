# site and species data
McL_Sites <- read_csv("data/community/raw/McLaughlin/AbioticSiteDataNew.csv") %>%
  rename(Site = site)
McL_spp <- read_csv("data/community/raw/McLaughlin/McLaughlin_FunctionalGroups.csv")

# annual site community data
McLAnn <- read_csv("data/community/raw/McLaughlin/Core_Community_Data2019.csv",
                   col_types = c(
                     id = "i", Year = "i", Site = "d", Quadrat = "i", Species_Name = "c",
                     Cover = "d", Notes = "c", Merged_Species = "c"
                   )
)

# combining the data with site infomation, filtering to just serpentine sites
McLAnn <- left_join(McLAnn, McL_Sites, by = "Site")
McLAnn <- McLAnn %>%
  filter(Serpentine == "N") %>%
  rename(plot = Site)

# combining the guild information, creating a "guild" column, selecting only the desired info
McLAnn <- left_join(McLAnn, McL_spp, by = "Species_Name") %>%
  rename(
    nat.exo = Native.Exotic,
    lifeform = Grass.Forb.Shrub,
    ann.per = Annual.Perennial
  ) %>%
  mutate(
    Guild1 = str_sub(nat.exo, 1, 1),
    Guild3 = str_sub(lifeform, 1, 1),
    Guild2 = str_sub(ann.per, 1, 1)
  ) %>%
  unite(Guild1, Guild2, col = "Guild1", sep = "") %>%
  unite(Guild1, Guild3, col = "guild", sep = "") %>%
  mutate(Site = "mclann") %>%
  select(Year, Site, plot, Quadrat, Cover, Species_Name, nat.exo, ann.per, lifeform, guild) %>%
  group_by(Year, Site, plot, Species_Name, nat.exo, ann.per, lifeform, guild) %>%
  summarise(cover = mean(Cover)) %>%
  ungroup()
mclann_data <- McLAnn %>%
  rename(
    species.name = Species_Name,
    year = Year
  ) %>%
  filter(
    species.name != "Bare",
    species.name != "Rock",
    species.name != "Unknown grass",
    species.name != "Unknown forb"
  ) %>%
  mutate(site = "mclann")

mclann_data <- mclann_data %>%
  as_tibble() %>%
  ungroup() %>%
  select(site = Site, year, plot, species = species.name, guild, abund = cover) %>%
  mutate(plot = as.integer(plot), species = as.character(species) %>% str_trim(), guild = as.character(guild), abund_type = "cover") %>%
  arrange(site, year, plot, species) %>%
  filter(!is.na(abund), abund > 0)



# importing the data
McLSerp <- read_csv("data/community/raw/McLaughlin/Core_Community_Data2019.csv")

# combining the data with site information, filtering to just serpentine sites
McLSerp <- left_join(McLSerp, McL_Sites, by = "Site")
McLSerp <- McLSerp %>%
  filter(Serpentine == "S") %>%
  rename(plot = Site)

# combining the guild information, creating a "guild" column, selecting only the desired info
McLSerp <- left_join(McLSerp, McL_spp, by = "Species_Name") %>%
  rename(
    nat.exo = Native.Exotic,
    lifeform = Grass.Forb.Shrub,
    ann.per = Annual.Perennial
  ) %>%
  mutate(
    Guild1 = str_sub(nat.exo, 1, 1),
    Guild3 = str_sub(lifeform, 1, 1),
    Guild2 = str_sub(ann.per, 1, 1)
  ) %>%
  unite(Guild1, Guild2, col = "Guild1", sep = "") %>%
  unite(Guild1, Guild3, col = "guild", sep = "") %>%
  mutate(Site = "mclserp") %>%
  select(Year, Site, plot, Quadrat, Cover, Species_Name, nat.exo, ann.per, lifeform, guild) %>%
  group_by(Year, Site, plot, Species_Name, nat.exo, ann.per, lifeform, guild) %>%
  summarise(cover = mean(Cover))
mclserp_data <- McLSerp %>%
  rename(
    species.name = Species_Name,
    year = Year
  ) %>%
  filter(
    species.name != "Bare",
    species.name != "Rock",
    species.name != "Unknown forb"
  ) %>%
  mutate(site = "mclserp")

mclserp_data <- mclserp_data %>%
  as_tibble() %>%
  ungroup() %>%
  select(site = Site, year, plot, species = species.name, guild, abund = cover) %>%
  mutate(plot = as.integer(plot), species = as.character(species) %>% str_trim(), guild = as.character(guild), abund_type = "cover") %>%
  arrange(site, year, plot, species) %>%
  filter(!is.na(abund), abund > 0)
