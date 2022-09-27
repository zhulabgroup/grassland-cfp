# community data
Angelo <- read_csv(paste0(.path$com_raw, "Angelo/Angelo_CommCompData.csv")) %>%
  filter(TMT == "C") %>%
  pivot_longer(-c(Year, Plot, TMT), names_to = "species", values_to = "cover")
names(Angelo) <- tolower(names(Angelo))

# species data
Angelo_spp <- read_csv(paste0(.path$com_raw, "Angelo/Angelo_spp_guilds.csv"))
Angelo <- left_join(Angelo, Angelo_spp, by = "species") %>%
  mutate(Site = "angelo") %>%
  filter(cover > 0)

# filter
angelo_data <- Angelo %>%
  select("tmt", "plot", "year", "species.name", "cover", "guild") %>%
  mutate(site = "angelo") %>%
  filter(
    guild != "Bare",
    guild != "Moss",
    guild != "Litter"
  ) %>%
  mutate(site = "angelo") %>% 
  select(site, year, plot, species = species.name, guild, abund = cover) %>%
  mutate(species = str_trim(species), guild = as.character(guild), abund_type = "cover") %>%
  arrange(site, year, plot, species)
