# community data
com_tbl <- read_csv(paste0(.path$com_raw, "Angelo/Angelo_CommCompData.csv"),
                   col_types = cols(.default = "d", Year = "i", Plot = "i", TMT = "c")) %>%
  filter(TMT == "C") %>%
  pivot_longer(-c(Year, Plot, TMT), names_to = "species", values_to = "cover") %>% 
  rename_with(tolower)

# species data
spp_tbl <- read_csv(paste0(.path$com_raw, "Angelo/Angelo_spp_guilds.csv"),
                    col_types = "c")

# combine
angelo_data <- com_tbl %>% 
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
