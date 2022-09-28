# read cover data
cover_tbl <- .path$com_raw %>% 
  str_c("Jasper/JR_cover_forJosie.csv") %>% 
  read_csv(col_types = cols_only(year = "d", species = "c", cover = "d", uniqueID = "c")) %>%
  pivot_wider(names_from = species, values_from = cover) %>%
  replace(is.na(.), 0) %>%
  pivot_longer(cols = aghe:vumi, names_to = "species", values_to = "cover") %>%
  group_by(uniqueID, year, species) %>%
  summarize(cover = mean(cover)) %>% 
  ungroup()

# read species data
spp_tbl <- .path$com_raw %>% 
  str_c("Jasper/JR_speciesnames2.csv") %>% 
  read_csv(col_types = "c")

# combine
jasper_tbl <- cover_tbl %>%
  left_join(spp_tbl, by = "species") %>% 
  filter(cover > 0) %>%
  mutate(site = "jasper",
         species.name = str_trim(species.name),
         abund_type = "cover") %>% 
  select(site, year, plot = uniqueID,
         species = species.name, guild,
         abund = cover, abund_type) %>% 
  arrange(site, year, plot, species)
