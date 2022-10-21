# add genus-only species as dummy and set their niche as average
Avena_tbl <- niche_tbl %>%
  filter(species %in% c(
    "Avena barbata",
    "Avena fatua"
  )) %>%
  summarize(across(-c(species:occ_n), mean))

Festuca_tbl <- niche_tbl %>%
  filter(species %in% c(
    "Festuca bromoides",
    "Festuca perennis"
  )) %>%
  summarize(across(-c(species:occ_n), mean))

Hypochaeris_tbl <- niche_tbl %>%
  filter(species %in% c(
    "Hypochaeris glabra",
    "Hypochaeris radicata"
  )) %>%
  summarize(across(-c(species:occ_n), mean))

# Raphanus_tbl <- niche_tbl %>%
#   filter(species %in% c(
#     "Raphanus sativus",
#     "Raphanus raphanistrum" # this species doesn't exist from GBIF download, so no need to add dummy
#   )) %>%
#   summarize(across(-c(species:occ_n), mean))

niche_tbl <- niche_tbl %>%
  add_row(species = "Avena DUMMY", occ_n = NA, Avena_tbl) %>%
  add_row(species = "Festuca DUMMY", occ_n = NA, Festuca_tbl) %>%
  add_row(species = "Hypochaeris DUMMY", occ_n = NA, Hypochaeris_tbl)
