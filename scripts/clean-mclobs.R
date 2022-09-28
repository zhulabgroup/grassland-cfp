# community data
com_tbl <- .path$com_raw %>%
  str_c("McLaughlin/Core_Community_Data2019.csv") %>%
  read_csv(col_types = "iiiicdcc") %>%
  rename_with(tolower) %>%
  rename(plot = site)

# plot data
plt_tbl <- .path$com_raw %>%
  str_c("McLaughlin/AbioticSiteDataNew.csv") %>%
  read_csv(col_types = "iicdd") %>%
  rename_with(tolower) %>%
  rename(plot = site)

# species data
spp_tbl <- .path$com_raw %>%
  str_c("McLaughlin/McLaughlin_FunctionalGroups.csv") %>%
  read_csv(col_types = "c") %>%
  rename_with(tolower) %>%
  mutate(
    species_name = str_trim(species_name),
    guild = str_c(
      str_sub(native.exotic, 1, 1),
      str_sub(annual.perennial, 1, 1),
      str_sub(grass.forb.shrub, 1, 1)
    )
  )

# annual plot data
mclann_data <- plt_tbl %>%
  filter(serpentine == "N") %>%
  left_join(com_tbl, by = "plot") %>%
  left_join(spp_tbl, by = "species_name") %>%
  group_by(year, plot, species_name, guild) %>%
  summarize(abund = mean(cover)) %>%
  ungroup() %>%
  filter(
    !is.na(abund),
    abund > 0,
    species_name != "Bare",
    species_name != "Rock",
    species_name != "Unknown grass",
    species_name != "Unknown forb"
  ) %>%
  mutate(
    site = "mclann",
    abund_type = "cover"
  ) %>%
  select(site, year, plot, species = species_name, guild, abund, abund_type) %>%
  arrange(site, year, plot, species)

# serpentine plot data
mclserp_data <- plt_tbl %>%
  filter(serpentine == "S") %>%
  left_join(com_tbl, by = "plot") %>%
  left_join(spp_tbl, by = "species_name") %>%
  group_by(year, plot, species_name, guild) %>%
  summarize(abund = mean(cover)) %>%
  ungroup() %>%
  filter(
    !is.na(abund),
    abund > 0,
    species_name != "Bare",
    species_name != "Rock",
    species_name != "Unknown grass",
    species_name != "Unknown forb"
  ) %>%
  mutate(
    site = "mclserp",
    abund_type = "cover"
  ) %>%
  select(site, year, plot, species = species_name, guild, abund, abund_type) %>%
  arrange(site, year, plot, species)
