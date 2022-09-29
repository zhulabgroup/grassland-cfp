# import data
UCSCspp <- read_csv("data/community/raw/HollData/ElkhornSpecies.csv") %>%
  mutate(species.code = tolower(species.code))
UCSC_guilds <- read_csv("data/community/raw/HollData/ElkhornGuilds.csv")
ucsc_counts <- list()
for (i in 1:14) {
  year <- as.character(1999:2012)[i]
  skips <- c(4, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0)
  if (i != 10) {
    filename <- paste0("data/community/raw/HollData/Elk_", year, ".csv")
  } else {
    filename <- "data/community/raw/HollData/ucsc_08.csv"
  }
  out <- read_csv(filename, skip = skips[i], show_col_types = FALSE)
  ucsc_counts[[year]] <- out
}
ucsc_counts <- lapply(seq_along(ucsc_counts), function(i) {
  rename_with(ucsc_counts[[i]], tolower) %>%
    rename_with(~ sub("frequency", "freq", sub("treatment", "treat", sub("replicate", "rep", sub("9$", "", .x))))) %>%
    select(-starts_with("comment"), -starts_with("..."), -starts_with("0"))
})

# lapply(ucsc_counts, names)

# clean data
# After creating objects for each year, we will clean each to a compatible structure.
ucsc_counts_clean <- list()
ucsc_counts_clean[[1]] <- ucsc_counts[[1]] %>% cleanElkhornData("anaarv", "bare", site = "ucsc")
ucsc_counts_clean[[2]] <- ucsc_counts[[2]] %>%
  cleanElkhornData("aircar", "naspul", site = "ucsc") %>%
  cleanElkhornPlot()
ucsc_counts_clean[[3]] <- ucsc_counts[[3]] %>%
  cleanElkhornData("anaarv", "thatch", site = "ucsc") %>%
  cleanElkhornPlot()
ucsc_counts_clean[[4]] <- ucsc_counts[[4]] %>% cleanElkhornData("anaarv", "thatch", site = "ucsc")
ucsc_counts_clean[[5]] <- ucsc_counts[[5]] %>% cleanElkhornData("aircar", "vulpsp", site = "ucsc")
ucsc_counts_clean[[6]] <- ucsc_counts[[6]] %>% cleanElkhornData("aircar", "vulpsp", site = "ucsc")
ucsc_counts_clean[[7]] <- ucsc_counts[[7]] %>% cleanElkhornData("aircar", "vulpsp", site = "ucsc")
ucsc_counts_clean[[8]] <- ucsc_counts[[8]] %>% cleanElkhornData("aircar", "vulpsp", site = "ucsc")
ucsc_counts_clean[[9]] <- ucsc_counts[[9]] %>% cleanElkhornData("aircar", "vulpsp", site = "ucsc")
ucsc_counts_clean[[10]] <- ucsc_counts[[10]] %>% cleanElkhornData("aircar", "brohor", site = "ucsc")
ucsc_counts_clean[[11]] <- ucsc_counts[[11]] %>% cleanElkhornData("aircar", "vulpsp", site = "ucsc")
ucsc_counts_clean[[12]] <- ucsc_counts[[12]] %>% cleanElkhornData("aircar", "vulpsp", site = "ucsc")
ucsc_counts_clean[[13]] <- ucsc_counts[[13]] %>% cleanElkhornData("aircar", "vulpsp", site = "ucsc")
ucsc_counts_clean[[14]] <- ucsc_counts[[14]] %>% cleanElkhornData("aircar", "vulpsp", site = "ucsc")

# lapply(ucsc_counts_clean, dim)
for (i in 1:length(ucsc_counts_clean)) {
  ucsc_counts_clean[[i]]$year <- (1999:2012)[i]
}

# Once the data is cleaned and in a similar format, it's ready to be compiled.
ucsc_counts_df <- do.call(rbind, ucsc_counts_clean) %>%
  left_join(UCSCspp, by = "species.code") %>%
  filter(intercepts > 0) %>%
  select("rep", "year", "species.name", "intercepts", "guild") %>%
  mutate(
    site = "ucsc",
    species.name = factor(species.name)
  )
# removing unwanted guilds (moss, litter, etc.)
ucsc_counts_df <- ucsc_counts_df %>%
  group_by(rep, year, guild, species.name) %>%
  summarize(intercepts = sum(intercepts)) %>%
  filter(
    guild != "Moss",
    guild != "Thatch",
    guild != "Bare"
  )
# unique(ucsc_counts_df$species.name)
# unique(ucsc_counts_df$guild)
elk_guilds <- ucsc_counts_df %>%
  group_by(rep, year, guild) %>%
  summarize(guildtotal = sum(intercepts)) %>%
  ungroup() %>%
  group_by(year, guild) %>%
  summarize(mean.intercepts = mean(guildtotal))

ucsc_data <- ucsc_counts_df %>%
  filter(
    species.name != "Moss",
    species.name != "Thatch",
    species.name != "Bare ground"
  ) %>%
  mutate(site = "ucsc")

ucsc_data <- ucsc_data %>%
  as_tibble() %>%
  select(site, year, plot = rep, species = species.name, guild, abund = intercepts) %>%
  mutate(year = as.integer(year), species = as.character(species) %>% str_trim(), guild = as.character(guild), abund = as.integer(abund), abund_type = "point_intercept") %>%
  arrange(site, year, plot, species)
