# import data
elkspp <- read_csv("data/community/raw/HollData/ElkhornSpecies.csv") %>%
  mutate(species.code = tolower(species.code))
elk_guilds <- read_csv("data/community/raw/HollData/ElkhornGuilds.csv")
elk_counts <- list()
for (i in 1:20) {
  year <- as.character(1999:2018)[i]
  skips <- c(4, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 2, 1, 1, 1, 1, 1)
  filename <- paste0("data/community/raw/HollData/Elk_", year, ".csv")
  out <- read_csv(filename, skip = skips[i], show_col_types = FALSE)
  # elk_counts[[year]] <- select_if(out, has_data)
  elk_counts[[year]] <- out
}
elk_counts <- lapply(seq_along(elk_counts), function(i) {
  rename_with(elk_counts[[i]], tolower) %>%
    rename_with(~ sub("frequency", "freq", sub("treatment", "treat", sub("replicate", "rep", sub("9$", "", .x))))) %>%
    select(-starts_with("comment"), -starts_with("..."), -starts_with("0"))
})

# Because these data are in separate files (one year per file), we need to clean the data and compile all of the years into a single object. To clean the data, we selected only the Elkhorn plots (these files also contained the data from two other sites; UCSC and Swanton, removed the non-control plots, restructured the data to make sure only non-zero datapoints were included, and added the year. We then combined the file with species guild information to each yearly object.
elk_counts_clean <- list()
elk_counts_clean[[1]] <- elk_counts[[1]] %>% cleanElkhornData("anaarv", "bare")
elk_counts_clean[[2]] <- elk_counts[[2]] %>%
  cleanElkhornData("aircar", "naspul") %>%
  cleanElkhornPlot()
elk_counts_clean[[3]] <- elk_counts[[3]] %>%
  cleanElkhornData("anaarv", "thatch") %>%
  cleanElkhornPlot()
elk_counts_clean[[4]] <- elk_counts[[4]] %>% cleanElkhornData("anaarv", "thatch")
elk_counts_clean[[5]] <- elk_counts[[5]] %>% cleanElkhornData("aircar", "vulpsp")
elk_counts_clean[[6]] <- elk_counts[[6]] %>% cleanElkhornData("aircar", "vulpsp")
elk_counts_clean[[7]] <- elk_counts[[7]] %>% cleanElkhornData("aircar", "vulpsp")
elk_counts_clean[[8]] <- elk_counts[[8]] %>% cleanElkhornData("aircar", "vulpsp")
elk_counts_clean[[9]] <- elk_counts[[9]] %>% cleanElkhornData("aircar", "vulpsp")
elk_counts_clean[[10]] <- elk_counts[[10]] %>% cleanElkhornData("aircar", "hormur")
elk_counts_clean[[11]] <- elk_counts[[11]] %>% cleanElkhornData("aircar", "vulpsp")
elk_counts_clean[[12]] <- elk_counts[[12]] %>% cleanElkhornData("aircar", "vulpsp")
elk_counts_clean[[13]] <- elk_counts[[13]] %>% cleanElkhornData("aircar", "vulpsp")
elk_counts_clean[[14]] <- elk_counts[[14]] %>% cleanElkhornData("aircar", "vulpsp")
elk_counts_clean[[15]] <- elk_counts[[15]] %>% cleanElkhornData("aircar", "vulpsp")
elk_counts_clean[[16]] <- elk_counts[[16]] %>% cleanElkhornData("aircar", "vulpsp")
elk_counts_clean[[17]] <- elk_counts[[17]] %>% cleanElkhornData("aircar", "vulpsp")
elk_counts_clean[[18]] <- elk_counts[[18]] %>% cleanElkhornData("aircar", "vulpia")
elk_counts_clean[[19]] <- elk_counts[[19]] %>% cleanElkhornData("aircar", "vulpia")
elk_counts_clean[[20]] <- elk_counts[[20]] %>% cleanElkhornData("aircar", "vulpia")

# lapply(elk_counts_clean, dim)
for (i in 1:length(elk_counts_clean)) {
  elk_counts_clean[[i]]$year <- (1999:2018)[i]
}

# At this point, all that is left to do is to combine the objects so that we have one large object with each year of data in it.

elk_counts_df <- do.call(rbind, elk_counts_clean) %>%
  left_join(elkspp, by = "species.code") %>%
  filter(intercepts > 0) %>%
  select("rep", "year", "species.name", "intercepts", "guild") %>%
  mutate(
    site = "elkhorn",
    species.name = factor(species.name)
  )
# removing unwanted guilds (moss, litter, etc.)
elk_counts_df <- elk_counts_df %>%
  group_by(rep, year, guild, species.name) %>%
  summarize(intercepts = sum(intercepts)) %>%
  filter(
    guild != "Moss",
    guild != "Thatch",
    guild != "Bare"
  )
# unique(elk_counts_df$species.name)
# unique(elk_counts_df$guild)
elk_guilds <- elk_counts_df %>%
  group_by(rep, year, guild) %>%
  summarize(guildtotal = sum(intercepts)) %>%
  ungroup() %>%
  group_by(year, guild) %>%
  summarize(mean.intercepts = mean(guildtotal))

elkhorn_tbl <- elk_counts_df %>%
  filter(
    species.name != "Moss",
    species.name != "Thatch",
    species.name != "Bare ground"
  ) %>%
  mutate(site = "elkhorn")

elkhorn_tbl <- elkhorn_tbl %>%
  as_tibble() %>%
  select(site, year, plot = rep, species = species.name, guild, abund = intercepts) %>%
  mutate(year = as.integer(year), species = as.character(species) %>% str_trim(), guild = as.character(guild), abund = as.integer(abund), abund_type = "point_intercept") %>%
  arrange(site, year, plot, species)
