# import data
Swanspp <- read_csv("data/community/raw/HollData/ElkhornSpecies.csv") %>%
  mutate(species.code = tolower(species.code))
Swan_guilds <- read_csv("data/community/raw/HollData/ElkhornGuilds.csv")
swan_counts <- list()
for (i in 1:14) {
  year <- as.character(1999:2012)[i]
  skips <- c(4, 3, 3, 3, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0)
  if (i != 10) {
    filename <- paste0("data/community/raw/HollData/Elk_", year, ".csv")
  } else {
    filename <- "data/community/raw/HollData/swanton_08.csv"
  }
  out <- read_csv(filename, skip = skips[i], show_col_types = FALSE)
  swan_counts[[year]] <- out
}
swan_counts <- lapply(seq_along(swan_counts), function(i) {
  rename_with(swan_counts[[i]], tolower) %>%
    rename_with(~ sub("frequency", "freq", sub("treatment", "treat", sub("replicate", "rep", sub("9$", "", .x))))) %>%
    select(-starts_with("comment"), -starts_with("..."), -starts_with("0"))
})

# lapply(swan_counts, names)

# clean data
swan_counts_clean <- list()
swan_counts_clean[[1]] <- swan_counts[[1]] %>% clean_com("anaarv", "bare", site = "swa")
swan_counts_clean[[2]] <- swan_counts[[2]] %>%
  clean_com("aircar", "naspul", site = "swa") %>%
  clean_plt()
swan_counts_clean[[3]] <- swan_counts[[3]] %>%
  clean_com("anaarv", "thatch", site = "swa") %>%
  clean_plt()
swan_counts_clean[[4]] <- swan_counts[[4]] %>% clean_com("anaarv", "thatch", site = "swa")
swan_counts_clean[[5]] <- swan_counts[[5]] %>% clean_com("aircar", "vulpsp", site = "swa")
swan_counts_clean[[6]] <- swan_counts[[6]] %>% clean_com("aircar", "vulpsp", site = "swa")
swan_counts_clean[[7]] <- swan_counts[[7]] %>% clean_com("aircar", "vulpsp", site = "swa")
swan_counts_clean[[8]] <- swan_counts[[8]] %>% clean_com("aircar", "vulpsp", site = "swa")
swan_counts_clean[[9]] <- swan_counts[[9]] %>% clean_com("aircar", "vulpsp", site = "swa")
swan_counts_clean[[10]] <- swan_counts[[10]] %>% clean_com("aircar", "spearv", site = "swa")
swan_counts_clean[[11]] <- swan_counts[[11]] %>% clean_com("aircar", "vulpsp", site = "swa")
swan_counts_clean[[12]] <- swan_counts[[12]] %>% clean_com("aircar", "vulpsp", site = "swa")
swan_counts_clean[[13]] <- swan_counts[[13]] %>% clean_com("aircar", "vulpsp", site = "swa")
swan_counts_clean[[14]] <- swan_counts[[14]] %>% clean_com("aircar", "vulpsp", site = "swa")

# lapply(swan_counts_clean, dim)
for (i in 1:length(swan_counts_clean)) {
  swan_counts_clean[[i]]$year <- (1999:2012)[i]
}

swan_counts_df <- do.call(rbind, swan_counts_clean) %>%
  left_join(Swanspp, by = "species.code") %>%
  filter(intercepts > 0) %>%
  select("rep", "year", "species.name", "intercepts", "guild") %>%
  mutate(
    site = "swanton",
    species.name = factor(species.name)
  )
# removing unwanted guilds (moss, litter, etc.)
swan_counts_df <- swan_counts_df %>%
  group_by(rep, year, guild, species.name) %>%
  summarize(intercepts = sum(intercepts)) %>%
  filter(
    guild != "Moss",
    guild != "Thatch",
    guild != "Bare"
  )
# unique(swan_counts_df$species.name)
# unique(swan_counts_df$guild)
elk_guilds <- swan_counts_df %>%
  group_by(rep, year, guild) %>%
  summarize(guildtotal = sum(intercepts)) %>%
  ungroup() %>%
  group_by(year, guild) %>%
  summarize(mean.intercepts = mean(guildtotal))

swanton_tbl <- swan_counts_df %>%
  filter(
    species.name != "Moss",
    species.name != "Thatch",
    species.name != "Bare ground"
  ) %>%
  mutate(site = "swanton")

swanton_tbl <- swanton_tbl %>%
  as_tibble() %>%
  select(site, year, plot = rep, species = species.name, guild, abund = intercepts) %>%
  mutate(year = as.integer(year), species = as.character(species) %>% str_trim(), guild = as.character(guild), abund = as.integer(abund), abund_type = "point_intercept") %>%
  arrange(site, year, plot, species)
