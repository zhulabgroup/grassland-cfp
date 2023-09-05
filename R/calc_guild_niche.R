calc_guild_niche <- function(dat_community, dat_niche) {
  df <- bind_rows(
    dat_community$obs %>%
      select(species, guild),
    dat_community$exp %>%
      select(species, guild)
  ) %>%
    distinct(species, guild) %>%
    filter(!is.na(guild)) %>% # why is there NA?
    mutate(
      native = str_sub(guild, 1, 1),
      annual = str_sub(guild, 2, 2),
      grass = str_sub(guild, 3, 3)
    ) %>%
    select(-guild) %>%
    mutate(native = factor(native,
      levels = c("N", "E", "D"),
      labels = c("Native", "Non-native", "DUMMY")
    )) %>%
    mutate(annual = factor(annual,
      levels = c("P", "A", "U"),
      labels = c("Perennial", "Annual", "DUMMY")
    )) %>%
    mutate(grass = factor(grass,
      levels = c("F", "S", "G", "R", "T", "U", "M"),
      labels = c("Forb", "Shrub", "Grass", "Rush", "Tree", "Unknown", "DUMMY")
    )) %>%
    gather(key = "group", value = "guild", -species) %>%
    mutate(group = factor(group,
      levels = c("native", "annual", "grass"),
      labels = c("Origin", "Life history", "Functional group")
    )) %>%
    mutate(guild = factor(guild,
      levels = c(
        "Native", "Non-native",
        "Annual", "Perennial",
        "Grass", "Forb"
      )
    )) %>%
    inner_join(
      dat_niche %>%
        filter(occ_n > 100 | is.na(occ_n)) %>%
        select(species,
          tmp = tmp_occ_median,
          ppt = ppt_occ_median
        ),
      by = "species"
    )

  return(df)
}
