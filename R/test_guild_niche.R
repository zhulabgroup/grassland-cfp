test_guild_niche <- function(dat_guild_niche) {
  ls_res <- vector(mode = "list")
  for (grp in dat_guild_niche$group %>% unique()) {
    response <- dat_guild_niche %>%
      filter(!is.na(guild)) %>%
      filter(group == "Origin") %>%
      select(tmp, ppt)
    predict <- dat_guild_niche %>%
      filter(!is.na(guild)) %>%
      filter(group == "Origin") %>%
      select(guild)
    ls_res[[grp]] <- vegan::adonis2(response ~ guild, data = predict)
  }

  return(ls_res)
}
