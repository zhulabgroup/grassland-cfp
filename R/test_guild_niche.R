#' @export
test_guild_niche <- function(dat_guild_niche) {
  ls_res <- vector(mode = "list")
  for (grp in dat_guild_niche$group %>% unique()) {
    response <- dat_guild_niche %>%
      filter(!is.na(guild)) %>%
      filter(group == grp) %>%
      select(tmp, ppt)
    predict <- dat_guild_niche %>%
      filter(!is.na(guild)) %>%
      filter(group == grp) %>%
      select(guild)

    set.seed(1)
    ls_res[[grp]] <- vegan::adonis2(response ~ guild, data = predict)
  }

  return(ls_res)
}
