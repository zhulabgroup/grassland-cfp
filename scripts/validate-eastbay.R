# validate with Joan's functional group (guild) spreadsheet
eb_guild_sum_tbl1 <- .path$com_raw %>%
  paste0("Dudney/FunctionalGroups_EBParks.xlsx") %>%
  readxl::read_xlsx(range = readxl::cell_cols("A:E")) %>%
  rename(plot = plot.ID, guild = fungrp)

eb_guild_sum_tbl2 <- .path$com_raw %>%
  paste0("Dudney/EBRPD_2002thru2012_Dec2013_BRXX AVXX updated.csv") %>%
  read_csv(col_types = "cicidc") %>%
  rename(plot = plot.ID, species_code = species) %>%
  left_join(eb_spp_tbl, by = "species_code") %>%
  group_by(site, year, plot, guild) %>%
  summarize(abund = n())

inner_join(eb_guild_sum_tbl1, eb_guild_sum_tbl2, by = c("site", "year", "plot", "guild")) %>%
  ggplot(aes(abund.x, abund.y)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Species hit from Dudney", y = "Species hit from Zhu")
