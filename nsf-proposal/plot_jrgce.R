library(tidyverse)

jrgce_com <- read_rds("nsf-proposal/data/cfp_community.rds")$exp %>%
  filter(site == "jrgce") %>%
  select(year, plot, treat, species, guild, abund)

jrgce_npp <- read_csv("nsf-proposal/data/jrgce_biomass.csv", col_types = "iicd") %>%
  pivot_wider(names_from = grp, values_from = bio) %>%
  select(year = yr, plot = plt, anpp = ANPP, bnpp = BNPP, npp = NPP)
