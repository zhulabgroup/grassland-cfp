# pin count table
pin_tbl <- .path$com_raw %>%
  str_c("JRGCE/Pin count.csv") %>%
  read_csv(col_types = "iiccicc") %>%
  select(year = YEAR, plot = ID, species = SPECIES, abund = TOTAL) %>%
  mutate(species = if_else(
    str_starts(species, "Acmispon americanus"),
    "Acmispon americanus var.americanus",
    species
  )) %>%
  mutate(species = if_else(
    str_starts(species, "Crepis"),
    "Crepis vesicaria ssp. taraxacifolia",
    species
  )) %>%
  filter(!is.na(abund))

# species table
spp_tbl <- .path$com_raw %>%
  str_c("JRGCE/Species.csv") %>%
  read_csv(col_types = "c") %>%
  mutate(guild = str_c(case_when(
    NATIVE_CODE == "I" ~ "E", # Invasive -> Exotic
    NATIVE_CODE == "N" ~ "N", # Native
    NATIVE_CODE == "U" ~ "U", # Unknown
    is.na(NATIVE_CODE) ~ "U"
  ), FXNGROUP_CODE)) %>%
  select(species = `Species or unit name`, guild)

# treatment table
trt_tbl_raw <- .path$com_raw %>%
  str_c("JRGCE/Treatment.csv") %>%
  read_csv(col_types = "iiiiciiiiiiiiic")

trt_tbl <- trt_tbl_raw %>%
  pull(id) %>%
  expand_grid(year = 1998:2014, plot = .) %>% # full year-treatment tibble
  full_join(
    trt_tbl_raw %>%
      select(plot = id, starts_with("Heat_"), Precip, CO2, Nitrogen),
    by = "plot"
  ) %>%
  mutate( # recode treatment
    tmp = case_when(
      year >= 1998 & year <= 2003 ~ as.logical(Heat_1998_2003 - 1),
      year >= 2004 & year <= 2006 ~ as.logical(Heat_2004_2006 - 1),
      year >= 2007 & year <= 2014 ~ as.logical(Heat_2007_2014 - 1)
    ),
    ppt = as.logical(Precip - 1),
    co2 = as.logical(CO2 - 1),
    ntg = as.logical(Nitrogen - 1)
  ) %>%
  mutate(
    tmp = if_else(year == 1998, FALSE, tmp),
    ppt = if_else(year == 1998, FALSE, ppt),
    co2 = if_else(year == 1998, FALSE, co2),
    ntg = if_else(year == 1998, FALSE, ntg)
  ) %>%
  mutate(
    treat = str_c(
      ifelse(tmp, "T", "_"),
      ifelse(ppt, "P", "_"),
      ifelse(co2, "C", "_"),
      ifelse(ntg, "N", "_")
    )
  ) %>%
  select(year, plot, treat) %>%
  arrange(year, plot)

# join all tables
jrgce_tbl <- pin_tbl %>%
  left_join(spp_tbl, by = "species") %>%
  left_join(trt_tbl, by = c("year", "plot")) %>%
  mutate(site = "jrgce", abund_type = "point_intercept") %>%
  select(site, year, plot, treat, species, guild, abund, abund_type) %>%
  arrange(year, plot, species)
