# get JRGCE temperature treatments in deg C
jrgce_avgt_tbl <- .path$com_raw %>%
  str_c("JRGCE/Environment mid H1 H2-1_V2.CSV") %>%
  read_csv(col_types = "icfcd") %>%
  rename_with(tolower) %>%
  filter(variable == "AVGT308_126") %>%
  pivot_wider(names_from = treatment, values_from = value) %>%
  rename_with(tolower) %>%
  mutate(tmp_diff = t_elev - amb)
