# summary tables to report statistics
# niche data
niche_tbl <- read_rds(.path$sum_niche) %>%
  filter(occ_n > 100 | is.na(occ_n)) # species with many observations and dummy species

niche_tbl %>%
  filter(!is.na(occ_n)) %>%
  mutate(
    occ_n = occ_n %>% format(big.mark = ","),
    across(starts_with(c("tmp", "ppt")), round, 3)
  ) %>%
  select(
    Species = species, n = occ_n,
    `T median (°C)` = tmp_occ_mean, `T std dev (°C)` = tmp_occ_sd,
    `P median (mm)` = ppt_occ_mean, `P std dev (mm)` = ppt_occ_sd,
  ) %>%
  knitr::kable(caption = "Species climate niche summary")

# exp data
exp_tbl <- read_rds(.path$com_exp) %>%
  filter(site == "jrgce", year >= 1999) %>%
  inner_join(niche_tbl, by = "species") %>%
  group_by(site, year, plot, treat) %>%
  summarize(
    tmp_com_mean = sum(abund * tmp_occ_median) / sum(abund),
    ppt_com_mean = sum(abund * ppt_occ_median) / sum(abund)
  ) %>%
  ungroup() %>%
  mutate(treat_T = str_sub(treat, start = 1L, end = 1L)) %>%
  select(site, year, plot, treat_T, tmp_com_mean, ppt_com_mean)

# report CTI
exp_tbl %>%
  group_by(year, treat_T) %>%
  summarize(cwm = mean(tmp_com_mean)) %>%
  ungroup() %>%
  pivot_wider(names_from = treat_T, values_from = cwm) %>%
  mutate(diff = `T` - `_`) %>%
  full_join(
    ggpubr::compare_means(
      formula = tmp_com_mean ~ treat_T,
      data = exp_tbl,
      method = "wilcox.test",
      group.by = "year"
    ),
    by = "year"
  ) %>%
  select(
    Year = year,
    Ambient = `_`, Warming = `T`, Diff = diff,
    p = p.format, Sig = p.signif
  ) %>%
  mutate(across(Ambient:Diff, signif, 4)) %>%
  knitr::kable()

# report CPI
exp_tbl %>%
  group_by(year, treat_T) %>%
  summarize(cwm = mean(ppt_com_mean)) %>%
  ungroup() %>%
  pivot_wider(names_from = treat_T, values_from = cwm) %>%
  mutate(diff = `T` - `_`) %>%
  full_join(
    ggpubr::compare_means(
      formula = ppt_com_mean ~ treat_T,
      data = exp_tbl,
      method = "wilcox.test",
      group.by = "year"
    ),
    by = "year"
  ) %>%
  select(
    Year = year,
    Ambient = `_`, Warming = `T`, Diff = diff,
    p = p.format, Sig = p.signif
  ) %>%
  mutate(across(Ambient:Diff, signif, 4)) %>%
  knitr::kable()
