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
    `T median (°C)` = tmp_occ_median, `T std dev (°C)` = tmp_occ_sd,
    `P median (mm)` = ppt_occ_median, `P std dev (mm)` = ppt_occ_sd,
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

# obs data
obs_tbl <- read_rds(.path$com_obs) %>%
  inner_join(niche_tbl, by = "species") %>%
  group_by(site, year, plot) %>%
  summarize(
    tmp_com_mean = sum(abund * tmp_occ_median) / sum(abund),
    ppt_com_mean = sum(abund * ppt_occ_median) / sum(abund)
  )

obs_tbl %>%
  group_by(site) %>%
  summarize(year_max = max(year), year_min = min(year)) %>%
  mutate(year_rng = year_max - year_min + 1)

obs_tbl %>%
  group_by(site) %>%
  nest() %>%
  mutate(
    cti_lm = map(
      data,
      ~ lm(tmp_com_mean ~ year, data = .) %>%
        broom::tidy() %>%
        filter(term == "year") %>%
        select(cti_estimate = estimate, cti_std_err = std.error, cti_p_val = p.value) %>%
        # mutate(across(everything(), signif, 4)) %>%
        mutate(cti_sig = gtools::stars.pval(cti_p_val))
    ),
    cpi_lm = map(
      data,
      ~ lm(ppt_com_mean ~ year, data = .) %>%
        broom::tidy() %>%
        filter(term == "year") %>%
        select(cpi_estimate = estimate, cpi_std_err = std.error, cpi_p_val = p.value) %>%
        # mutate(across(everything(), signif, 4)) %>%
        mutate(cpi_sig = gtools::stars.pval(cpi_p_val))
    )
  ) %>%
  unnest(cols = c(cti_lm, cpi_lm))
