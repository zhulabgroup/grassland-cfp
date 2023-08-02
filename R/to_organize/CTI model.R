# niche data
niche_tbl <- read_rds(.path$sum_niche) %>%
  filter(occ_n > 100 | is.na(occ_n)) # species with many observations and dummy species


df_comm <- read_rds(.path$com_exp) %>%
  filter(site == "jrgce", year >= 1999) %>%
  mutate(treat_T = str_sub(treat, start = 1L, end = 1L)) %>%
  filter(year == 2013) %>%
  filter(treat == "T___" | treat == "____") %>%
  # distinct(plot, treat_T)
  # filter(plot %in% c(12, 107)) %>%
  select(plot, treat_T, species, abund) %>%
  inner_join(niche_tbl %>%
    select(species,
      tmp = tmp_occ_median,
      ppt = ppt_occ_median
    ), by = "species")


df_comm %>%
  ggplot() +
  geom_point(aes(x = ppt, y = abund, col = treat_T, group = plot)) +
  geom_segment(aes(x = ppt, xend = ppt, y = 0, yend = abund, col = treat_T, group = plot)) +
  facet_wrap(. ~ treat_T * plot) +
  scale_color_manual(values = c("black", "red"))

df_comm %>%
  ggplot() +
  geom_jitter(aes(x = ppt, y = abund, col = treat_T, group = plot), width = 0.1, height = 0) +
  # tidyquant::geom_ma(ma_fun = SMA, n = 30)+
  # geom_smooth(se=F, method = "ma")+
  facet_wrap(. ~ treat_T, ncol = 1) +
  scale_color_manual(values = c("black", "red"))

# sample

# read GBIF-CHELSA data
gbif_chelsa_sf <- read_rds(.path$geo_clim) %>%
  dplyr::select(geometry, key, species, tmp = chelsa_tmp, ppt = chelsa_ppt) %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")

get_trait_sample <- function(df_comm) {
  ls_df_trait <- vector(mode = "list")
  for (i in 1:nrow(df_comm)) {
    sp <- df_comm$species[i] %>%
      str_remove(" DUMMY")
    abund <- df_comm$abund[i]
    ls_df_trait[[i]] <- gbif_chelsa_sf %>%
      filter(str_detect(species, sp)) %>%
      sample_n(abund, replace = T) %>%
      data.frame() %>%
      select(species, tmp, ppt)
  }
  df_trait <- bind_rows(ls_df_trait)

  return(df_trait)
}

df_ind <- df_comm %>%
  select(-tmp, -ppt) %>%
  group_by(plot, treat_T) %>%
  nest(data = c(species, abund)) %>%
  mutate(trait = map(data, get_trait_sample)) %>%
  select(-data) %>%
  unnest(cols = c(trait))


df_ind %>%
  ggplot(aes(x = ppt, col = treat_T, fill = treat_T, group = plot)) +
  geom_histogram() +
  facet_wrap(. ~ treat_T * plot) +
  scale_color_manual(values = c("black", "red")) +
  scale_fill_manual(values = c("black", "red"))

df_ind %>%
  ggplot(aes(x = ppt, col = treat_T, group = plot)) +
  geom_density(alpha = 0.5) +
  facet_wrap(. ~ treat_T, ncol = 1) +
  scale_color_manual(values = c("black", "red"))

df_ind %>%
  group_by(treat_T) %>%
  summarise(
    tmp_median = median(tmp),
    tmp_mean = mean(tmp),
    ppt_median = median(ppt),
    ppt_mean = mean(ppt)
  ) %>%
  arrange(desc(treat_T))
