site_tbl <- clim_tbl %>%
  select(abbr, year, tmp, ppt) %>%
  # right_join(read_rds(.path$com_obs) %>%
  #              distinct(abbr = site, year),
  #            by = c("abbr", "year")) %>%
  pivot_longer(tmp:ppt, names_to = "clim_var", values_to = "clim_val") %>%
  mutate(clim_var = factor(clim_var,
    levels = c("tmp", "ppt")
  )) %>%
  group_by(abbr, clim_var) %>%
  nest() %>%
  mutate(
    map(data, ~ lm(clim_val ~ year, data = .)) %>%
      map_df(~ broom::tidy(.) %>%
        filter(term == "year") %>%
        select(estimate, std.error, p.value)) # ,
  ) %>%
  select(-data) %>%
  ungroup() %>%
  mutate(sig = gtools::stars.pval(p.value)) %>%
  mutate(sig = ifelse(sig != " ", sig, "ns")) %>%
  mutate(name = site_vec[abbr])

# summary statistics
obs_tmp_tbl <- site_tbl %>%
  filter(clim_var == "tmp") %>%
  mutate(across(estimate:`std.error`, signif, 3)) %>%
  select(Site = name, Estimate = estimate, `Standard error` = std.error, `p-value` = p.value, Significance = sig)

obs_ppt_tbl <- site_tbl %>%
  filter(clim_var == "ppt") %>%
  mutate(across(estimate:`std.error`, signif, 3)) %>%
  select(Site = name, Estimate = estimate, `Standard error` = std.error, `p-value` = p.value, Significance = sig)

df_clim_site <- bind_rows(
  obs_tmp_tbl %>%
    select(site = Site, rate = Estimate) %>%
    mutate(var = "T"),
  obs_ppt_tbl %>%
    select(site = Site, rate = Estimate) %>%
    mutate(var = "P")
) %>%
  # left_join(data_avail_tbl %>%
  #             mutate(site = site_vec[site]),
  #           by = "site") %>%
  # mutate(change = rate * nyear) %>%
  # mutate(change = rate * length(1980:2019)) %>%
  mutate(var = str_c("r_", var)) %>%
  select(site, var, rate)

df_clim_site %>%
  spread(key = "var", value = "rate") %>%
  ggplot(aes(x = r_P, y = r_T)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  coord_flip()

df_comm <- obs_sum_tbl %>%
  select(site, T = cti_estimate, P = cpi_estimate) %>%
  mutate(site = site_vec[site]) %>%
  gather(key = "var", value = "rate", -site) %>%
  left_join(
    data_avail_tbl %>%
      mutate(site = site_vec[site]),
    by = "site"
  ) %>%
  # mutate(change = rate * nyear) %>%
  select(plot = site, var, rate) %>%
  mutate(group = "observation") %>%
  bind_rows(
    bind_rows(
      exp_cti_tbl %>%
        select(plot = Year, rate = Difference) %>%
        mutate(var = "T"),
      exp_cpi_tbl %>%
        select(plot = Year, rate = Difference) %>%
        mutate(var = "P")
    ) %>%
      mutate(group = "experiment") %>%
      mutate(plot = str_c("JRGCE_", plot))
  ) %>%
  mutate(var = str_c("r_", var))

df_comm %>%
  spread(key = "var", value = "rate") %>%
  ggplot(aes(x = r_P, y = r_T, col = group, group = group)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  coord_flip()


df_clim_cfp <- bind_cols(
  T = terra::rast(str_c(.path$cli_chelsa_cfp_annual, "tas_trend.nc"))[[1]] %>% as.data.frame() %>% unlist(),
  P = terra::rast(str_c(.path$cli_chelsa_cfp_annual, "pr_trend.nc"))[[1]] %>% as.data.frame() %>% unlist()
) %>%
  mutate(pixel = row_number()) %>%
  sample_n(100) %>%
  gather(key = "var", value = "rate", -pixel) %>%
  # mutate(change = rate * length(1980:2019)) %>%
  select(pixel, var, rate) %>%
  mutate(var = str_c("r_", var))


df_clim_cfp %>%
  spread(key = "var", value = "rate") %>%
  ggplot(aes(x = r_P, y = r_T)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  coord_flip()

# lm(delta_T~delta_P,
#    data = df_clim_cfp %>%
#      spread(key = "var", value = "change")) %>%
#   summary()

bind_rows(
  df_comm %>% filter(group == "observation") %>% mutate(dimension = "community"),
  # df_clim_cfp %>%  mutate(dimension = "climate-cfp"),
  df_clim_site %>% mutate(dimension = "climate")
) %>%
  spread(key = "var", value = "rate") %>%
  ggplot(aes(x = r_P, y = r_T, col = dimension, group = dimension)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  coord_flip()


df_comm %>%
  filter(group == "observation") %>%
  select(site = plot, var, rate) %>%
  mutate(dimension = "community") %>%
  bind_rows(df_clim_site %>%
    mutate(dimension = "climate")) %>%
  spread(key = "dimension", value = "rate") %>%
  # filter(!str_detect(site, "Serpentine")) %>%
  ggplot(aes(x = climate, y = community)) +
  geom_point() +
  geom_smooth(method = MASS::rlm) +
  facet_wrap(. ~ var, scales = "free")


df_comm %>%
  filter(group == "observation") %>%
  select(site = plot, var, rate) %>%
  mutate(dimension = "community") %>%
  bind_rows(df_clim_site %>%
    mutate(dimension = "climate")) %>%
  spread(key = "dimension", value = "rate") %>%
  View()

ggplot() +
  geom_segment(aes(x = 0, xend = 0.00531, y = 0, yend = -15.1), col = "orange") +
  geom_segment(aes(x = 0, xend = 0.0522, y = 0, yend = -7.40), col = "dark green")

ggplot() +
  geom_segment(aes(x = 0, xend = 0.0183, y = 0, yend = -4.16), col = "orange") +
  geom_segment(aes(x = 0, xend = 0.0255, y = 0, yend = -3.24), col = "dark green")

ggplot() +
  geom_segment(aes(x = 0, xend = 0.02780, y = 0, yend = -0.32600), col = "orange") +
  geom_segment(aes(x = 0, xend = 0.0203552091, y = 0, yend = -3.5249894219), col = "dark green")

# test if it's due to correlation in niche
data <- niche_tbl
