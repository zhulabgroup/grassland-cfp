niche_tbl <- read_rds(.path$sum_niche) %>%
  filter(occ_n > 100 | is.na(occ_n)) %>% # species with many observations and dummy species
  select(species, tmp_occ_median, ppt_occ_median)

site_vec <- c(
  angelo = "Angelo Coast",
  carrizo = "Carrizo Plain",
  elkhorn = "Elkhorn Slough",
  jasper = "Jasper Ridge Serpentine",
  mclann = "McLaughlin Annual",
  mclserp = "McLaughlin Serpentine",
  morganterritory = "Morgan Territory",
  pleasantonridge = "Pleasanton Ridge",
  sunol = "Sunol",
  swanton = "Swanton Ranch",
  ucsc = "UC Santa Cruz",
  vascocaves = "Vasco Caves"
)

read_rds(.path$com_obs) %>%
  filter(site == "morganterritory") %>%
  slice(126, 127)

obs_gainloss_tbl_list <- vector(mode = "list")
for (siteoi in names(site_vec)) {
  df_trend<-read_rds(.path$com_obs) %>%
    filter(site == siteoi) %>%
    filter(guild!="DUMMY") %>% 
    group_by(year, plot, species) %>%
    summarise(abund = sum(abund)) %>% # doing this because of duplicated records. not the ideal solution.
    ungroup() %>%
    left_join(group_by(.,year, plot) %>% 
                summarise(total=sum(abund)) %>% 
                ungroup(),
              by=c("year", "plot")) %>% 
    mutate(rel_abun=abund/total) %>%
    select(-abund, -total) %>% 
    spread(key = "species", value = "rel_abun") %>%
    mutate_if(is.numeric, ~ replace_na(., 0)) %>%
    gather(key = "species", value = "rel_abun", -year, -plot) %>%
    group_by(species) %>%
    nest() %>%
    mutate(
      map(data, ~ lm(rel_abun ~ year, data = .)) %>%
        map_df(~ broom::tidy(.) %>%
          filter(term == "year") %>%
          select(estimate, p.value)),
    ) %>%
    unnest(cols = data) %>%
    distinct(species, estimate, p.value) %>%
    mutate(change = case_when(
      (estimate > 0 & p.value <= 0.05) ~ "gain",
      (estimate < 0 & p.value <= 0.05) ~ "loss",
      TRUE ~ "no clear change"
    )) 
  
  df_dominance<-read_rds(.path$com_obs) %>%
    filter(site == siteoi) %>%
    filter(guild!="DUMMY") %>% 
    group_by(year, plot, species) %>%
    summarise(abund = sum(abund)) %>% # doing this because of duplicated records. not the ideal solution.
    ungroup() %>% 
    group_by(species) %>% 
    summarise(abund=sum(abund)) %>% 
    ungroup() %>% 
    mutate(dominance=abund/sum(abund)) %>% 
    select(-abund)
    
  df_complete<-read_rds(.path$com_obs) %>%
    filter(site == siteoi) %>%
    filter(guild!="DUMMY") %>% 
    mutate(period =case_when (year %in% (year %>% unique() %>% sort() %>% head(5))~"early",
                              year %in% (year %>% unique() %>% sort()  %>% tail(5))~"late")) %>% 
    filter(!is.na(period)) %>% 
    group_by(period, species) %>%
    summarise(abund = sum(abund)) %>% # doing this because of duplicated records. not the ideal solution.
    ungroup() %>%
    spread(key = "species", value = "abund") %>%
    mutate_if(is.numeric, ~ replace_na(., 0)) %>%
    gather(key = "species", value = "abund", -period) %>%
    spread(key="period", value="abund") %>% 
    mutate(complete=case_when((early==0 & late!=0 )~"recruited",
                              (early!=0 & late==0 )~"extirpated")) %>% 
    select(species, complete)
  
  obs_gainloss_tbl_list[[siteoi]] <- df_trend %>% 
    left_join(df_dominance, by="species") %>% 
    left_join(df_complete, by = "species") %>% 
    left_join(niche_tbl, by = "species") %>%
    mutate(site = siteoi)
}
obs_gainloss_tbl <- bind_rows(obs_gainloss_tbl_list)

gainloss_gg <-
  ggplot() +
  geom_point(
    data = niche_tbl,
    aes(
      x = tmp_occ_median,
      y = ppt_occ_median
    ), color = "gray", alpha = 0
  ) +
  geom_point(
    data = obs_gainloss_tbl,
    aes(
      x = tmp_occ_median,
      y = ppt_occ_median,
      color = change,
      size = dominance
    ), alpha = 1,pch=21, fill=NA
  ) +
  geom_point(
    data = obs_gainloss_tbl %>% filter(change=="gain"|change=="loss") %>% filter(!is.na(complete)),
    aes(
      x = tmp_occ_median,
      y = ppt_occ_median,
      size = dominance ,
      fill = complete,
    ), alpha = 0.75,pch=21
  ) +
  scale_color_manual(values = c(gain = "dark green", `no clear change` = "gray", loss = "dark orange")) +
  scale_fill_manual(values = c(recruited = "dark green",  extirpated = "dark orange")) +
  labs(x = "Mean annual temperature (°C)", y = "Mean annual precipitation (mm)") +
  guides(fill = "none") +
  facet_wrap(. ~ site,
    labeller = site_vec %>% as_labeller(),
    nrow = 3
  )

# save figure file
if (.fig_save) {
  ggsave(
    plot = gainloss_gg,
    filename = str_c(.path$out_fig, "fig-supp-gainloss-obs.png"),
    width = 12,
    height = 10
  )
}
