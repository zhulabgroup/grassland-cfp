# niche data
niche_tbl <- read_rds(.path$sum_niche) %>%
  filter(occ_n > 100 | is.na(occ_n)) # species with many observations and dummy species

# JRGCE data
jrgce_tbl <- read_rds(.path$com_exp) %>%
  filter(site == "jrgce", year >= 1999) %>%
  inner_join(niche_tbl, by = "species") %>%
  group_by(site, year, plot, treat) %>%
  summarize(
    tmp_com_mean = sum(abund * tmp_occ_median) / sum(abund),
    ppt_com_mean = sum(abund * ppt_occ_median) / sum(abund)
  ) %>%
  mutate(treat_T = str_sub(treat, start = 1L, end = 1L)) %>%
  select(site, year, plot, treat_T, tmp_com_mean, ppt_com_mean) %>%
  pivot_longer(cols = tmp_com_mean:ppt_com_mean, names_to = "com_idx_name", values_to = "com_idx_value") %>%
  mutate(com_idx_name = factor(com_idx_name,
                               levels = c("tmp_com_mean", "ppt_com_mean"),
                               labels = c("CTI", "CPI")
  ))

# warming phrases: +80 W m?2 (years 2?5), to +100 W m?2 (years 6?12), to +250 W m?2 (years 13?17)
warm_tbl <- tribble(
  ~tag, ~name, ~start, ~end,
  1, "Phase I", -Inf, 2002,
  2, "Phase II", 2003, 2009,
  3, "Phase III", 2010, Inf # end in 2014, but set to Inf to fill space
)

# plot
df_exp_sum<-jrgce_tbl %>% 
  mutate(phase = case_when(year<=2002~"Phase I",
                           year>=2010~"Phase III",
                           TRUE ~ "Phase II")) %>% 
  group_by(site, phase,year, treat_T,com_idx_name) %>% 
  summarise(m=median(com_idx_value),
            se=sd(com_idx_value)/sqrt(n())) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c("site", "phase", "year", "treat_T"), 
              names_from = com_idx_name, 
              values_from = c("m", "se"))

df_exp_shift<-jrgce_tbl %>% 
  mutate(phase = case_when(year<=2002~"Phase I",
                           year>=2010~"Phase III",
                           TRUE ~ "Phase II")) %>% 
  group_by(site, phase,year, treat_T,com_idx_name) %>% 
  summarise(m=median(com_idx_value)) %>% 
  ungroup() %>% 
  mutate(group_metric=paste0(com_idx_name, treat_T)) %>% 
  select(-com_idx_name, -treat_T) %>% 
  pivot_wider(id_cols = c("site", "phase", "year"), 
              names_from = group_metric, 
              values_from = m)

ggplot() +
  geom_point(data=df_exp_sum,
             aes(x=m_CTI, y=m_CPI,
                 ))+
  geom_segment(data=df_exp_shift,
               aes(x=CTI_, xend=CTIT, y=CPI_,yend=CPIT,
                   group=phase, col=phase),
               arrow = arrow(length = unit(0.2,"cm")),
               linewidth=1)+
  facet_wrap(.~phase, nrow=1)+
  xlab("Community Temperature Index (CTI, °C)")+
  ylab("Community Precipitation Index (CPI, mm)")+
  guides(col="none")



#########

# import niche and observational data, calculate CTI and CPI
niche_tbl <- read_rds(.path$sum_niche) %>%
  filter(occ_n > 100 | is.na(occ_n)) # species with many observations and dummy species

obs_tbl <- read_rds(.path$com_obs) %>%
  inner_join(niche_tbl, by = "species") %>%
  group_by(site, year, plot) %>%
  summarize(
    tmp_com_mean = sum(abund * tmp_occ_median) / sum(abund),
    ppt_com_mean = sum(abund * ppt_occ_median) / sum(abund)
  )

# setup site labels
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

# reshape data
obs_idx_tbl <- obs_tbl %>%
  dplyr::select(site, year, plot, tmp_com_mean, ppt_com_mean) %>%
  pivot_longer(cols = tmp_com_mean:ppt_com_mean, names_to = "com_idx_name", values_to = "com_idx_value") %>%
  mutate(com_idx_name = factor(com_idx_name,
                               levels = c("tmp_com_mean", "ppt_com_mean"),
                               labels = c("CTI", "CPI")
  ))



df_obs_sum<-obs_idx_tbl %>% 
  group_by(site, year, com_idx_name) %>% 
  summarise(m=median(com_idx_value),
            se=sd(com_idx_value)/sqrt(n())) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c("site", "year"), 
              names_from = com_idx_name, 
              values_from = c("m", "se"))
df_obs_shift<-obs_idx_tbl %>% 
  group_by(site,com_idx_name) %>% 
  do(broom::augment( lm(com_idx_value ~ year, data = .))) %>% 
  select(site, com_idx_name, year, fitted=.fitted) %>% 
  filter(year==min(year)|year==max(year)) %>% 
  mutate(year=case_when(year==min(year)~"start",
                        year==max(year)~"end")) %>% 
  ungroup() %>% 
  distinct() %>% 
  pivot_wider(id_cols = c("site", "year"), 
              names_from = com_idx_name, 
              values_from = fitted) %>% 
  pivot_wider(id_cols = site, 
              names_from = year, 
              values_from = c("CTI", "CPI"))

ggplot()+
  geom_point(data=df_obs_sum,
             aes(x=m_CTI, y=m_CPI,col=year),
             alpha=0.75)+
  geom_segment(data=df_obs_shift,
             arrow = arrow(length = unit(0.2,"cm")),
             linewidth=1
             )+
  scale_color_viridis_c()+
  facet_wrap(.~site, labeller=site_vec %>% as_labeller())+
  xlab("Community Temperature Index (CTI, °C)")+
  ylab("Community Precipitation Index (CPI, mm)")


df_all_shift<-bind_rows(df_exp_shift %>% 
            filter(phase=="Phase III") %>% 
            mutate(site=paste(site, year, sep="_"))%>% 
            select(site,CTI0=CTI_, CTI1=CTIT, CPI0=CPI_, CPI1=CPIT) %>% 
            mutate(group="experiment"),
          df_obs_shift %>% 
            select(site, CTI0=CTI_start, CTI1=CTI_end, CPI0=CPI_start, CPI1=CPI_end) %>% 
            mutate(group="observation")
            )
ggplot(df_all_shift)+
  geom_segment(aes(x=CTI0, xend=CTI1, y=CPI0, yend=CPI1,
                   group=site, col=group),
               arrow = arrow(length = unit(0.2,"cm")),
               linewidth=1, 
               alpha=0.8)+
  xlab("Community Temperature Index (CTI, °C)")+
  ylab("Community Precipitation Index (CPI, mm)")

