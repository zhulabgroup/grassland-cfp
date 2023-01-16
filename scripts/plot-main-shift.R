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
ggplot(jrgce_tbl %>% 
         mutate(phase = case_when(year<=2002~"Phase I",
                                  year>=2010~"Phase III",
                                  TRUE ~ "Phase II")) %>% 
         group_by(site, phase,year, treat_T,com_idx_name) %>% 
         summarise(m=median(com_idx_value),
                   se=sd(com_idx_value)/sqrt(n())) %>% 
         ungroup() %>% 
         mutate(group_metric=paste0(com_idx_name, treat_T)) %>% 
         select(-com_idx_name, -treat_T) %>% 
         pivot_wider(id_cols = c("site", "phase", "year"), 
                     names_from = group_metric, 
                     values_from = c("m", "se"))) +
  geom_point(aes(x=m_CTI_, y=m_CPI_,
                   # group=phase, col=phase
                 ))+
  # geom_errorbar(aes(x=m_CTI_,ymin=m_CPI_-se_CPI_, ymax=m_CPI_+se_CPI_,
  #                    group=phase, col=phase), alpha=0.5)+
  # geom_errorbarh(aes(y=m_CPI_,xmin=m_CTI_-se_CTI_, xmax=m_CTI_+se_CTI_,
  #                group=phase, col=phase), alpha=0.5)+
  geom_point(aes(x=m_CTIT, y=m_CPIT,
                 # group=phase, col=phase
                 ))+
  # geom_errorbar(aes(x=m_CTIT,ymin=m_CPIT-se_CPIT, ymax=m_CPIT+se_CPIT,
  #                   group=phase, col=phase), alpha=0.5)+
  # geom_errorbarh(aes(y=m_CPIT,xmin=m_CTIT-se_CTIT, xmax=m_CTIT+se_CTIT,
  #                    group=phase, col=phase), alpha=0.5)+
  geom_segment(aes(x=m_CTI_, xend=m_CTIT, y=m_CPI_,yend=m_CPIT,
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




ggplot()+
  geom_point(data=obs_idx_tbl %>% 
               group_by(site, year, com_idx_name) %>% 
               summarise(m=median(com_idx_value),
                         se=sd(com_idx_value)/sqrt(n())) %>% 
               ungroup() %>% 
               pivot_wider(id_cols = c("site", "year"), 
                           names_from = com_idx_name, 
                           values_from = c("m", "se")),
             aes(x=m_CTI, y=m_CPI,col=year),
             alpha=0.75)+
  geom_segment(data=obs_idx_tbl %>% 
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
                             values_from = c("CTI", "CPI")),
             aes(x=CTI_start, xend=CTI_end, y=CPI_start, yend=CPI_end),
             arrow = arrow(length = unit(0.2,"cm")),
             linewidth=1
             )+
  scale_color_viridis_c()+
  facet_wrap(.~site, labeller=site_vec %>% as_labeller())+
  xlab("Community Temperature Index (CTI, °C)")+
  ylab("Community Precipitation Index (CPI, mm)")
