site_vec <- c( 
  jrgce = "Jasper Ridge Global Change Experiment",
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
  distinct(year) %>% 
  nrow()

data_avail<-bind_rows(read_rds(.path$com_exp) %>%
  distinct(site, plot, year),
  read_rds(.path$com_obs) %>% 
    distinct(site, plot, year)
  )%>% 
  mutate(sitename=site_vec[site]) %>% 
  filter(!is.na(sitename)) %>% 
  group_by(sitename, year) %>% 
  summarise(count=n() ) %>% 
  ungroup() %>% 
  arrange(count) %>% 
  mutate(sitename=factor(sitename, levels=site_vec))

ggplot(data_avail)+
  geom_segment(data=data_avail %>% 
                 group_by(sitename) %>% 
                 summarise(min=min(year), max=max(year)),
               aes(x=min, xend=max, y=sitename, yend=sitename), linewidth=3, col="dark green", alpha=0.4)+
  geom_point(data=data_avail, aes(x=year,  y=sitename), pch=19, cex=2)+
  theme_classic()+
  # scale_size("sample size",range=c(2,4), breaks=c(10, 50, 100))+
  xlab("")+
  ylab("")+
  scale_y_discrete(limits=rev)
