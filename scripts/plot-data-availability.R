data_avail<-read_rds(.path$com_exp) %>%
  distinct(site, plot, year) %>% 
  group_by(site, year) %>% 
  summarise(count=n() ) %>% 
  ungroup() %>% 
  arrange(count)

ggplot(data_avail)+
  geom_segment(data=data_avail %>% 
                 group_by(site) %>% 
                 summarise(min=min(year), max=max(year)),
               aes(x=min, xend=max, y=site, yend=site), linewidth=3, col="dark green", alpha=0.4)+
  geom_point(data=data_avail, aes(x=year,  y=site, size=count), pch=18)+
  theme_classic()+
  scale_size("sample size",range=c(2,4), breaks=c(10, 50, 100))+
  xlab("")+
  ylab("")
