plot_cfp_cc<-function(param) {
  trend_ras<-raster::stack(str_c(.path$cli_chelsa_cfp_annual, param, "_trend.nc"))[[1]]
  trend_df<-trend_ras %>% raster::as.data.frame(xy=T) %>% 
    select(lon=x, lat=y, trend=X1) %>% 
    mutate(param=param)
  
  if (param == "tas") {
    title <- "Trend in mean temperature"
    unit<-expression("°C/year")
    highcol<-"red"
    lowcol<-"blue"
  }
  if(param == "vpd") {
    title <- "Trend in maximum vapor pressure deficit"
    unit <- expression("Pa")
    highcol<-"red"
    lowcol<-"blue"
  }
  if(param == "pr") {
    title <- "Trend in total precipitation"
    unit <- expression("kg m"^"-2"~"year"^"-1"~"/year")
    highcol<-"blue"
    lowcol<-"red"
  }
  
  p<-ggplot(trend_df)+
    geom_tile(aes(x=lon, y=lat, fill=trend))+
    scale_fill_gradient2(low = lowcol, mid = "white", high = highcol, midpoint=0, na.value="white")+
    theme_void()+
    labs(fill=unit)+
    ggtitle(title)+
    coord_equal()+
    theme(legend.position="bottom")
  
  return(p)
}

cfp_clim_gg <-
  plot_cfp_cc(param="tas") +
  plot_cfp_cc(param="pr") +
  plot_cfp_cc(param="vpd") +
  plot_layout(ncol = 3, nrow = 1)
  
