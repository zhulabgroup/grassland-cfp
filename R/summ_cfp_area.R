summ_cfp_geography <- function() {
  sf_cfp <- read_cfp(path_cfp = system.file("extdata", "cfp", package = "grassland"))

  bbox_cfp <- sf::st_bbox(sf_cfp)

  return(bbox_cfp)
}

summ_cfp_climate <- function(dat_clim, source = "chelsa") {
  sf_cfp <- read_cfp(path_cfp = system.file("extdata", "cfp", package = "grassland"))

  df_chelsa_cfp <- dat_clim[[source]] %>%
    terra::crop(sf_cfp) %>%
    terra::mask(sf_cfp) %>%
    as.data.frame() %>%
    summarise(
      t_min = min(tmp, na.rm = T),
      t_max = max(tmp, na.rm = T),
      p_min = min(ppt, na.rm = T),
      p_max = max(ppt, na.rm = T)
    )

  return(df_chelsa_cfp)
}


summ_cfp_climate_change <- function(indir = "alldata/intermediate/background/cfp_annual/") {
  sum_cfp_cc <- bind_rows(
    sum_cfp_cc_func(indir = indir, param = "tas"),
    sum_cfp_cc_func(indir = indir, param = "pr")
  )
  return(sum_cfp_cc)
}

sum_cfp_cc_func <- function(indir, param) {
  ras <- terra::rast(str_c(indir, param, "_trend.nc"))
  trend_ras <- ras[[1]]
  names(trend_ras) <- "trend"
  sig_ras <- ras[[3]]
  names(sig_ras) <- "p"
  trend_df <- trend_ras %>%
    terra::as.data.frame(xy = T) %>%
    cbind(sig_ras %>% terra::as.data.frame(xy = F)) %>%
    select(lon = x, lat = y, trend, p) %>%
    drop_na(trend) %>%
    summarise(
      mean = mean(trend, na.rm = T),
      median = median(trend, na.rm = T),
      se = sd(trend, na.rm = T) / sqrt(n()),
      lower = quantile(trend, 0.025, na.rm = T),
      upper = quantile(trend, 0.975, na.rm = T),
      sig_pos_percentage = sum(p <= 0.05 & trend > 0) / n() * 100,
      sig_neg_percentage = sum(p <= 0.05 & trend < 0) / n() * 100
    ) %>%
    mutate(param = param) %>%
    select(param, everything())
}
