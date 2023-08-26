calc_climate_change_site <- function(dat_clim_site) {
  df_cc_site <- dat_clim_site %>%
    select(abbr, name, year, tmp, ppt) %>%
    pivot_longer(tmp:ppt, names_to = "clim_var", values_to = "clim_val") %>%
    mutate(clim_var = factor(clim_var,
      levels = c("tmp", "ppt")
    )) %>%
    group_by(abbr, name, clim_var) %>%
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
    mutate(sig = ifelse(sig != " ", sig, "ns"))

  return(df_cc_site)
}

calc_climate_annual_site <- function(indir = "alldata/input/climate/monthly/",
                                     v_param = c("tas", "pr", "vpd"),
                                     outdir = "alldata/intermediate/background/") {
  ## climate change maps at observational sites CFP
  sites <- read_site_info()

  # Extract monthly data at sites by year and get annual summaries
  cl <- makeCluster(length(1980:2019))
  registerDoSNOW(cl)

  val_list_allparam <- vector(mode = "list")
  for (param in v_param) {
    val_list <- foreach(
      year = 1980:2019,
      .packages = c("tidyverse", "sf", "raster", "matrixStats")
    ) %dopar% {
      files <- list.files(str_c(indir, param), pattern = year %>% as.character(), full.names = T)
      sta <- raster::stack(files)
      monthly_dat <- raster::extract(sta, sites)

      if (param == "tas") {
        monthly_dat_tr <- monthly_dat / 10 - 273.15
        val_year <- data.frame(id = 1:nrow(sites), year = year, value = monthly_dat_tr %>% rowMeans())
      }
      if (param == "pr") {
        monthly_dat_tr <- monthly_dat / 100
        val_year <- data.frame(id = 1:nrow(sites), year = year, value = monthly_dat_tr %>% rowSums())
      }
      if (param == "vpd") {
        monthly_dat_tr <- monthly_dat
        val_year <- data.frame(id = 1:nrow(sites), year = year, value = monthly_dat_tr %>% rowMaxs())
      }
      val_year
    }
    val_list_allparam[[param]] <- bind_rows(val_list) %>%
      left_join(sites %>% mutate(id = row_number()), by = "id") %>%
      mutate(param = param)
  }
  val_df <- bind_rows(val_list_allparam) %>%
    as_tibble() %>%
    select(abbr, name, year, param, value) %>%
    pivot_wider(names_from = param, values_from = value) %>%
    rename(tmp = tas, ppt = pr) %>%
    arrange(abbr)

  outfile <- str_c(outdir, "site-annual.rds")
  write_rds(val_df, outfile)

  stopcluster(cl)

  return(outfile)
}
