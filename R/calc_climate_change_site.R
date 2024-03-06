#' @export
calc_climate_change_site <- function(dat_clim_site) {
  df_cc_site_all <- dat_clim_site %>%
    select(site, year, tmp, ppt) %>%
    mutate(year = year - 1980) %>%
    pivot_longer(tmp:ppt, names_to = "clim_var", values_to = "clim_val") %>%
    mutate(clim_var = factor(clim_var,
      levels = c("tmp", "ppt")
    )) %>%
    group_by(clim_var) %>%
    nest() %>%
    mutate(
      map(data, ~ lmerTest::lmer(clim_val ~ year + (1 + year | site), data = .)) %>%
        map_df(~ summary(.) %>%
          coefficients() %>%
          data.frame() %>%
          rownames_to_column("term") %>%
          filter(term == "year") %>%
          select(
            estimate = Estimate,
            std.error = Std..Error,
            p.value = Pr...t..
          ))
    ) %>%
    select(-data) %>%
    ungroup() %>%
    mutate(
      site = "all"
    )

  df_cc_site_ind <- dat_clim_site %>%
    select(site, year, tmp, ppt) %>%
    pivot_longer(tmp:ppt, names_to = "clim_var", values_to = "clim_val") %>%
    mutate(clim_var = factor(clim_var,
      levels = c("tmp", "ppt")
    )) %>%
    group_by(site, clim_var) %>%
    nest() %>%
    mutate(
      map(data, ~ lm(clim_val ~ year, data = .)) %>%
        map_df(~ broom::tidy(.) %>%
          filter(term == "year") %>%
          select(estimate, std.error, p.value)) # ,
    ) %>%
    select(-data) %>%
    ungroup()

  df_cc_site <- bind_rows(
    df_cc_site_all,
    df_cc_site_ind
  ) %>%
    select(site, everything()) %>%
    mutate(sig = gtools::stars.pval(p.value)) %>%
    mutate(sig = ifelse(sig != " " & sig != ".", sig, "ns"))

  return(df_cc_site)
}

#' @export
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
    select(site, year, param, value) %>%
    pivot_wider(names_from = param, values_from = value) %>%
    rename(tmp = tas, ppt = pr) %>%
    arrange(site)

  outfile <- str_c(outdir, "site-annual.rds")
  write_rds(val_df, outfile)

  stopcluster(cl)

  return(outfile)
}
