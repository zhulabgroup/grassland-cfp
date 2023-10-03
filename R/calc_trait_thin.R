calc_trait_thin <- function(path_occ_thin = NULL, dat_trait, option = "spatial") {
  if (is.null(path_occ_thin)) {
    path_occ_thin <- str_c("alldata/intermediate/climate-niche/occ_thin_", option, ".rds")
  }

  dat_occ_thin <- read_rds(path_occ_thin)
  dat_trait_thin <- dat_trait %>%
    right_join(
      dat_occ_thin %>%
        select(key, species = consolidatedName),
      by = c("key", "species")
    )

  return(dat_trait_thin)
}

cal_thin_occ <- function(dat_occ, dat_trait, outdir = "alldata/intermediate/climate-niche/", option = "spatial") {
  cl <- makeCluster(detectCores() - 1, outfile = "")
  registerDoSNOW(cl)

  v_species <- dat_occ$consolidatedName %>% unique()

  ls_dat_occ_thin <-
    foreach(
      species = v_species,
      .packages = c("tidyverse", "sf"),
      .export = c("calc_thin_occ_sp")
    ) %dopar% {
      print(species)
      thin <- dat_occ %>%
        filter(consolidatedName == species) %>%
        mutate(
          lon = unlist(map(.$geometry, 1)),
          lat = unlist(map(.$geometry, 2))
        ) %>%
        left_join(dat_trait, by = c("key", "consolidatedName" = "species")) %>%
        mutate(
          tmp = (tmp - mean(tmp, na.rm = T)) / sd(tmp, na.rm = T) * (1 / 2),
          ppt = (ppt - mean(ppt, na.rm = T)) / sd(ppt, na.rm = T) * (1 / 2),
          vpd = (vpd - mean(vpd, na.rm = T)) / sd(vpd, na.rm = T) * (1 / 2)
        ) %>% # standardize to mean 0, standard deviation 0.5
        calc_thin_occ_sp(option = option)
      thin
    }
  dat_occ_thin <- bind_rows(ls_dat_occ_thin)
  stopCluster(cl)


  path_occ_thin <- str_c(outdir, "occ_thin_", option, ".rds")
  write_rds(dat_occ_thin, path_occ_thin)

  return(path_occ_thin)
}

calc_thin_occ_sp <- function(data, option = "spatial") {
  set.seed(31416)

  # define function to use spThin to sample and return data frame

  full_tbl <- tibble(species = "dummy", data) # create dummy species for spThin function

  if (option == "spatial") {
    thin_ls <- spThin::thin(
      loc.data = full_tbl,
      lat.col = "lat",
      long.col = "lon",
      spec.col = "species",
      thin.par = 10, # distance in km that records to be separated by
      reps = 1,
      locs.thinned.list.return = TRUE,
      write.files = FALSE,
      write.log.file = FALSE
    )
  }

  if (option == "climate") {
    thin_ls <- spThin::thin(
      loc.data = full_tbl,
      lat.col = "tmp",
      long.col = "ppt",
      spec.col = "species",
      thin.par = 10, # distance in km that records to be separated by
      reps = 1,
      locs.thinned.list.return = TRUE,
      write.files = FALSE,
      write.log.file = FALSE
    )
  }

  thin_tbl <- thin_ls[[1]] %>%
    rownames() %>%
    as.numeric() %>%
    slice(full_tbl, .) %>%
    select(consolidatedName, key)
}
