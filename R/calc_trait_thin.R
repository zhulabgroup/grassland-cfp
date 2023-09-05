calc_trait_thin <- function(path_occ_thin = NULL, dat_trait) {
  if (is.null(path_occ_thin)) {
    path_occ_thin <- "alldata/intermediate/climate-niche/occ_thin.rds"
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

cal_thin_occ <- function(dat_occ, outdir = "alldata/intermediate/climate-niche/") {
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
        calc_thin_occ_sp()
      thin
    }
  dat_occ_thin <- bind_rows(ls_dat_occ_thin)
  stopCluster(cl)


  path_occ_thin <- str_c(outdir, "occ_thin.rds")
  write_rds(dat_occ_thin, path_occ_thin)

  return(path_occ_thin)
}

calc_thin_occ_sp <- function(data) {
  set.seed(31416)

  # define function to use spThin to sample and return data frame

  full_tbl <- tibble(species = "dummy", data) %>% # create dummy species for spThin function
    mutate(
      lon = unlist(map(.$geometry, 1)),
      lat = unlist(map(.$geometry, 2))
    )

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

  thin_tbl <- thin_ls[[1]] %>%
    rownames() %>%
    as.numeric() %>%
    slice(full_tbl, .) %>%
    select(-species, -lon, -lat)
}
