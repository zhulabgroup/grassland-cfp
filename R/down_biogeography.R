down_biogeography <- function(species_table, num_cores = 22, outdir = "alldata/input/biogeography/", date = NULL, start_year = NULL, end_year = NULL, gbif_only = F, postfox = "") {
  sf_cfp <- read_cfp(path_cfp = system.file("extdata", "cfp", package = "grassland"))

  path_gbif <- down_gbif(species_table, sf_cfp, num_cores, outdir, date, start_year = start_year, end_year = end_year, postfix = postfix)
  if (!gbif_only) {
    path_bien <- down_bien(species_table, sf_cfp, num_cores, outdir, date) # sometimes blocked by firewall, in which case it needs to be run somewhere else
    path_cch <- down_cch(species_table, sf_cfp, outdir, date)
    path_inat <- down_inat(gbif_file = path_gbif, date)
  } else {
    path_bien <- path_cch <- path_inat <- NULL
  }

  out <- list(
    gbif = path_gbif,
    bien = path_bien,
    cch = path_cch,
    inat = path_inat
  )

  return(out)
}

down_gbif <- function(species_table, sf_cfp, num_cores, outdir, date, start_year = NULL, end_year = NULL, postfix = "") {
  if (is.null(date)) {
    date <- Sys.Date()
  }
  if (is.null(start_year) & is.null(end_year)) {
    date_range <- c(str_c(start_year, "-01-01"), str_c(end_year, "-12-31"))
  } else {
    date_range <- NULL
  }
  outfile <- str_c(outdir, "gbif-", date, ".rds")
  outfile_full <- str_c(outdir, "gbif-full-", date, ".rds")

  if (!is.null(postfix)) {
    outfile <- str_c(outdir, "gbif-", postfix, "-", date, ".rds")
    outfile_full <- str_c(outdir, "gbif-full-", postfix, "-", date, ".rds")
  }
  cl <- makeCluster(num_cores, outfile = "")
  registerDoSNOW(cl)

  gbif_box_ls <-
    foreach(
      i = 1:length(species_table$query_name),
      .packages = c("spocc", "sf", "tidyverse")
    ) %dopar% {
      sp <- species_table$query_name[i]
      res <- spocc::occ(
        query = sp,
        from = "gbif",
        has_coords = TRUE,
        limit = 1e6,
        geometry = st_bbox(sf_cfp),
        date = date_range,
        gbifopts = list(
          # occ_options(from = "gbif", where = "console")
          hasGeospatialIssue = FALSE
        )
      )
      print(str_c(i, " ", sp))
      tibble(queryName = sp, gbif = res$gbif$data) %>%
        unnest(cols = c(gbif))
    }
  stopCluster(cl)

  gbif_box_tbl <- bind_rows(gbif_box_ls) %>% # collapse from list to tibble
    distinct(key, .keep_all = T) # remove duplicated data entries

  gbif_cfp_tbl <- gbif_box_tbl %>%
    select(key, longitude, latitude) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    st_intersection(sf_cfp) %>% # find GBIF and CFP intersection
    # as_tibble() %>% # drop geometry
    select(key) %>% # use key to join (fast)
    right_join(gbif_box_tbl, ., by = "key")

  # filter coords and consolidate species
  dat_gbif_full <- gbif_cfp_tbl %>%
    # filter(coordinatePrecision < 0.01 | is.na(coordinatePrecision)) %>%
    filter(coordinateUncertaintyInMeters < 10000 | is.na(coordinateUncertaintyInMeters)) %>%
    filter(!coordinateUncertaintyInMeters %in% c(301, 3036, 999, 9999)) %>%
    left_join(species_table, by = c("queryName" = "query_name")) %>%
    select(queryName, consolidatedName = consolidated_name, key, longitude, latitude, everything())

  dat_gbif <- dat_gbif_full %>%
    select(queryName, consolidatedName, key, longitude, latitude)

  write_rds(dat_gbif_full, outfile_full)
  write_rds(dat_gbif, outfile)
  return(outfile)
}

down_bien <- function(species_table, sf_cfp, num_cores, outdir, date) {
  if (is.null(date)) {
    date <- Sys.Date()
  }
  outfile <- str_c(outdir, "bien-", date, ".rds")
  outfile_full <- str_c(outdir, "bien-full-", date, ".rds")

  cl <- makeCluster(num_cores, outfile = "")
  registerDoSNOW(cl)
  bien_df_list <-
    foreach(
      i = 1:length(species_table$query_name),
      .packages = c("BIEN", "tidyverse")
    ) %dopar% {
      sp <- species_table$query_name[i]
      res <- BIEN::BIEN_occurrence_species(species = sp)
      print(str_c(i, " ", sp))
      tibble(queryName = sp, res) %>%
        mutate(date_collected = as.Date(date_collected))
    }
  stopCluster(cl)

  bien_all_df <- bind_rows(bien_df_list) %>%
    mutate(key = row_number()) %>%
    drop_na(latitude, longitude)

  bien_all_sf <- st_as_sf(
    x = bien_all_df %>% dplyr::select(key, longitude, latitude),
    coords = c("longitude", "latitude"),
    crs = 4326
  )
  bien_cfp_sf <- st_intersection(bien_all_sf, sf_cfp)

  dat_bien_full <- bien_all_df %>%
    right_join(as_tibble(bien_cfp_sf) %>% dplyr::select(key), by = "key") %>%
    left_join(species_table, by = c("queryName" = "query_name")) %>%
    select(queryName, consolidatedName = consolidated_name, key, longitude, latitude, everything())

  dat_bien <- dat_bien_full %>%
    select(queryName, consolidatedName, key, longitude, latitude) %>%
    mutate(key = as.character(key))

  write_rds(dat_bien_full, outfile_full)
  write_rds(dat_bien, outfile)

  return(outfile)
}

down_cch <- function(species_table, sf_cfp, outdir, date) {
  if (is.null(date)) {
    date <- Sys.Date()
  }
  outfile <- str_c(outdir, "cch-", date, ".rds")
  outfile_full <- str_c(outdir, "cch-full-", date, ".rds")

  # downloaded the data manually from https://www.cch2.org/portal/collections/harvestparams.php
  # downloading limit is 1000000 records, so downloaded in three latitudinal bands
  # 125-115W
  # 28-34N 447335 records
  # 34-38N 914648 records
  # 38-44N 705205 records
  # retrieved on 11-08-2022 2pm
  cch_dir <- str_c(outdir, "cch/")

  # read manually downloaded file
  ls_cch <- vector(mode = "list")
  for (f in c("south", "mid", "north")) {
    ls_cch[[f]] <- list.files(cch_dir, pattern = f, full.names = T) %>%
      read_csv(col_types = cols_only(
        id = "i",
        scientificName = "c",
        decimalLongitude = "d",
        decimalLatitude = "d"
      ))
  }

  cch_all_tbl <- bind_rows(ls_cch) %>%
    drop_na(decimalLongitude, decimalLatitude) %>%
    inner_join(species_table, by = c("scientificName" = "query_name")) %>%
    rename(
      queryName = scientificName,
      consolidatedName = consolidated_name,
      longitude = decimalLongitude,
      latitude = decimalLatitude,
      key = id
    ) %>%
    distinct(key, .keep_all = T) %>%
    mutate(key = as.character(key))

  # filter by CFP
  dat_cch_full <- cch_all_tbl %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    st_intersection(sf_cfp) %>% # find CCH and CFP intersection
    select(key) %>% # use key to join (fast)
    right_join(cch_all_tbl, ., by = "key") %>%
    select(queryName, consolidatedName, key, longitude, latitude, everything())

  dat_cch <- dat_cch_full %>%
    select(queryName, consolidatedName, key, longitude, latitude)

  write_rds(dat_cch_full, outfile_full)
  write_rds(dat_cch, outfile)

  return(outfile)
}

down_inat <- function(gbif_file, date = NULL) {
  if (is.null(date)) {
    date <- Sys.Date()
  }
  outfile <- str_c(outdir, "inat-", date, ".rds")

  dat_inat <- read_rds(gbif_file) %>%
    filter(datasetName == "iNaturalist research-grade observations") %>%
    select(queryName, consolidatedName, key, longitude, latitude)

  write_rds(dat_inat, outfile)

  return(outfile)
}
