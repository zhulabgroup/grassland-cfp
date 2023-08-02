get_biogeography <- function(species_table, cfp_sf, num_cores = 22, outdir = "input/biogeography/", date = NULL) {
  path_gbif <- download_gbif(species_table, cfp_sf, num_cores, outdir, date)
  path_bien <- download_bien(species_table, cfp_sf, num_cores, outdir, date)
  path_cch <- download_cch(species_table, cfp_sf, outdir, date)
  path_inat <- download_inat(gbif_file = path_gbif, date)

  out <- list(
    gbif = path_gbif,
    bien = path_bien,
    cch = path_cch,
    inat = path_inat
  )
  return(out)
}

download_gbif <- function(species_table, cfp_sf, num_cores, outdir, date) {
  if (is.null(date)) {
    date <- Sys.Date()
  }
  outfile <- str_c(outdir, "gbif-", date, ".rds")

  cl <- makeCluster(num_cores)
  registerDoSNOW(cl)

  gbif_box_ls <-
    foreach(
      i = 1:length(species_table$query_name),
      .packages = c("spocc", "sf", "tidyverse")
    ) %dopar% {
      sp <- species_table$query_name[i]
      res <- occ(
        query = sp, from = "gbif", has_coords = TRUE, limit = 1e6,
        geometry = st_bbox(cfp_sf),
        gbifopts = list(
          # occ_options(from = "gbif", where = "console")
          hasGeospatialIssue = FALSE
        )
      )
      print(i)
      tibble(queryName = sp, gbif = res$gbif$data) %>%
        unnest(cols = c(gbif))
    }
  stopCluster(cl)

  gbif_box_tbl <- bind_rows(gbif_box_ls) # collapse from list to tibble

  gbif_cfp_tbl <- gbif_box_tbl %>%
    select(key, longitude, latitude) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    st_intersection(cfp_sf) %>% # find GBIF and CFP intersection
    as_tibble() %>% # drop geometry
    select(key) %>% # use key to join (fast)
    right_join(gbif_box_tbl, ., by = "key")

  # filter coords and consolidate species
  dat_gbif <- gbif_cfp_tbl %>%
    # filter(coordinatePrecision < 0.01 | is.na(coordinatePrecision)) %>%
    filter(coordinateUncertaintyInMeters < 10000 | is.na(coordinateUncertaintyInMeters)) %>%
    filter(!coordinateUncertaintyInMeters %in% c(301, 3036, 999, 9999)) %>%
    left_join(species_table, by = c("queryName" = "query_name")) %>%
    select(consolidatedName = consolidated_name, everything()) %>%
    mutate(
      consolidatedName =
        ifelse(
          is.na(consolidatedName),
          queryName,
          consolidatedName
        )
    ) %>%
    distinct() # remove duplicated data entries

  write_rds(dat_gbif, outfile)
  return(outfile)
}

download_bien <- function(species_table, cfp_sf, num_cores, outdir, date) {
  if (is.null(date)) {
    date <- Sys.Date()
  }
  outfile <- str_c(outdir, "bien-", date, ".rds")

  cl <- makeCluster(num_cores)
  registerDoSNOW(cl)
  bien_df_list <-
    foreach(
      i = 1:length(species_table$query_name),
      .packages = c("BIEN", "tidyverse")
    ) %dopar% {
      sp <- species_table$query_name[i]
      res <- BIEN_occurrence_species(species = sp)
      print(i)
      tibble(queryName = sp, res) %>%
        mutate(date_collected = as.Date(date_collected))
    }
  bien_all_df <- bind_rows(bien_df_list) %>%
    mutate(key = row_number()) %>%
    drop_na(latitude, longitude)
  stopCluster(cl)

  bien_all_sf <- st_as_sf(
    x = bien_all_df %>% dplyr::select(key, longitude, latitude),
    coords = c("longitude", "latitude"),
    crs = 4326
  )
  bien_cfp_sf <- st_intersection(bien_all_sf, cfp_sf)
  bien_cfp_df <- bien_all_df %>%
    right_join(as_tibble(bien_cfp_sf) %>% dplyr::select(key), by = "key")

  write_rds(bien_cfp_df, outfile)

  return(outfile)
}

download_cch <- function(species_table, cfp_sf, outdir, date) {
  if (is.null(date)) {
    date <- Sys.Date()
  }
  outfile <- str_c(outdir, "cch-", date, ".rds")

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
    select(
      queryName = scientificName,
      longitude = decimalLongitude,
      latitude = decimalLatitude,
      key = id
    ) %>%
    distinct(key, .keep_all = T) %>%
    left_join(species_table, by = c("queryName" = "query_name")) %>%
    mutate(species_name = ifelse(
      is.na(consolidated_name),
      queryName,
      consolidated_name
    )) %>%
    select(species_name, longitude, latitude, key)

  # filter by CFP
  cch_cfp_tbl <- cch_all_tbl %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    st_intersection(cfp_sf) %>% # find CCH and CFP intersection
    as_tibble() %>% # drop geometry
    select(key) %>% # use key to join (fast)
    right_join(cch_all_tbl, ., by = "key")

  write_rds(cch_cfp_tbl, outfile)
}

download_inat <- function(gbif_file, date = NULL) {
  if (is.null(date)) {
    date <- Sys.Date()
  }
  outfile <- str_c(outdir, "inat-", date, ".rds")

  dat_inat <- read_rds(gbif_file) %>%
    filter(datasetName == "iNaturalist research-grade observations")

  write_rds(dat_inat, outfile)

  return(outfile)
}
