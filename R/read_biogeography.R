read_biogeography <- function(path_occ = NULL, indir = "alldata/input/biogeography/", gbif_only = F) {
  if (is.null(path_occ)) {
    path_occ <- list(
      gbif = list.files(indir, pattern = "gbif-20", full.names = T) %>% tail(1),
      bien = list.files(indir, pattern = "bien-20", full.names = T) %>% tail(1),
      cch = list.files(indir, pattern = "cch-20", full.names = T) %>% tail(1),
      inat = list.files(indir, pattern = "inat-20", full.names = T) %>% tail(1)
    )
  }

  out <- vector(mode = "list")

  if (gbif_only) {
    dataset <- c(
      "gbif"
    )
  } else {
    dataset <- c(
      "gbif",
      "bien",
      "cch",
      "inat"
    )
  }

  for (d in dataset) {
    df <- read_rds(path_occ[[d]]) %>%
      mutate(
        dataset = d
      ) %>%
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326) # WGS84

    out[[d]] <- df
  }

  return(out)
}
