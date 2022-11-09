# get BIEN data
# code can't run because of BIEN server error, as of 5/2/2022
library(foreach)
library(doSNOW)

# prep species list
source("scripts/compile-all-species.R")

# read CFP file
cfp_sf <- st_read(.path$geo_cfp) %>%
  filter(
    NAME == "California Floristic Province",
    Type == "hotspot area"
  )

num_cores <- 22 # socs-stats has 44 cores
cl <- makeCluster(num_cores)
registerDoSNOW(cl)
bien_df_list <-
  foreach(
    i = 1:length(spp_tbl$query_name),
    .packages = c("BIEN", "tidyverse")
  ) %dopar% {
    sp <- spp_tbl$query_name[i]
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

write_rds(bien_cfp_df, .path$occ_bien)
