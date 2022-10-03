# prep species list
source("scripts/compile-all-species.R")

# read CFP file
cfp_sf <- st_read(.path$geo_cfp) %>%
  filter(
    NAME == "California Floristic Province",
    Type == "hotspot area"
  )

# read combined GBIF and terraclim data
gbif_terraclim_sf <- read_rds(.path$cli_all_gbif) %>%
  select(geometry, key, species, tmp = terraclim_tmp, ppt = terraclim_ppt, vpd = terraclim_vpd) %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")

# check species lists
sp_gbif_vec <- gbif_terraclim_sf %>%
  pull(species) %>%
  unique() %>%
  sort()
sp_comm_vec <- spp_tbl %>%
  mutate(final_name = ifelse(
    is.na(consolidated_name),
    query_name,
    consolidated_name
  )) %>%
  pull(final_name) %>%
  unique() %>%
  sort()
sp_gbif_vec %in% sp_comm_vec

# gbif summary
tmp_rng <- range(gbif_terraclim_sf$tmp)
ppt_rng <- range(gbif_terraclim_sf$ppt)
n_occ_tot <- nrow(gbif_terraclim_sf)
# exp data summary
exp_tbl <- read_rds(.path$com_exp)
n_exp_tot <- nrow(exp_tbl)
# obs data summary
obs_tbl <- read_rds(.path$com_obs)
n_obs_tot <- nrow(obs_tbl)
niche_gg <- vector(mode = "list")

for (i in seq_along(sp_gbif_vec)) {
  sp <- sp_gbif_vec[i]
  occ_sp_sf <- filter(gbif_terraclim_sf, species == sp)

  occ_sp_stat <- occ_sp_sf %>%
    as_tibble() %>%
    summarise(
      occ_n = n(),
      tmp_occ_mean = mean(tmp, na.rm = TRUE),
      tmp_occ_sd = sd(tmp, na.rm = TRUE),
      ppt_occ_mean = mean(ppt, na.rm = TRUE),
      ppt_occ_sd = sd(ppt, na.rm = TRUE)
    )
  n_exp_sp <- exp_tbl %>%
    filter(species == sp) %>%
    nrow()
  n_obs_sp <- obs_tbl %>%
    filter(species == sp) %>%
    nrow()

  occ_geog <- ggplot() +
    geom_sf(
      data = rnaturalearth::ne_states(
        country = c("Mexico", "United States of America"),
        returnclass = "sf"
      ),
      fill = NA,
      color = alpha("black", .1)
    ) +
    geom_sf(data = cfp_sf, fill = "white", alpha = .5) +
    geom_sf(data = occ_sp_sf, alpha = .1, size = .5) +
    labs(x = "Longitude", y = "Latitude", title = sp) +
    coord_sf(xlim = c(-125, -115), ylim = c(28, 44)) +
    theme(plot.title = element_text(face = "italic"))

  occ_clim <- ggplot(occ_sp_sf, aes(tmp, ppt)) +
    geom_point(alpha = .5, size = .5) +
    geom_rug(alpha = .5) +
    stat_ellipse(col = "red") +
    geom_point(
      data = occ_sp_stat,
      aes(x = tmp_occ_mean, y = ppt_occ_mean),
      shape = 3, col = "red", size = 10
    ) +
    lims(x = tmp_rng, y = ppt_rng) +
    # scale_y_log10() + # log scale ppt
    labs(
      x = "Mean annual temperature (°C)", y = "Annual precipitation (mm)",
      title = str_c(
        "n_occ = ", occ_sp_stat$occ_n, " (", round(occ_sp_stat$occ_n / n_occ_tot * 100, digits = 3), "%)\n",
        "n_exp = ", n_exp_sp, " (", round(n_exp_sp / n_exp_tot * 100, digits = 3), "%)\n",
        "n_obs = ", n_obs_sp, " (", round(n_obs_sp / n_obs_tot * 100, digits = 3), "%)"
      )
    )

  print(sp)
  niche_gg[[i]] <- occ_geog + occ_clim # no need to print(); will slow down
}

names(niche_gg) <- sp_gbif_vec
