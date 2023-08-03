extract_individual_trait <- function(dat_occ, dat_clim,
                                     occ_source = "gbif",
                                     clim_source = "chelsa",
                                     outdir = "intermediate/climate-niche/") {
  occ_sf <- dat_occ[[occ_source]] %>%
    rename(
      species = consolidatedName,
    ) %>%
    select(key, species, geometry)
  occ_proj <- occ_sf %>%
    terra::crs(proj = T)

  clim_ras <- dat_climate[[clim_source]]
  clim_proj <- clim_ras %>%
    terra::crs(proj = T)

  if (occ_proj != clim_proj) {
    occ_sf <- occ_sf %>%
      st_transform(crs = clim_proj)
  }

  # extract climate data on locations
  dat_clim_occ <- clim_ras %>%
    terra::extract(occ_sf,
      ID = F,
      bind = T,
      xy = F
    ) %>%
    as_tibble()

  outfile <- str_c(outdir, occ_source, "-", clim_source, ".rds")
  write_rds(dat_clim_occ, outfile)
  return(outfile)
}

load_individual_trait <- function(path_trait = NULL,
                                  occ_source = "gbif",
                                  clim_source = "chelsa",
                                  indir = "intermediate/climate-niche/") {
  if (is.null(path_trait)) {
    path_trait <- str_c(indir, occ_source, "-", clim_source, ".rds")
  }

  dat_niche <- read_rds(path_trait)

  return(dat_niche)
}
