calc_individual_trait <- function(dat_occ, dat_clim,
                                  occ_source = "gbif",
                                  clim_source = "chelsa",
                                  outdir = "alldata/intermediate/climate-niche/") {
  occ_sf <- dat_occ[[occ_source]] %>%
    rename(
      species = consolidatedName,
    ) %>%
    select(key, species, geometry)
  occ_proj <- occ_sf %>%
    terra::crs(proj = T)

  clim_ras <- dat_clim[[clim_source]] %>%
    terra::rast()
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
