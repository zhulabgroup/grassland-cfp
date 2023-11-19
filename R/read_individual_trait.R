#' @export
read_individual_trait <- function(path_trait = NULL,
                                  occ_source = "gbif",
                                  clim_source = "chelsa",
                                  indir = "alldata/intermediate/climate-niche/") {
  if (is.null(path_trait)) {
    path_trait <- str_c(indir, occ_source, "-", clim_source, ".rds")
  }

  dat_niche <- read_rds(path_trait)

  return(dat_niche)
}
