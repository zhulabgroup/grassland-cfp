test_species_niche <- function (dat_niche) {
  res <- cor.test(dat_niche$tmp_occ_median, dat_niche$ppt_occ_median)
  return(res)
}
