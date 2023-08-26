down_climate_monthly <- function(outdir = "alldata/input/climate/monthly/") {
  # download CHELSA monthly data
  # extract at site locations
  # Get download links from https://envicloud.wsl.ch/#/

  param_list <- c("tas", "pr", "vpd")
  for (param in param_list) {
    dir.create(str_c(outdir, param, "/raw/"))
    system(paste0("cat ", outdir, param, "/paths.txt | xargs -n 1 -P 40 wget --no-verbose --no-clobber --no-host-directories --no-directories --continue --directory-prefix=", outdir, param, "/raw"))
  }
  return(outdir)
}
