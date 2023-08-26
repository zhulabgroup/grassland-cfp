save_rda <- function(out, overwrite = F) {
  for (i in 1:length(out)) {
    try({
      usethis::use_data(out[[i]], overwrite = overwrite)
    })
  }
}

save_extdata <- function(out, dir = NULL) {
  if (is.null(dir)) {
    dir <- ""
  }
  for (i in 1:length(out)) {
    file.copy(from = out[[i]], to = str_c("inst/extdata/", dir), recursive = T)
  }
}
