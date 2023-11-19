#' @export
save_extdata <- function(out, dir = NULL) {
  if (is.null(dir)) {
    dir <- ""
  }
  for (i in 1:length(out)) {
    file.copy(from = out[[i]], to = str_c("inst/extdata/", dir), recursive = T)
  }
}
