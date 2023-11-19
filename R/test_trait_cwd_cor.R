#' @export
test_trait_cwd_cor <- function(dat_trait_cwd) {
  out <- list(
    cwd_ppt = cor.test(~ cwd + ppt, data = dat_trait_cwd),
    cwd_tmp = cor.test(~ cwd + tmp, data = dat_trait_cwd),
    ppt_tmp = cor.test(~ ppt + tmp, data = dat_trait_cwd)
  )
  return(out)
}
