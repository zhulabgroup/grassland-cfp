tidy_number <- function(x, round_digits = 3, sig_digits = 3) {
  if ((x %>% round(digits = round_digits)) == 0) {
    x <- 0
  }

  x <- x %>% signif(digits = sig_digits)

  return(x)
}
