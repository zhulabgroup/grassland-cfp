tidy_number <- function(x, round_digits = 3, sig_digits = 3) {
  if ((x %>% round(digits = round_digits)) == 0) {
    x <- 0
  }

  x <- x %>% signif(digits = sig_digits)

  return(x)
}

tidy_p_value <- function(p.value, sig = NULL) {
  if (is.null(sig)) {
    sig <- p.value %>% gtools::stars.pval()
    if (sig == " " | sig == ".") {
      sig <- "ns"
    }
  }
  if (p.value < 0.0001) {
    return(str_c("'", sig, " ' (italic(p) < '0.0001')"))
  } else {
    return(str_c("'", sig, " ' (italic(p) == '", p.value %>% round(4) %>% format(scientific = F), "')"))
  }
}
