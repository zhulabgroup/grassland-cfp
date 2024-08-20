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
