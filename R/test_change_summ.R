test_change_summ <- function(model) {
  df_summ <- data.frame(
    estimate = model$summary$coefficients[2, 1],
    std.error = model$summary$coefficients[2, 2],
    p.value = model$summary$coefficients[2, ncol(model$summary$coefficients)] # good for both lm and lme
  ) %>%
    mutate(sig = gtools::stars.pval(p.value)) %>%
    mutate(sig = ifelse(sig != " " & sig != ".", sig, "ns"))

  return(df_summ)
}
