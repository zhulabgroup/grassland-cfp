test_change_summ <- function(mod_lme) {
  df_summ <- data.frame(
    estimate = mod_lme$summary$coefficients[2, 1],
    p.value = mod_lme$summary$coefficients[2, ncol(mod_lme$summary$coefficients)] # good for both lm and lme
  ) %>%
    mutate(sig = gtools::stars.pval(p.value))

  return(df_summ)
}
