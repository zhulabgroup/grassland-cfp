test_change_summ <- function(model) {
  df <- model$summary$coefficients %>% data.frame()

  df_summ <- data.frame(
    estimate = df[2, 1],
    std.error = df[2, 2],
    p.value = df[2, ncol(df)] # good for both lm and lme
  ) %>%
    mutate(p.value_cap = case_when(
      p.value > 1 ~ 1,
      TRUE ~ p.value
    )) %>%
    mutate(sig = gtools::stars.pval(p.value_cap)) %>%
    select(-p.value_cap) %>%
    mutate(sig = ifelse(sig != " " & sig != ".", sig, "ns"))

  return(df_summ)
}
