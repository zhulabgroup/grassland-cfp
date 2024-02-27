#' @export
test_change_summ <- function(model, dat_model,n_comparison = 1, option, bonferroni = F) {
  df <- model$summary$coefficients %>% data.frame()
  if (option == "obs") {
    df_summ <- data.frame(
      estimate = df[2, 1],
      std.error = df[2, 2],
      p.value = df[2, ncol(df)] # good for both lm and lme
    )

    if (bonferroni) {
      df_summ <- df_summ %>%
        mutate(p.value = p.value * n_comparison)
    }

    df_summ <- df_summ %>%
      mutate(p.value_cap = case_when(p.value>1~1,
                                     TRUE~p.value)) %>%
      mutate(sig = gtools::stars.pval(p.value_cap)) %>%
      select(-p.value_cap) %>%
      mutate(sig = ifelse(sig != " " & sig != ".", sig, "ns"))
  }

  if (option == "exp") {

      df_summ <- data.frame(
        estimate = df[2, 1],
        std.error = df[2, 2],
        p.value = df[2, ncol(df)] # good for both lm and lme
      )
      if (bonferroni) {
        df_summ <- df_summ %>%
          mutate(p.value = p.value *n_comparison)
      }

      df_summ <- df_summ %>%
        mutate(p.value_cap = case_when(p.value>1~1,
                                       TRUE~p.value)) %>%
        mutate(sig = gtools::stars.pval(p.value_cap)) %>%
        select(-p.value_cap) %>%
        mutate(sig = ifelse(sig != " " & sig != ".", sig, "ns"))
  }

  return(list(df_summ))
}
