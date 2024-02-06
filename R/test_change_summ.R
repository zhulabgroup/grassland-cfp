#' @export
test_change_summ <- function(model, dat_model, option) {
  df <- model$summary$coefficients %>% data.frame()
  if (option == "obs") {
    df_summ <- data.frame(
      estimate = df[2, 1],
      std.error = df[2, 2],
      p.value = df[2, ncol(df)] # good for both lm and lme
    ) %>%
      mutate(sig = gtools::stars.pval(p.value)) %>%
      mutate(sig = ifelse(sig != " " & sig != ".", sig, "ns"))
  }

  if (option == "exp") {
    if ("subgrp" %in% colnames(dat_model)) {
      df_term <- df %>%
        rownames_to_column(var = "term") %>%
        filter(str_detect(term, "trt")) %>%
        select(-term)

      df_summ <- data.frame(
        subgrp = dat_model %>% pull(subgrp) %>% unique() %>% sort(na.last = T),
        estimate = df_term[, 1],
        std.error = df_term[, 2],
        p.value = df_term[, ncol(df_term)] # good for both lm and lme
      ) %>%
        mutate(p.value = p.value * dat_model %>%
          pull(subgrp) %>%
          unique() %>%
          length()) %>% # bonferroni correction
        mutate(sig = gtools::stars.pval(p.value)) %>%
        mutate(sig = ifelse(sig != " " & sig != ".", sig, "ns"))
    } else {
      df_summ <- data.frame(
        estimate = df[2, 1],
        std.error = df[2, 2],
        p.value = df[2, ncol(df)] # good for both lm and lme
      ) %>%
        mutate(sig = gtools::stars.pval(p.value)) %>%
        mutate(sig = ifelse(sig != " " & sig != ".", sig, "ns"))
    }
  }

  return(list(df_summ))
}
