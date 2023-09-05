summ_change <- function(df_change, option = "obs") {
  grouping_factor <- intersect(df_change %>% colnames(), c("index", "trait"))
  if (option == "obs") {
    df <- df_change %>%
      filter(site != "all") %>%
      group_by(!!sym(grouping_factor)) %>%
      summarise(
        n = n(),
        n_sig = sum(str_detect(sig, "\\*")),
        mean = mean(estimate),
        min = min(estimate),
        max = max(estimate)
      )
  }

  return(df)
}
