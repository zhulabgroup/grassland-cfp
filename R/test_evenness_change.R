#' @export
test_evenness_change <- function(dat_evenness) {
  out <- list(
    obs = test_evenness_change_obs(dat_evenness_obs = dat_evenness$obs),
    exp = test_evenness_change_exp(dat_evenness_exp = dat_evenness$exp)
  )
  return(out)
}

#' @export
test_evenness_change_obs <- function(dat_evenness_obs) {
  df <- dat_evenness_obs %>%
    group_by(site) %>%
    nest() %>%
    mutate(
      lm = map(
        data,
        ~ lm(even ~ year, data = .) %>%
          broom::tidy() %>%
          filter(term == "year") %>%
          select(estimate, std.error, p.value) %>%
          # mutate(across(everything(), signif, 4)) %>%
          mutate(sig = gtools::stars.pval(p.value))
      )
    ) %>%
    unnest(cols = lm) %>%
    select(-data)

  return(df)
}

#' @export
test_evenness_change_exp <- function(dat_evenness_exp) {
  df <- ggpubr::compare_means(
    formula = even ~ treat_T,
    data = dat_evenness_exp,
    method = "wilcox.test",
    group.by = "year",
    ref.group = "_"
  ) %>%
    mutate(phase = case_when(
      year <= 2002 ~ "Phase I",
      year > 2002 & year <= 2009 ~ "Phase II",
      year > 2009 ~ "Phase III"
    )) %>%
    select(phase, year, p.value = p) %>%
    mutate(sig = gtools::stars.pval(p.value))

  return(df)
}
