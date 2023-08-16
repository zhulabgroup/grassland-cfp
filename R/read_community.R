read_community <- function(version = c("tidy", "final")) {
  if (version == "tidy") {
    indir <- "alldata/intermediate/observation-experiment/tidy-community/"
  } else if (version == "final") {
    indir <- "alldata/intermediate/observation-experiment/final-community/"
  }

  obs_tbl <- c(
    "angelo", "carrizo", "elkhorn", "jasper", "mclann", "mclserp", "morganterritory",
    "pleasantonridge", "sunol", "swanton", "ucsc", "vascocaves"
  ) %>%
    str_c(indir, ., ".csv") %>%
    map_df(~ read_csv(., col_types = "cicccdc"))

  exp_tbl <- c("jrgce", "mclexp", "scide") %>%
    str_c(indir, ., ".csv") %>%
    map_df(~ read_csv(., col_types = "ciccccdc"))

  out <- list(obs = obs_tbl, exp = exp_tbl)

  return(out)
}
