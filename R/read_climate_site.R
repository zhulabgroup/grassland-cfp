read_climate_site <- function(path = "alldata/intermediate/background/site-annual.rds") {
  df <- read_rds(path) %>%
    filter(abbr %in% c(
      "angelo", "carrizo", "elkhorn", "jasper", "mclann", "mclserp",
      "morganterritory", "pleasantonridge", "sunol",
      "swanton", "ucsc", "vascocaves"
    ))

  return(df)
}
