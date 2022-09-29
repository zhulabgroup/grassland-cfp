cleanElkhornData <- function(data, gathercol1, gathercol2, site = c("elk", "swa", "ucsc")) {
  filtersite <- match.arg(site, c("elk", "swa", "ucsc"))
  keep_cols <- c("site", "freq", "treat", "rep", "species.code", "intercepts", "plot")
  if (!(gathercol1 %in% names(data)) | !(gathercol2 %in% names(data))) {
    stop("gathercol1 and gathercol2 must be columns in data.")
  }
  ind_col1 <- which(names(data) == gathercol1)
  ind_col2 <- which(names(data) == gathercol2)
  if (ind_col1 > ind_col2) stop("gathercol1 must come before gathercol2 in the column names of the data.")
  columns <- names(data)[ind_col1:ind_col2]
  data <- suppressWarnings(mutate(data, across(all_of(columns), function(x) as.numeric(x))))
  data %>%
    mutate(
      freq = tolower(freq),
      freq = if_else(freq == "control", "con", freq),
      site = tolower(site),
      site = if_else(site == "swan", "swa", site),
      site = if_else(site == "swanton", "swa", site)
    ) %>%
    filter(
      site == filtersite,
      freq == "con"
    ) %>%
    pivot_longer(all_of(columns), names_to = "species.code", values_to = "intercepts") %>%
    mutate(
      intercepts = as.numeric(intercepts),
      intercepts = if_else(is.na(intercepts), 0, intercepts),
      species.code = tolower(species.code)
    ) %>%
    select(any_of(keep_cols)) %>%
    return()
}

cleanElkhornPlot <- function(data) {
  data %>%
    pivot_wider(names_from = "plot", values_from = "intercepts", names_prefix = "plot") %>%
    mutate(intercepts = (plot1 + plot2 + plot3 + plot4)) %>%
    select(-plot1, -plot2, -plot3, -plot4)
}
