summ_grass <- function(ras_grass) {
  percent <- ras_grass %>%
    terra::as.data.frame(xy = T) %>%
    as_tibble() %>%
    select(x, y, percent = Land_Cover_Type_1_Percent_10) %>%
    pull(percent) %>%
    mean()
  return(percent)
}
