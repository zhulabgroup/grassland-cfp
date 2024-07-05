source("_setup.R")

tree_occ <- .path$tree_niche %>%
  str_c("occurrences.csv") %>%
  read_csv(col_types = "icdddddf")

# BIO1 = Annual Mean Temperature
# BIO5 = Max Temperature of Warmest Month
# BIO6 = Min Temperature of Coldest Month
