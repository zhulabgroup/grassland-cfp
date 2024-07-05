library(tidyverse)

# set link to turbo
if (!file.exists("turbo")) {
  switch(Sys.info()[["sysname"]],
    Darwin = {
      R.utils::createLink(link = "turbo", "/Volumes/seas-zhukai")
    },
    Linux = {
      R.utils::createLink(link = "turbo", "/nfs/turbo/seas-zhukai")
    },
    Windows = { # Windows setup needs to have admin access
      R.utils::createLink(link = "turbo", "Z:\\")
    }
  )
}

# set path
.path <- list(
  fia_db = "turbo/datasets/vegetation/FIA/FIADB/2024-04-15/SQLite_FIADB_ENTIRE.db",
  fia_rds = "turbo/proj-wildfire-trait/FIA/",
  laughlin_mcgill = "turbo/proj-grassland-cfp/input/biogeography/laughlin-mcgill/occurrences.csv"
)

# check if path exists
lapply(.path, file.exists)
