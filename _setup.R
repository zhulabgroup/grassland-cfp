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
    Windows = { # Windows setups needs to have admin access
      R.utils::createLink(link = "turbo", "Z:\\")
    }
  )
}

# set path
.path <- list(
  fia_db = "turbo/datasets/vegetation/FIA/FIADB/2024-04-15/SQLite_FIADB_ENTIRE.db",
  fia_rds = "turbo/proj-wildfire-trait/FIA/",
  tree_niche = "turbo/proj-grassland-cfp/nsf-proposal/tree-niche/"
)

# check if path exists
lapply(.path, file.exists)
