setup_dir <- function(release = FALSE) {
  library(fs) # cross-platform file system operations

  # setup base dir
  if (!release) { # create link to turbo
    base_dir <- "~/.turbo/grassland/submission"
    link_create(str_c(base_dir, "/input"), "input")
    link_create(str_c(base_dir, "/intermediate"), "intermediate")
    link_create(str_c(base_dir, "/output"), "output")
  } else { # create dir with files
    NULL
  }

  # setup input dir
  dir_create(c(
    "input/basemap",
    "input/biogeography",
    "input/climate",
    "input/community"
  ))

  # setup intermediate dir
  dir_create(c(
    "intermediate/background",
    "intermediate/climate-niche",
    "intermediate/observation-experiment",
    "intermediate/observation-experiment/tidy-community",
    "intermediate/synthesis-decomposition"
  ))

  # setup output dir
  dir_create(c(
    "output/data",
    "output/figures",
    "output/tables"
  ))

  detach(package:fs, unload = TRUE)
}
