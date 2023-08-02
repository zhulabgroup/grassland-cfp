setup_dir <- function(release = FALSE) {
  # setup base dir
  if (!release) { # create link to turbo
    base_dir <- "/nfs/turbo/seas-zhukai/grassland/package/"
    fs::link_create(str_c(base_dir, "input"), "input")
    fs::link_create(str_c(base_dir, "intermediate"), "intermediate")
    fs::link_create(str_c(base_dir, "output"), "output")
  } else { # create dir with files
    NULL
  }

  # setup input dir
  fs::dir_create(c(
    "input/basemap",
    "input/biogeography",
    "input/climate",
    "input/community"
  ))

  # setup intermediate dir
  fs::dir_create(c(
    "intermediate/background",
    "intermediate/climate-niche",
    "intermediate/observation-experiment",
    "intermediate/observation-experiment/tidy-community",
    "intermediate/observation-experiment/final-community",
    "intermediate/synthesis-decomposition"
  ))

  # setup output dir
  fs::dir_create(c(
    "output/data",
    "output/figures",
    "output/tables"
  ))
}
