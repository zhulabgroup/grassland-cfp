#' @export
util_setup_dir <- function(sym_dir = "/nfs/turbo/seas-zhukai/grassland/") {
  if (!is.null(sym_dir)) {
    if (dir.exists("alldata")) {
      user_input <- readline("There is a folder called alldata. Do you want to remote it? Please enter Yes or No: ") %>% tolower()
      if (user_input == "yes" | user_input == "y") {
        fs::dir_delete("alldata")
      } else if (user_input == "no" | user_input == "n") {
        print("Symbolic link may not be generated because alldata folder already exists.")
      }
    }
    fs::link_create(sym_dir, "alldata")
  }

  # setup input dir
  fs::dir_create(c(
    "alldata/input/basemap",
    "alldata/input/biogeography",
    "alldata/input/climate",
    "alldata/input/community"
  ))

  # setup intermediate dir
  fs::dir_create(c(
    "alldata/intermediate/background",
    "alldata/intermediate/climate-niche",
    "alldata/intermediate/observation-experiment",
    "alldata/intermediate/observation-experiment/tidy-community",
    "alldata/intermediate/observation-experiment/final-community"
  ))

  # setup output dir
  fs::dir_create(c(
    "alldata/output/data",
    "alldata/output/figures/main",
    "alldata/output/figures/supp",
    "alldata/output/figures/slide",
    "alldata/output/figures/animation",
    "alldata/output/tables"
  ))
}
