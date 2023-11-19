# docu

#' @export
util_install_dependency <- function() {
  # Install and load the desc package
  if (!require(desc)) {
    install.packages("desc")
  }

  # Install and load the pacman package
  if (!require(pacman)) {
    install.packages("pacman")
  }

  # Read the DESCRIPTION file
  description <- desc::desc(file = "DESCRIPTION")

  # Get dependencies
  dependencies <- description$get_deps()$package %>% setdiff("R")

  # Check for missing dependencies and install
  for (dep in dependencies) {
    if (!pacman::p_loadable(dep)) {
      install.packages("dep")
    }
  }

  return("packages installed.")
}
