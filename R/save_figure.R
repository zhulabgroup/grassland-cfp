save_figure <- function(out, dir = "alldata/output/figures/") {
  for (i in 1:length(out)) {
    gg_name <- names(out)[i]
    param <- save_figure_param(gg_name)
    filename <- str_c(dir, "/", param$type, "/", param$filename)
    ggsave(
      plot = out[[i]],
      filename = filename,
      width = param$width,
      height = param$height,
      device = png, type = "cairo"
    )
    file.copy(from = filename, to = str_c("inst/figures/", param$type, "/"), recursive = T)
  }
}

save_figure_param <- function(gg_name) {
  if (str_detect(gg_name, "niche")) {
    type <- "main"
    width <- 8
    height <- 8 * 1.618
    filename <- "fig-main-niche.png"
  }

  if (str_detect(gg_name, "community_index_obs")) {
    type <- "main"
    width <- 11
    height <- 11 * 1.5
    filename <- "fig-main-obs.png"
  }

  if (str_detect(gg_name, "community_index_exp")) {
    type <- "main"
    width <- 10
    height <- 6.18
    filename <- "fig-main-exp.png"
  }

  if (str_detect(gg_name, "community_shift")) {
    type <- "main"
    width <- 9
    height <- 9 * 1.618
    filename <- "fig-main-shift.png"
  }

  if (str_detect(gg_name, "gg_species_change")) {
    type <- "main"
    width <- 11
    height <- 11.5
    filename <- "fig-main-gainloss.png"
  }

  out <- list(type = type, width = width, height = height, filename = filename)
  return(out)
}
