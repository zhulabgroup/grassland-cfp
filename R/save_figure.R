save_figure <- function(outdir = "alldata/output/figures/") {
  save_figure_main(outdir = outdir)
  save_figure_supp(outdir = outdir)

  file.copy(from = outdir, to = "inst/figures/", recursive = T)
}

save_figure_main <- function(outdir) {
  outdir <- str_c(outdir, "/main/")
  ggsave(
    plot = gg_niche$combined,
    filename = str_c(outdir, "fig-main-niche.png"),
    width = 8,
    height = 8 * 1.618,
    device = png, type = "cairo"
  )

  ggsave(
    plot = gg_community_index_obs,
    filename = str_c(outdir, "fig-main-obs.png"),
    width = 11,
    height = 11 * 1.5,
    device = png, type = "cairo"
  )

  ggsave(
    plot = gg_community_index_exp,
    filename = str_c(outdir, "fig-main-exp.png"),
    width = 10,
    height = 6.18,
    device = png, type = "cairo"
  )

  ggsave(
    plot = gg_community_shift$combined,
    filename = str_c(outdir, "fig-main-shift.png"),
    width = 9,
    height = 9 * 1.618,
    device = png, type = "cairo"
  )

  ggsave(
    plot = gg_species_change$combined,
    filename = str_c(outdir, "fig-main-gainloss.png"),
    width = 11,
    height = 11.5,
    device = png, type = "cairo"
  )
}

save_figure_supp <- function(outdir) {
  outdir <- str_c(outdir, "/supp/")
}
