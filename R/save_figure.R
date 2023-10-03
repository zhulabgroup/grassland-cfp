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
  if (gg_name == "niche") {
    type <- "main"
    width <- 8
    height <- 8 * 1.618
    filename <- "fig-main-niche.png"
  }

  if (gg_name == "community_index_obs") {
    type <- "main"
    width <- 11
    height <- 11 * 1.5
    filename <- "fig-main-obs.png"
  }

  if (gg_name == "community_index_exp") {
    type <- "main"
    width <- 10
    height <- 6.18
    filename <- "fig-main-exp.png"
  }

  if (gg_name == "community_shift") {
    type <- "main"
    width <- 9
    height <- 9 * 1.618
    filename <- "fig-main-shift.png"
  }

  if (gg_name == "species_change") {
    type <- "main"
    width <- 11
    height <- 11.5
    filename <- "fig-main-gainloss.png"
  }

  if (gg_name == "cfp_cc") {
    type <- "supp"
    width <- 9
    height <- 9 * .618
    filename <- "fig-supp-cfp-cc.png"
  }

  if (gg_name == "data_avail") {
    type <- "supp"
    width <- 10
    height <- 10 * .618
    filename <- "fig-supp-data-avail.png"
  }

  if (gg_name == "site_cc") {
    type <- "supp"
    width <- 11
    height <- 11 * 1.5
    filename <- "fig-supp-site-cc.png"
  }

  if (gg_name == "niche_cool") {
    type <- "supp"
    width <- 8
    height <- 8 * .618
    filename <- "fig-supp-niche-cool.png"
  }

  if (gg_name == "niche_warm") {
    type <- "supp"
    width <- 8
    height <- 8 * .618
    filename <- "fig-supp-niche-warm.png"
  }

  if (gg_name == "species_change_obs") {
    type <- "supp"
    width <- 10
    height <- 8
    filename <- "fig-supp-gainloss-obs.png"
  }

  if (gg_name == "species_change_exp") {
    type <- "supp"
    width <- 12
    height <- 8
    filename <- "fig-supp-gainloss-exp.png"
  }

  if (gg_name == "biogeography") {
    type <- "supp"
    width <- 10
    height <- 5
    filename <- "fig-supp-biogeography.png"
  }

  if (gg_name == "community_index_exp_jrgce_water") {
    type <- "supp"
    width <- 10
    height <- 6.18
    filename <- "fig-supp-exp-jrgce-water.png"
  }

  if (gg_name == "community_index_exp_mclexp") {
    type <- "supp"
    width <- 11
    height <- 8
    filename <- "fig-supp-exp-mclexp.png"
  }

  if (gg_name == "community_index_exp_scide") {
    type <- "supp"
    width <- 11
    height <- 8
    filename <- "fig-supp-exp-scide.png"
  }

  if (gg_name == "guild_niche") {
    type <- "supp"
    width <- 12
    height <- 5
    filename <- "fig-supp-guild-niche.png"
  }

  if (gg_name == "guild_perc_obs") {
    type <- "supp"
    width <- 11
    height <- 11 * 1.5
    filename <- "fig-supp-guild-perc-obs.png"
  }

  if (gg_name == "guild_perc_exp") {
    type <- "supp"
    width <- 10
    height <- 6.18 * 1.5
    filename <- "fig-supp-guild-perc-exp.png"
  }

  if (gg_name == "bioclim_tmp") {
    type <- "supp"
    width <- 7
    height <- 7
    filename <- "fig-supp-bioclim-tmp.png"
  }

  if (gg_name == "bioclim_ppt") {
    type <- "supp"
    width <- 7
    height <- 7
    filename <- "fig-supp-bioclim-ppt.png"
  }

  if (gg_name == "niche_stat_tmp") {
    type <- "supp"
    width <- 7
    height <- 7
    filename <- "fig-supp-niche-stat-tmp.png"
  }

  if (gg_name == "niche_stat_ppt") {
    type <- "supp"
    width <- 7
    height <- 7
    filename <- "fig-supp-niche-stat-ppt.png"
  }

  if (gg_name == "niche_thin") {
    type <- "supp"
    width <- 10
    height <- 5
    filename <- "fig-supp-niche-thin.png"
  }

  if (gg_name == "niche_thin_clim") {
    type <- "supp"
    width <- 10
    height <- 5
    filename <- "fig-supp-niche-thin-clim.png"
  }

  if (gg_name == "rank_abund_obs") {
    type <- "supp"
    width <- 10
    height <- 8
    filename <- "fig-supp-rank-abund-obs.png"
  }

  if (gg_name == "rank_abund_exp") {
    type <- "supp"
    width <- 12
    height <- 8
    filename <- "fig-supp-rank-abund-exp.png"
  }

  if (gg_name == "trait_change_obs") {
    type <- "supp"
    width <- 11
    height <- 11 * 1.5
    filename <- "fig-supp-trait-change-obs.png"
  }

  if (gg_name == "trait_change_exp") {
    type <- "supp"
    width <- 10
    height <- 6.18
    filename <- "fig-supp-trait-change-exp.png"
  }

  if (gg_name == "clim_source") {
    type <- "supp"
    width <- 10
    height <- 10
    filename <- "fig-supp-clim-source.png"
  }

  if (gg_name == "trait_cwd") {
    type <- "supp"
    width <- 12
    height <- 5
    filename <- "fig-supp-trait-cwd.png"
  }

  if (gg_name == "community_index_obs_cwd") {
    type <- "supp"
    width <- 11
    height <- 11 * 1.5
    filename <- "fig-supp-obs-cwd.png"
  }

  if (gg_name == "community_index_exp_cwd") {
    type <- "supp"
    width <- 10
    height <- 6.18 * 1.5
    filename <- "fig-supp-exp-cwd.png"
  }

  out <- list(type = type, width = width, height = height, filename = filename)
  return(out)
}
