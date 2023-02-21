# multi-page plot for all species niches, one page per species, in supplements
source("scripts/plot-ind-sp-niche.R")

niche_gg <- vector(mode = "list")
for (i in seq_along(sp_gbif_vec)) {
  niche_gg[[i]] <- sp_gbif_vec[i] %>%
    plot_sp_niche() # no need to print(); will slow down
}
names(niche_gg) <- sp_gbif_vec

# runtime ~= 4 min
pdf(.path$out_fig_niche, width = 8, height = 8 * .618)
print(niche_gg)
dev.off()
