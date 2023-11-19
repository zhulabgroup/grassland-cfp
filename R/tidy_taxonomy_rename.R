#' @export
tidy_taxonomy_rename <- function(path_community_tidy = "alldata/intermediate/observation-experiment/tidy-community/",
                                 path_community_final = "alldata/intermediate/observation-experiment/final-community/",
                                 misspelling_file = "alldata/input/community/species/misspelling.csv") {
  # import misspelling table prepared Josie and Justin
  misspelling_tbl <- misspelling_file %>%
    read_csv(col_types = "cccclcd")

  sites <- list.files(path_community_tidy) %>% str_remove(".csv")
  for (s in sites) {
    if (s %in% c(
      "angelo", "carrizo", "elkhorn", "jasper", "mclann", "mclserp", "morganterritory",
      "pleasantonridge", "sunol", "swanton", "ucsc", "vascocaves"
    )) {
      read_csv(list.files(path_community_tidy, pattern = s, full.names = T),
        col_types = "cicccdc"
      ) %>%
        rename(
          "original_species" = "species",
          "original_guild" = "guild"
        ) %>%
        left_join(
          misspelling_tbl,
          by = c("original_species", "original_guild")
        ) %>%
        filter(keep) %>%
        group_by(site, year, plot, corrected_species, corrected_guild, abund_type) %>%
        summarize(abund = sum(abund)) %>% # combine abundances for same species with old and new names
        ungroup() %>%
        select(
          site, year, plot,
          species = corrected_species, guild = corrected_guild,
          abund, abund_type
        ) %>%
        arrange(site, year, plot, species) %>%
        write_csv(str_c(path_community_final, s, ".csv"))
    }
    if (s %in% c("jrgce", "mclexp", "scide")) {
      read_csv(list.files(path_community_tidy, pattern = s, full.names = T),
        col_types = "ciccccdc"
      ) %>%
        rename(
          "original_species" = "species",
          "original_guild" = "guild"
        ) %>%
        left_join(
          misspelling_tbl,
          by = c("original_species", "original_guild")
        ) %>%
        filter(keep) %>%
        group_by(site, year, plot, treat, corrected_species, corrected_guild, abund_type) %>%
        summarize(abund = sum(abund)) %>% # combine abundances for same species with old and new names
        ungroup() %>%
        select(
          site, year, plot, treat,
          species = corrected_species, guild = corrected_guild,
          abund, abund_type
        ) %>%
        arrange(site, year, plot, species) %>%
        write_csv(str_c(path_community_final, s, ".csv"))
    }
  }

  return(path_community_final)
}
