# group -------------------------------------------------------------------

tidy_obs <- function(
    in_dir = "input/community/raw/",
    out_dir = "intermediate/observation-experiment/tidy-community/") {
  tidy_angelo(in_dir) %>% write_rds(str_c(out_dir, "angelo.rds"))
}

# observation -------------------------------------------------------------

tidy_angelo <- function(base_dir) {
  # community data
  com_tbl <- base_dir %>%
    str_c("Angelo/Angelo_CommCompData.csv") %>%
    read_csv(col_types = cols(.default = "d", Year = "i", Plot = "i", TMT = "c")) %>%
    filter(TMT == "C") %>%
    pivot_longer(-c(Year, Plot, TMT), names_to = "species", values_to = "cover") %>%
    rename_with(tolower)

  # species data
  spp_tbl <- base_dir %>%
    str_c("Angelo/Angelo_spp_guilds.csv") %>%
    read_csv(col_types = "c")

  # combine
  angelo_tbl <- com_tbl %>%
    left_join(spp_tbl, by = "species") %>%
    select("tmt", "plot", "year", "species.name", "cover", "guild") %>%
    filter(
      cover > 0,
      guild != "Bare",
      guild != "Moss",
      guild != "Litter"
    ) %>%
    mutate(site = "angelo", species = str_trim(species.name), abund_type = "cover") %>%
    select(site, year, plot, species, guild, abund = cover, abund_type) %>%
    arrange(site, year, plot, species)

  return(angelo_tbl)
}
