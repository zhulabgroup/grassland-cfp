# read in raw data
# note 2020 data is missing.
cover_2015_tbl <- readxl::read_excel("data/community/raw/HarrisonExperiment/2015-2021 Water Experiment Data.xlsx", sheet = "2015")
cover_2016_tbl <- readxl::read_excel("data/community/raw/HarrisonExperiment/2015-2021 Water Experiment Data.xlsx", sheet = "2016")
cover_2017_tbl <- readxl::read_excel("data/community/raw/HarrisonExperiment/2015-2021 Water Experiment Data.xlsx", sheet = "2017")
cover_2018_tbl <- readxl::read_excel("data/community/raw/HarrisonExperiment/2015-2021 Water Experiment Data.xlsx", sheet = "2018")
cover_2019_tbl <- readxl::read_excel("data/community/raw/HarrisonExperiment/2015-2021 Water Experiment Data.xlsx", sheet = "2019")
# cover_2020_tbl <- readxl::read_excel("data/community/raw/HarrisonExperiment/2015-2021 Water Experiment Data.xlsx", sheet = "2020")
cover_2021_tbl <- readxl::read_excel("data/community/raw/HarrisonExperiment/2015-2021 Water Experiment Data.xlsx", sheet = "2021")
plot_tbl <- readxl::read_excel("data/community/raw/HarrisonExperiment/2015-2021 Water Experiment Data.xlsx", sheet = "Plot codes")

# combine cover data across years and join with plot-treatment data.
cover_long_tbl <- (
  cover_2015_tbl %>%
    rename(species = "Plots") %>%
    pivot_longer(-species, names_to = "plot", values_to = "abund") %>%
    mutate(year = 2015)
) %>%
  bind_rows(
    cover_2016_tbl %>%
      rename(species = "Plots") %>%
      pivot_longer(-species, names_to = "plot", values_to = "abund") %>%
      mutate(year = 2016)
  ) %>%
  bind_rows(
    cover_2017_tbl %>%
      rename(species = "Plots") %>%
      pivot_longer(-species, names_to = "plot", values_to = "abund") %>%
      mutate(year = 2017)
  ) %>%
  bind_rows(
    cover_2018_tbl %>%
      rename(species = "Plots") %>%
      pivot_longer(-species, names_to = "plot", values_to = "abund") %>%
      mutate(year = 2018)
  ) %>%
  bind_rows(
    cover_2019_tbl %>%
      rename(species = "Plots") %>%
      pivot_longer(-species, names_to = "plot", values_to = "abund") %>%
      mutate(year = 2019)
  ) %>%
  bind_rows(
    cover_2021_tbl %>%
      rename(species = "Plots") %>%
      pivot_longer(-species, names_to = "plot", values_to = "abund") %>%
      mutate(year = 2021)
  ) %>%
  select(year, plot, species, abund) %>%
  mutate(plot = as.integer(plot))

mclexp_data <- plot_tbl %>%
  mutate(
    plot = as.integer(Site),
    treat = case_when(
      Trt == "W" ~ "WX", # watering; X means not available; watering is paired--compare WX vs. _X, not X_
      Trt == "WC" ~ "_X", # watering control (_)
      Trt == "S" ~ "XD", # shelter -> drought
      Trt == "SC" ~ "X_" # shelter control -> drought control
    ) %>%
      str_c(Soil) # S = serpentine soil, N = non-serpentine soil
  ) %>%
  select(plot, treat) %>%
  full_join(cover_long_tbl, by = "plot") %>%
  mutate(site = "mclexp", guild = NA, abund_type = "cover") %>%
  select(site, year, plot, treat, species, guild, abund, abund_type) %>%
  filter(
    abund > 0,
    !(species %in% c("bare", "gopher", "litter", "mosses", "rock", "soil"))
  ) %>%
  arrange(year, plot, species)
