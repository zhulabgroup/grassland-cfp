tidy_all <- function(
    indir = "input/community/raw/",
    outdir = "intermediate/observation-experiment/tidy-community/") {
  # observational sites
  tidy_angelo(indir) %>% write_csv(str_c(outdir, "angelo.csv"))
  tidy_carrizo(indir) %>% write_csv(str_c(outdir, "carrizo.csv"))
  tidy_eastbay(indir, "pleasantonridge") %>% write_csv(str_c(outdir, "pleasantonridge.csv"))
  tidy_eastbay(indir, "sunol") %>% write_csv(str_c(outdir, "sunol.csv"))
  tidy_eastbay(indir, "vascocaves") %>% write_csv(str_c(outdir, "vascocaves.csv"))
  tidy_elkhorn(indir) %>% write_csv(str_c(outdir, "elkhorn.csv"))
  tidy_swanton(indir) %>% write_csv(str_c(outdir, "swanton.csv"))
  tidy_ucsc(indir) %>% write_csv(str_c(outdir, "ucsc.csv"))
  tidy_jasper(indir) %>% write_csv(str_c(outdir, "jasper.csv"))
  tidy_morganterritory(indir) %>% write_csv(str_c(outdir, "morganterritory.csv"))
  # experimental sites
  tidy_jrgce(indir) %>% write_csv(str_c(outdir, "jrgce.csv"))
  tidy_mclexp(indir) %>% write_csv(str_c(outdir, "mclexp.csv"))
}

# observation -------------------------------------------------------------

tidy_angelo <- function(basedir) {
  # community data
  com_tbl <- basedir %>%
    str_c("Angelo/Angelo_CommCompData.csv") %>%
    read_csv(col_types = cols(.default = "d", Year = "i", Plot = "i", TMT = "c")) %>%
    filter(TMT == "C") %>%
    pivot_longer(-c(Year, Plot, TMT), names_to = "species", values_to = "cover") %>%
    rename_with(tolower)

  # species data
  spp_tbl <- basedir %>%
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

tidy_carrizo <- function(basedir) {
  # community data spreadsheet
  com_tbl <- basedir %>%
    str_c("Carrizo/2022-06-28/Carrizo_data for Joise_2007_2021.xlsx") %>%
    readxl::read_xlsx(
      sheet = 1,
      col_types = c(
        "date", "numeric", "text", "text", "text",
        "numeric", "text", "text", "text", "text",
        "text", "numeric", "text", "text", "text",
        "text", "text", "text", "text", "numeric",
        "numeric", "text"
      ),
      na = "NA"
    ) %>%
    filter(cattle == "Ungrazed") %>% # only ungrazed plots
    select(
      plot = Site, year,
      species_code = SpeciesCode, species_name = Species.Name,
      intercept = Count2 # Updated number of hits, where any species that was present but not hit were given a 1
    ) %>%
    filter(intercept > 0, !is.na(intercept))

  # species data
  spp_tbl <- basedir %>%
    str_c("Carrizo/2022-07-05/Carrizo_global_species list_v2.csv") %>%
    read_csv(col_types = "c") %>%
    mutate(guild = case_when(
      newform == "ia" ~ "EAG", # Invasive (Exotic) Annual Grass
      newform == "ih" ~ "EAF", # Invasive (Exotic) Annual Forb
      newform == "ip" ~ "EPF", # Invasive (Exotic) Perennial Forb
      newform == "na" ~ "NAG", # Native Annual Grass
      newform == "nh" ~ "NAF", # Native Annual Forb
      newform == "np" ~ "NPF", # Native	Perennial	Forb
      newform == "npg" ~ "NPG", # Native Perennial Grass
      newform == "nps" ~ "NPS" # Native Perennial Shrub
    ))

  # join community and species data
  carrizo_tbl <- com_tbl %>%
    filter(
      plot %in% c("EP2", "EP3", "EP4", "EP6", "SC1", "SC2", "SC7", "SC9"),
      # !(species_code == "AMSMEN" & is.na(species_name)),
      !(species_code == "ASTspp" & is.na(species_name)),
      !(species_code == "BARE" & species_name == "Bare ground"),
      !(species_code == "BURROW" & species_name == "Animal burrow"),
      !(species_code == "CRYPTO" & is.na(species_name)),
      # !(species_code == "EUPPOL" & is.na(species_name)),
      !(species_code == "FRESHDIRT" & species_name == "freshly disturbed dirt"),
      !(species_code == "GOPHER" & species_name == "gopher mound"),
      !(species_code == "HOLE" & species_name == "hole in ground"),
      !(species_code == "LITTER" & species_name == "litter"),
      !(species_code == "MOSS" & is.na(species_name)),
      !(species_code == "UNK 11" & is.na(species_name))
    ) %>%
    left_join(spp_tbl %>% select(species_code = spp.code, species_name = SCIENTIFIC.NAME, guild),
      by = c("species_code", "species_name")
    ) %>%
    mutate(species_name = case_when(
      species_code == "AMSMEN" ~ "Amsinckia menziesii",
      species_code == "EUPPOL" ~ "Chamaesyce polycarpa (Euphorbia polycarpa)",
      TRUE ~ species_name
    )) %>%
    group_by(plot, year, species_code, species_name, guild) %>%
    summarize(abund = sum(intercept)) %>%
    ungroup() %>%
    mutate(site = "carrizo", species_name = str_trim(species_name), abund_type = "point_intercept") %>%
    select(site, year, plot, species = species_name, guild, abund, abund_type) %>%
    arrange(site, year, plot, species)

  return(carrizo_tbl)
}

tidy_eastbay <- function(basedir, site) {
  # community data
  com_tbl <- basedir %>%
    str_c("Dudney/EBRPD_2002thru2012_Dec2013_BRXX AVXX updated.csv") %>%
    read_csv(col_types = "cicidc") %>%
    rename(plot = plot.ID, species_code = species) %>%
    group_by(site, year, plot, species_code) %>%
    summarize(hits = n()) %>%
    group_by(site, year, plot) %>%
    mutate(tot_hits = sum(hits)) %>%
    group_by(site, year, plot, species_code) %>%
    summarize(abund = hits / tot_hits)

  # species data
  spp_tbl <- basedir %>%
    str_c("Dudney/_VegSpCodeAttributes_2010.xlsx") %>%
    readxl::read_xlsx() %>%
    mutate(
      species_code = toupper(species),
      guild = str_c(
        case_when(
          `native/exotic` == "e" ~ "E", # Exotic
          `native/exotic` == "n" ~ "N", # Native
          TRUE ~ "U" # else is Unknown
        ),
        case_when(
          `annual/perennial` == "a" ~ "A", # Annual
          `annual/perennial` == "p" ~ "P", # Perennial
          TRUE ~ "U" # else is Unknown
        ),
        case_when(
          `forb/grass` == "f" ~ "F", # Forb
          `forb/grass` == "g" ~ "G", # Grass
          `forb/grass` == "n" ~ "R", # Rush
          `forb/grass` == "s" ~ "S", # Shurb
          `forb/grass` == "t" ~ "T", # Tree
          TRUE ~ "U" # else is Unknown
        )
      )
    ) %>%
    select(species_code, species = latin, guild)

  # combine
  eastbay_tbl <- com_tbl %>%
    left_join(spp_tbl, by = "species_code") %>%
    mutate(abund_type = "point_intercept") %>%
    mutate(species = coalesce(species, species_code)) %>% # if species == NA, replace with species_code
    filter(
      !(species %in% c("bare", "COWPIE", "gopher", "litter", "mosses", "rock", "soil")),
      !str_detect(species, "unknown"),
      is.na(guild) | guild != "UUU"
    ) %>%
    select(site, year, plot, species, guild, abund, abund_type) %>%
    arrange(site, year, plot, species)

  # return site-specific data
  if (site == "pleasantonridge") {
    return(eastbay_tbl %>%
      filter(site == "PR", plot %in% str_c("PR", 4:9)) %>%
      mutate(site = "pleasantonridge"))
  } else if (site == "sunol") {
    return(eastbay_tbl %>%
      filter(site == "SU", plot %in% str_c("SU", 1:9)) %>%
      mutate(site = "sunol"))
  } else if (site == "vascocaves") {
    return(eastbay_tbl %>%
      filter(site == "VC", plot %in% str_c("VC", 1:10)) %>%
      filter(!(year == 2012 & plot %in% c("VC1", "VC8", "VC9"))) %>%
      mutate(site = "vascocaves"))
  } else {
    stop("incorrect site")
  }
}

utils_santacruz <- function(target) {
  clean_com <- function(data, gathercol1, gathercol2, site = c("elk", "swa", "ucsc")) {
    filtersite <- match.arg(site, c("elk", "swa", "ucsc"))
    keep_cols <- c("site", "freq", "treat", "rep", "species.code", "intercepts", "plot")
    if (!(gathercol1 %in% names(data)) | !(gathercol2 %in% names(data))) {
      stop("gathercol1 and gathercol2 must be columns in data.")
    }
    ind_col1 <- which(names(data) == gathercol1)
    ind_col2 <- which(names(data) == gathercol2)
    if (ind_col1 > ind_col2) stop("gathercol1 must come before gathercol2 in the column names of the data.")
    columns <- names(data)[ind_col1:ind_col2]
    data <- suppressWarnings(mutate(data, across(all_of(columns), function(x) as.numeric(x))))
    data %>%
      mutate(
        freq = tolower(freq),
        freq = if_else(freq == "control", "con", freq),
        site = tolower(site),
        site = if_else(site == "swan", "swa", site),
        site = if_else(site == "swanton", "swa", site)
      ) %>%
      filter(
        site == filtersite,
        freq == "con"
      ) %>%
      pivot_longer(all_of(columns), names_to = "species.code", values_to = "intercepts") %>%
      mutate(
        intercepts = as.numeric(intercepts),
        intercepts = if_else(is.na(intercepts), 0, intercepts),
        species.code = tolower(species.code)
      ) %>%
      select(any_of(keep_cols)) %>%
      return()
  }

  clean_plt <- function(data) {
    data %>%
      pivot_wider(names_from = "plot", values_from = "intercepts", names_prefix = "plot") %>%
      mutate(intercepts = (plot1 + plot2 + plot3 + plot4)) %>%
      select(-plot1, -plot2, -plot3, -plot4)
  }

  if (target == "community") {
    return(clean_com)
  } else if (target == "plot") {
    return(clean_plt)
  } else {
    stop("incorrect target")
  }
}

tidy_elkhorn <- function(basedir) {
  # set clean functions
  clean_com <- utils_santacruz("community")
  clean_plt <- utils_santacruz("plot")

  # import data
  elkspp <- basedir %>%
    str_c("HollData/ElkhornSpecies.csv") %>%
    read_csv() %>%
    mutate(species.code = tolower(species.code))
  elk_guilds <- basedir %>%
    str_c("HollData/ElkhornGuilds.csv") %>%
    read_csv()
  elk_counts <- list()
  for (i in 1:20) {
    year <- as.character(1999:2018)[i]
    skips <- c(4, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 2, 1, 1, 1, 1, 1)
    filename <- str_c(basedir, "HollData/Elk_", year, ".csv")
    out <- read_csv(filename, skip = skips[i], show_col_types = FALSE)
    # elk_counts[[year]] <- select_if(out, has_data)
    elk_counts[[year]] <- out
  }
  elk_counts <- lapply(seq_along(elk_counts), function(i) {
    rename_with(elk_counts[[i]], tolower) %>%
      rename_with(~ sub("frequency", "freq", sub("treatment", "treat", sub("replicate", "rep", sub("9$", "", .x))))) %>%
      select(-starts_with("comment"), -starts_with("..."), -starts_with("0"))
  })

  # Because these data are in separate files (one year per file), we need to clean the data and compile all of the years into a single object. To clean the data, we selected only the Elkhorn plots (these files also contained the data from two other sites; UCSC and Swanton, removed the non-control plots, restructured the data to make sure only non-zero datapoints were included, and added the year. We then combined the file with species guild information to each yearly object.
  elk_counts_clean <- list()
  elk_counts_clean[[1]] <- elk_counts[[1]] %>% clean_com("anaarv", "bare")
  elk_counts_clean[[2]] <- elk_counts[[2]] %>%
    clean_com("aircar", "naspul") %>%
    clean_plt()
  elk_counts_clean[[3]] <- elk_counts[[3]] %>%
    clean_com("anaarv", "thatch") %>%
    clean_plt()
  elk_counts_clean[[4]] <- elk_counts[[4]] %>% clean_com("anaarv", "thatch")
  elk_counts_clean[[5]] <- elk_counts[[5]] %>% clean_com("aircar", "vulpsp")
  elk_counts_clean[[6]] <- elk_counts[[6]] %>% clean_com("aircar", "vulpsp")
  elk_counts_clean[[7]] <- elk_counts[[7]] %>% clean_com("aircar", "vulpsp")
  elk_counts_clean[[8]] <- elk_counts[[8]] %>% clean_com("aircar", "vulpsp")
  elk_counts_clean[[9]] <- elk_counts[[9]] %>% clean_com("aircar", "vulpsp")
  elk_counts_clean[[10]] <- elk_counts[[10]] %>% clean_com("aircar", "hormur")
  elk_counts_clean[[11]] <- elk_counts[[11]] %>% clean_com("aircar", "vulpsp")
  elk_counts_clean[[12]] <- elk_counts[[12]] %>% clean_com("aircar", "vulpsp")
  elk_counts_clean[[13]] <- elk_counts[[13]] %>% clean_com("aircar", "vulpsp")
  elk_counts_clean[[14]] <- elk_counts[[14]] %>% clean_com("aircar", "vulpsp")
  elk_counts_clean[[15]] <- elk_counts[[15]] %>% clean_com("aircar", "vulpsp")
  elk_counts_clean[[16]] <- elk_counts[[16]] %>% clean_com("aircar", "vulpsp")
  elk_counts_clean[[17]] <- elk_counts[[17]] %>% clean_com("aircar", "vulpsp")
  elk_counts_clean[[18]] <- elk_counts[[18]] %>% clean_com("aircar", "vulpia")
  elk_counts_clean[[19]] <- elk_counts[[19]] %>% clean_com("aircar", "vulpia")
  elk_counts_clean[[20]] <- elk_counts[[20]] %>% clean_com("aircar", "vulpia")

  # lapply(elk_counts_clean, dim)
  for (i in 1:length(elk_counts_clean)) {
    elk_counts_clean[[i]]$year <- (1999:2018)[i]
  }

  # At this point, all that is left to do is to combine the objects so that we have one large object with each year of data in it.

  elk_counts_df <- do.call(rbind, elk_counts_clean) %>%
    left_join(elkspp, by = "species.code") %>%
    filter(intercepts > 0) %>%
    select("rep", "year", "species.name", "intercepts", "guild") %>%
    mutate(
      site = "elkhorn",
      species.name = factor(species.name)
    )
  # removing unwanted guilds (moss, litter, etc.)
  elk_counts_df <- elk_counts_df %>%
    group_by(rep, year, guild, species.name) %>%
    summarize(intercepts = sum(intercepts)) %>%
    filter(
      guild != "Moss",
      guild != "Thatch",
      guild != "Bare"
    )

  elk_guilds <- elk_counts_df %>%
    group_by(rep, year, guild) %>%
    summarize(guildtotal = sum(intercepts)) %>%
    ungroup() %>%
    group_by(year, guild) %>%
    summarize(mean.intercepts = mean(guildtotal))

  elkhorn_tbl <- elk_counts_df %>%
    filter(
      species.name != "Moss",
      species.name != "Thatch",
      species.name != "Bare ground"
    ) %>%
    mutate(site = "elkhorn")

  elkhorn_tbl <- elkhorn_tbl %>%
    as_tibble() %>%
    select(site, year, plot = rep, species = species.name, guild, abund = intercepts) %>%
    mutate(year = as.integer(year), species = as.character(species) %>% str_trim(), guild = as.character(guild), abund = as.integer(abund), abund_type = "point_intercept") %>%
    arrange(site, year, plot, species)

  return(elkhorn_tbl)
}

tidy_swanton <- function(basedir) {
  # set clean functions
  clean_com <- utils_santacruz("community")
  clean_plt <- utils_santacruz("plot")

  # import data
  Swanspp <- basedir %>%
    str_c("HollData/ElkhornSpecies.csv") %>%
    read_csv() %>%
    mutate(species.code = tolower(species.code))
  Swan_guilds <- basedir %>%
    str_c("HollData/ElkhornGuilds.csv") %>%
    read_csv()
  swan_counts <- list()
  for (i in 1:14) {
    year <- as.character(1999:2012)[i]
    skips <- c(4, 3, 3, 3, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0)
    if (i != 10) {
      filename <- str_c(basedir, "HollData/Elk_", year, ".csv")
    } else {
      filename <- str_c(basedir, "HollData/swanton_08.csv")
    }
    out <- read_csv(filename, skip = skips[i], show_col_types = FALSE)
    swan_counts[[year]] <- out
  }
  swan_counts <- lapply(seq_along(swan_counts), function(i) {
    rename_with(swan_counts[[i]], tolower) %>%
      rename_with(~ sub("frequency", "freq", sub("treatment", "treat", sub("replicate", "rep", sub("9$", "", .x))))) %>%
      select(-starts_with("comment"), -starts_with("..."), -starts_with("0"))
  })

  # clean data
  swan_counts_clean <- list()
  swan_counts_clean[[1]] <- swan_counts[[1]] %>% clean_com("anaarv", "bare", site = "swa")
  swan_counts_clean[[2]] <- swan_counts[[2]] %>%
    clean_com("aircar", "naspul", site = "swa") %>%
    clean_plt()
  swan_counts_clean[[3]] <- swan_counts[[3]] %>%
    clean_com("anaarv", "thatch", site = "swa") %>%
    clean_plt()
  swan_counts_clean[[4]] <- swan_counts[[4]] %>% clean_com("anaarv", "thatch", site = "swa")
  swan_counts_clean[[5]] <- swan_counts[[5]] %>% clean_com("aircar", "vulpsp", site = "swa")
  swan_counts_clean[[6]] <- swan_counts[[6]] %>% clean_com("aircar", "vulpsp", site = "swa")
  swan_counts_clean[[7]] <- swan_counts[[7]] %>% clean_com("aircar", "vulpsp", site = "swa")
  swan_counts_clean[[8]] <- swan_counts[[8]] %>% clean_com("aircar", "vulpsp", site = "swa")
  swan_counts_clean[[9]] <- swan_counts[[9]] %>% clean_com("aircar", "vulpsp", site = "swa")
  swan_counts_clean[[10]] <- swan_counts[[10]] %>% clean_com("aircar", "spearv", site = "swa")
  swan_counts_clean[[11]] <- swan_counts[[11]] %>% clean_com("aircar", "vulpsp", site = "swa")
  swan_counts_clean[[12]] <- swan_counts[[12]] %>% clean_com("aircar", "vulpsp", site = "swa")
  swan_counts_clean[[13]] <- swan_counts[[13]] %>% clean_com("aircar", "vulpsp", site = "swa")
  swan_counts_clean[[14]] <- swan_counts[[14]] %>% clean_com("aircar", "vulpsp", site = "swa")

  # lapply(swan_counts_clean, dim)
  for (i in 1:length(swan_counts_clean)) {
    swan_counts_clean[[i]]$year <- (1999:2012)[i]
  }

  swan_counts_df <- do.call(rbind, swan_counts_clean) %>%
    left_join(Swanspp, by = "species.code") %>%
    filter(intercepts > 0) %>%
    select("rep", "year", "species.name", "intercepts", "guild") %>%
    mutate(
      site = "swanton",
      species.name = factor(species.name)
    )
  # removing unwanted guilds (moss, litter, etc.)
  swan_counts_df <- swan_counts_df %>%
    group_by(rep, year, guild, species.name) %>%
    summarize(intercepts = sum(intercepts)) %>%
    filter(
      guild != "Moss",
      guild != "Thatch",
      guild != "Bare"
    )

  elk_guilds <- swan_counts_df %>%
    group_by(rep, year, guild) %>%
    summarize(guildtotal = sum(intercepts)) %>%
    ungroup() %>%
    group_by(year, guild) %>%
    summarize(mean.intercepts = mean(guildtotal))

  swanton_tbl <- swan_counts_df %>%
    filter(
      species.name != "Moss",
      species.name != "Thatch",
      species.name != "Bare ground"
    ) %>%
    mutate(site = "swanton")

  swanton_tbl <- swanton_tbl %>%
    as_tibble() %>%
    select(site, year, plot = rep, species = species.name, guild, abund = intercepts) %>%
    mutate(year = as.integer(year), species = as.character(species) %>% str_trim(), guild = as.character(guild), abund = as.integer(abund), abund_type = "point_intercept") %>%
    arrange(site, year, plot, species)

  return(swanton_tbl)
}

tidy_ucsc <- function(basedir) {
  # set clean functions
  clean_com <- utils_santacruz("community")
  clean_plt <- utils_santacruz("plot")

  # import data
  UCSCspp <- basedir %>%
    str_c("HollData/ElkhornSpecies.csv") %>%
    read_csv() %>%
    mutate(species.code = tolower(species.code))
  UCSC_guilds <- basedir %>%
    str_c("HollData/ElkhornGuilds.csv") %>%
    read_csv()
  ucsc_counts <- list()
  for (i in 1:14) {
    year <- as.character(1999:2012)[i]
    skips <- c(4, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0)
    if (i != 10) {
      filename <- str_c(basedir, "HollData/Elk_", year, ".csv")
    } else {
      filename <- str_c(basedir, "HollData/ucsc_08.csv")
    }
    out <- read_csv(filename, skip = skips[i], show_col_types = FALSE)
    ucsc_counts[[year]] <- out
  }
  ucsc_counts <- lapply(seq_along(ucsc_counts), function(i) {
    rename_with(ucsc_counts[[i]], tolower) %>%
      rename_with(~ sub("frequency", "freq", sub("treatment", "treat", sub("replicate", "rep", sub("9$", "", .x))))) %>%
      select(-starts_with("comment"), -starts_with("..."), -starts_with("0"))
  })

  # lapply(ucsc_counts, names)

  # clean data
  # After creating objects for each year, we will clean each to a compatible structure.
  ucsc_counts_clean <- list()
  ucsc_counts_clean[[1]] <- ucsc_counts[[1]] %>% clean_com("anaarv", "bare", site = "ucsc")
  ucsc_counts_clean[[2]] <- ucsc_counts[[2]] %>%
    clean_com("aircar", "naspul", site = "ucsc") %>%
    clean_plt()
  ucsc_counts_clean[[3]] <- ucsc_counts[[3]] %>%
    clean_com("anaarv", "thatch", site = "ucsc") %>%
    clean_plt()
  ucsc_counts_clean[[4]] <- ucsc_counts[[4]] %>% clean_com("anaarv", "thatch", site = "ucsc")
  ucsc_counts_clean[[5]] <- ucsc_counts[[5]] %>% clean_com("aircar", "vulpsp", site = "ucsc")
  ucsc_counts_clean[[6]] <- ucsc_counts[[6]] %>% clean_com("aircar", "vulpsp", site = "ucsc")
  ucsc_counts_clean[[7]] <- ucsc_counts[[7]] %>% clean_com("aircar", "vulpsp", site = "ucsc")
  ucsc_counts_clean[[8]] <- ucsc_counts[[8]] %>% clean_com("aircar", "vulpsp", site = "ucsc")
  ucsc_counts_clean[[9]] <- ucsc_counts[[9]] %>% clean_com("aircar", "vulpsp", site = "ucsc")
  ucsc_counts_clean[[10]] <- ucsc_counts[[10]] %>% clean_com("aircar", "brohor", site = "ucsc")
  ucsc_counts_clean[[11]] <- ucsc_counts[[11]] %>% clean_com("aircar", "vulpsp", site = "ucsc")
  ucsc_counts_clean[[12]] <- ucsc_counts[[12]] %>% clean_com("aircar", "vulpsp", site = "ucsc")
  ucsc_counts_clean[[13]] <- ucsc_counts[[13]] %>% clean_com("aircar", "vulpsp", site = "ucsc")
  ucsc_counts_clean[[14]] <- ucsc_counts[[14]] %>% clean_com("aircar", "vulpsp", site = "ucsc")

  # lapply(ucsc_counts_clean, dim)
  for (i in 1:length(ucsc_counts_clean)) {
    ucsc_counts_clean[[i]]$year <- (1999:2012)[i]
  }

  # Once the data is cleaned and in a similar format, it's ready to be compiled.
  ucsc_counts_df <- do.call(rbind, ucsc_counts_clean) %>%
    left_join(UCSCspp, by = "species.code") %>%
    filter(intercepts > 0) %>%
    select("rep", "year", "species.name", "intercepts", "guild") %>%
    mutate(
      site = "ucsc",
      species.name = factor(species.name)
    )
  # removing unwanted guilds (moss, litter, etc.)
  ucsc_counts_df <- ucsc_counts_df %>%
    group_by(rep, year, guild, species.name) %>%
    summarize(intercepts = sum(intercepts)) %>%
    filter(
      guild != "Moss",
      guild != "Thatch",
      guild != "Bare"
    )
  # unique(ucsc_counts_df$species.name)
  # unique(ucsc_counts_df$guild)
  elk_guilds <- ucsc_counts_df %>%
    group_by(rep, year, guild) %>%
    summarize(guildtotal = sum(intercepts)) %>%
    ungroup() %>%
    group_by(year, guild) %>%
    summarize(mean.intercepts = mean(guildtotal))

  ucsc_tbl <- ucsc_counts_df %>%
    filter(
      species.name != "Moss",
      species.name != "Thatch",
      species.name != "Bare ground"
    ) %>%
    mutate(site = "ucsc")

  ucsc_tbl <- ucsc_tbl %>%
    as_tibble() %>%
    select(site, year, plot = rep, species = species.name, guild, abund = intercepts) %>%
    mutate(year = as.integer(year), species = as.character(species) %>% str_trim(), guild = as.character(guild), abund = as.integer(abund), abund_type = "point_intercept") %>%
    arrange(site, year, plot, species)

  return(ucsc_tbl)
}

tidy_jasper <- function(basedir) {
  # read cover data
  com_tbl <- basedir %>%
    str_c("Jasper/JR_cover_forJosie.csv") %>%
    read_csv(col_types = cols_only(year = "d", species = "c", cover = "d", uniqueID = "c")) %>%
    pivot_wider(names_from = species, values_from = cover) %>%
    replace(is.na(.), 0) %>%
    pivot_longer(cols = aghe:vumi, names_to = "species", values_to = "cover") %>%
    group_by(uniqueID, year, species) %>%
    summarize(cover = mean(cover)) %>%
    ungroup()

  # read species data
  spp_tbl <- basedir %>%
    str_c("Jasper/JR_speciesnames2.csv") %>%
    read_csv(col_types = "c")

  # combine
  jasper_tbl <- com_tbl %>%
    left_join(spp_tbl, by = "species") %>%
    filter(cover > 0) %>%
    mutate(
      site = "jasper",
      species.name = str_trim(species.name),
      abund_type = "cover"
    ) %>%
    select(site, year,
      plot = uniqueID,
      species = species.name, guild,
      abund = cover, abund_type
    ) %>%
    arrange(site, year, plot, species)

  return(jasper_tbl)
}

tidy_morganterritory <- function(basedir) {
  excel_file <- basedir %>%
    str_c("MorganTerritory/UCBerkeley Range Lab Morgan Territory all plots transect data 2003-2011.xlsx")

  com_tbl <- excel_file %>%
    readxl::read_xlsx(sheet = "MT1-MT16 2003-2011") %>%
    mutate(year = as.integer(year)) %>%
    select(year, plot = `plot ID`, transect, point, species_code = species) %>%
    mutate(species_code = toupper(species_code)) %>%
    group_by(year, plot, species_code) %>%
    summarize(hits = n()) %>% # hits as total counts
    group_by(year, plot) %>%
    mutate(tot_hits = sum(hits)) %>%
    group_by(year, plot, species_code) %>%
    summarize(abund = hits / tot_hits) %>% # rel abundance
    ungroup()

  spp_tbl <- excel_file %>%
    readxl::read_xlsx(sheet = "Species codes & attributes") %>%
    mutate(
      species_code = toupper(species) %>% str_trim(),
      latin = str_trim(latin),
      guild = str_c(
        case_when(
          `native/exotic` == "e" ~ "E", # Exotic
          `native/exotic` == "n" ~ "N", # Native
          TRUE ~ "U" # else is Unknown
        ),
        case_when(
          `annual/perennial` == "a" ~ "A", # Annual
          `annual/perennial` == "p" ~ "P", # Perennial
          `annual/perennial` == "a, p" ~ "AP", # both Annual and Perennial?
          TRUE ~ "U" # else is Unknown
        ),
        case_when(
          `forb/grass` == "f" ~ "F", # Forb
          `forb/grass` == "g" ~ "G", # Grass
          `forb/grass` == "n" ~ "R", # Rush
          `forb/grass` == "s" ~ "S", # Shurb
          `forb/grass` == "t" ~ "T", # Tree
          TRUE ~ "U" # else is Unknown
        )
      )
    ) %>%
    select(species_code, species = latin, guild) %>%
    filter(
      !species_code %in% c(
        "DISKED", # disked ground
        "LITT", # litter
        "MOSS", # moss
        "MUD", # mud
        "ND", # no data
        "ROCK", # rock
        "SOIL", # soil
        "TRASH", # trash
        "WATER" # water
      ),
      !str_starts(species_code, "UN") # unknown species
    )

  plt_tbl <- excel_file %>%
    readxl::read_xlsx(sheet = "Grazed status") %>%
    separate_rows(year, sep = "-") %>% # 2003-2004 and 2010-2011 to separate rows
    select(year, plot = `plot ID`, grazed, type) %>%
    mutate(year = as.integer(year))

  morganterritory_tbl <- com_tbl %>%
    inner_join(spp_tbl, by = "species_code") %>% # only keep species in species table
    mutate(
      site = "morganterritory",
      abund_type = "point_intercept"
    ) %>%
    right_join(filter(plt_tbl, grazed == "n"), # join non-grazed plots
      by = c("year", "plot")
    ) %>%
    select(site, year, plot, species, guild, abund, abund_type) %>%
    arrange(year, plot, species)

  return(morganterritory_tbl)
}

tidy_jrgce <- function(basedir) {
  # pin count table
  pin_tbl <- basedir %>%
    str_c("JRGCE/Pin count.csv") %>%
    read_csv(col_types = "iiccicc") %>%
    select(year = YEAR, plot = ID, species = SPECIES, abund = TOTAL) %>%
    mutate(species = if_else(
      str_starts(species, "Acmispon americanus"),
      "Acmispon americanus var.americanus",
      species
    )) %>%
    mutate(species = if_else(
      str_starts(species, "Crepis"),
      "Crepis vesicaria ssp. taraxacifolia",
      species
    )) %>%
    filter(!is.na(abund))

  # species table
  spp_tbl <- basedir %>%
    str_c("JRGCE/Species.csv") %>%
    read_csv(col_types = "c") %>%
    mutate(guild = str_c(case_when(
      NATIVE_CODE == "I" ~ "E", # Invasive -> Exotic
      NATIVE_CODE == "N" ~ "N", # Native
      NATIVE_CODE == "U" ~ "U", # Unknown
      is.na(NATIVE_CODE) ~ "U"
    ), FXNGROUP_CODE)) %>%
    select(species = `Species or unit name`, guild)

  # treatment table
  trt_tbl_raw <- basedir %>%
    str_c("JRGCE/Treatment.csv") %>%
    read_csv(col_types = "iiiiciiiiiiiiic")

  trt_tbl <- trt_tbl_raw %>%
    pull(id) %>%
    expand_grid(year = 1998:2014, plot = .) %>% # full year-treatment tibble
    full_join(
      trt_tbl_raw %>%
        select(plot = id, starts_with("Heat_"), Precip, CO2, Nitrogen),
      by = "plot"
    ) %>%
    mutate( # recode treatment
      tmp = case_when(
        year >= 1998 & year <= 2003 ~ as.logical(Heat_1998_2003 - 1),
        year >= 2004 & year <= 2006 ~ as.logical(Heat_2004_2006 - 1),
        year >= 2007 & year <= 2014 ~ as.logical(Heat_2007_2014 - 1)
      ),
      ppt = as.logical(Precip - 1),
      co2 = as.logical(CO2 - 1),
      ntg = as.logical(Nitrogen - 1)
    ) %>%
    mutate(
      tmp = if_else(year == 1998, FALSE, tmp),
      ppt = if_else(year == 1998, FALSE, ppt),
      co2 = if_else(year == 1998, FALSE, co2),
      ntg = if_else(year == 1998, FALSE, ntg)
    ) %>%
    mutate(
      treat = str_c(
        ifelse(tmp, "T", "_"),
        ifelse(ppt, "P", "_"),
        ifelse(co2, "C", "_"),
        ifelse(ntg, "N", "_")
      )
    ) %>%
    select(year, plot, treat) %>%
    arrange(year, plot)

  # join all tables
  jrgce_tbl <- pin_tbl %>%
    left_join(spp_tbl, by = "species") %>%
    left_join(trt_tbl, by = c("year", "plot")) %>%
    mutate(site = "jrgce", abund_type = "point_intercept") %>%
    select(site, year, plot, treat, species, guild, abund, abund_type) %>%
    arrange(year, plot, species)

  return(jrgce_tbl)
}

tidy_mclexp <- function(basedir) {
  # read in raw data
  # note 2020 data is missing.
  excel_file <- str_c(basedir, "HarrisonExperiment/2015-2021 Water Experiment Data.xlsx")
  com_2015_tbl <- readxl::read_excel(excel_file, sheet = "2015")
  com_2016_tbl <- readxl::read_excel(excel_file, sheet = "2016")
  com_2017_tbl <- readxl::read_excel(excel_file, sheet = "2017")
  com_2018_tbl <- readxl::read_excel(excel_file, sheet = "2018")
  com_2019_tbl <- readxl::read_excel(excel_file, sheet = "2019")
  # com_2020_tbl <- readxl::read_excel(excel_file, sheet = "2020")
  com_2021_tbl <- readxl::read_excel(excel_file, sheet = "2021")
  plt_tbl <- readxl::read_excel(excel_file, sheet = "Plot codes")

  # combine cover data across years and join with plot-treatment data.
  com_long_tbl <- (
    com_2015_tbl %>%
      rename(species = "Plots") %>%
      pivot_longer(-species, names_to = "plot", values_to = "abund") %>%
      mutate(year = 2015)
  ) %>%
    bind_rows(
      com_2016_tbl %>%
        rename(species = "Plots") %>%
        pivot_longer(-species, names_to = "plot", values_to = "abund") %>%
        mutate(year = 2016)
    ) %>%
    bind_rows(
      com_2017_tbl %>%
        rename(species = "Plots") %>%
        pivot_longer(-species, names_to = "plot", values_to = "abund") %>%
        mutate(year = 2017)
    ) %>%
    bind_rows(
      com_2018_tbl %>%
        rename(species = "Plots") %>%
        pivot_longer(-species, names_to = "plot", values_to = "abund") %>%
        mutate(year = 2018)
    ) %>%
    bind_rows(
      com_2019_tbl %>%
        rename(species = "Plots") %>%
        pivot_longer(-species, names_to = "plot", values_to = "abund") %>%
        mutate(year = 2019)
    ) %>%
    bind_rows(
      com_2021_tbl %>%
        rename(species = "Plots") %>%
        pivot_longer(-species, names_to = "plot", values_to = "abund") %>%
        mutate(year = 2021)
    ) %>%
    select(year, plot, species, abund) %>%
    mutate(plot = as.integer(plot))

  mclexp_tbl <- plt_tbl %>%
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
    full_join(com_long_tbl, by = "plot") %>%
    mutate(site = "mclexp", guild = NA, abund_type = "cover") %>%
    select(site, year, plot, treat, species, guild, abund, abund_type) %>%
    filter(
      abund > 0,
      !(species %in% c("bare", "gopher", "litter", "mosses", "rock", "soil"))
    ) %>%
    arrange(year, plot, species)

  return(mclexp_tbl)
}
