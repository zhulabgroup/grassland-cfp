#' @export
test_index_change_data <- function(dat_index, index, grouping, option) {
  if (option == "obs") {
    return(test_index_change_data_obs(dat_index$obs, index,
      siteoi = grouping$site
    ))
  }
  if (option == "exp") {
    return(test_index_change_data_exp(dat_index$exp, index,
      exp = grouping$exp,
      trt = grouping$trt
    ))
  }
}

test_index_change_data_obs <- function(dat_index, index, siteoi = "all") {
  var <- case_when(
    index == "cti" ~ "tmp",
    index == "cpi" ~ "ppt"
  )
  dat_index <- dat_index %>%
    select(site, year, plot, value = !!sym(str_c(var, "_com_mean")))

  if (siteoi == "all") {
    siteoi <- dat_index %>%
      pull(site) %>%
      unique()
  }
  dat_lme <- dat_index %>%
    filter(site %in% siteoi) %>%
    mutate(year = year - (dat_index %>%
      pull(year) %>%
      range() %>%
      mean()))

  return(dat_lme)
}

test_index_change_data_exp <- function(dat_index, index, exp, trt, grp) {
  var <- case_when(
    index == "cti" ~ "tmp",
    index == "cpi" ~ "ppt"
  )
  dat_index <- dat_index %>%
    select(site, year, plot, treat, value = !!sym(str_c(var, "_com_mean")))
  if (exp == "jrgce") {
    if (trt == "Warming") {
      dat_exp <- dat_index %>%
        filter(site == exp, year >= 1999) %>%
        mutate(trt = str_sub(treat, start = 1L, end = 1L)) %>%
        # mutate(subgrp = str_sub(treat, start = 2L, end = 4L)) %>%
        # filter(subgrp == "___") %>%
        mutate(subgrp = case_when(
          year <= 2002 ~ "Phase I",
          year >= 2010 ~ "Phase III",
          TRUE ~ "Phase II",
        )) %>%
        # filter(phase == grp) %>%
        select(trt, subgrp, year, plot, value)
    }
    if (trt == "Watering") {
      dat_exp <- dat_index %>%
        filter(site == "jrgce", year >= 1999) %>%
        mutate(trt = str_sub(treat, start = 2L, end = 2L)) %>%
        # mutate(subgrp = str_c(
        #   str_sub(treat, start = 1L, end = 1L),
        #   str_sub(treat, start = 3L, end = 4L)
        # )) %>%
        # filter(subgrp == "___") %>%
        mutate(subgrp = NA) %>%
        select(trt, subgrp, year, plot, value)
    }
  }
  if (exp == "mclexp") {
    dat_exp <- dat_index %>%
      filter(site == exp, year > 2015) %>%
      separate(treat, c("treat", "soil"), sep = 2) %>%
      mutate(subgrp = case_when(
        soil == "S" ~ "Serpentine",
        soil == "N" ~ "Non-serpentine"
      ))

    if (trt == "Watering") {
      dat_exp <- dat_exp %>%
        mutate(treat_W = case_when(
          treat == "_X" ~ "_",
          treat == "WX" ~ "P"
        )) %>%
        mutate(trt = factor(treat_W, levels = c("_", "P"))) %>%
        drop_na(trt) %>%
        select(trt, subgrp, year, plot, value)
    }

    if (trt == "Drought") {
      dat_exp <- dat_exp %>%
        mutate(treat_D = case_when(
          treat == "X_" ~ "_",
          treat == "XD" ~ "D"
        )) %>%
        mutate(trt = factor(treat_D, levels = c("_", "D"))) %>%
        drop_na(trt) %>%
        select(trt, subgrp, year, plot, value)
    }
  }
  if (exp == "scide") {
    dat_exp <- dat_index %>%
      filter(str_detect(site, "scide")) %>%
      mutate(site = str_replace(site, "scide_", "")) %>%
      mutate(subgrp = case_when(
        site == "arboretum" ~ "Arboretum",
        site == "marshallfield" ~ "Marshall Field",
        site == "ylr" ~ "Younger Lagoon"
      )) %>%
      mutate(trt = factor(treat, levels = c("_", "D"))) %>%
      select(trt, subgrp, year, plot, value)
  }

  dat_lme <- dat_exp

  return(dat_lme)
}

#' @export
test_trait_change_data <- function(dat_community, dat_niche, trait, grouping, option) {
  if (option == "obs") {
    return(test_trait_change_data_obs(dat_community$obs, dat_niche, trait,
      siteoi = grouping$site
    ))
  }
  if (option == "exp") {
    return(test_trait_change_data_exp(dat_community$exp, dat_niche, trait,
      exp = grouping$exp,
      trt = grouping$trt,
      grp = grouping$grp
    ))
  }
}

test_trait_change_data_obs <- function(dat_community, dat_niche, trait, siteoi = "all") {
  if (siteoi == "all") {
    siteoi <- dat_community %>%
      pull(site) %>%
      unique()
  }
  dat_lme <- dat_community %>%
    filter(site %in% siteoi) %>%
    select(site, year, plot, species, abund) %>%
    group_by(site, plot, year) %>%
    mutate(weight = abund / sum(abund)) %>% # convert all abundance (absolute or relative) to percentage, such that all plots get equal weight later in lme
    ungroup() %>%
    inner_join(
      dat_niche %>%
        select(species, value = !!sym(str_c(trait, "_occ_median"))),
      by = "species"
    )

  return(dat_lme)
}

test_trait_change_data_exp <- function(dat_community, dat_niche, trait, exp, trt, grp) {
  if (exp == "jrgce") {
    if (trt == "Warming") {
      dat_exp <- dat_community %>%
        filter(site == exp, year >= 1999) %>%
        mutate(trt = str_sub(treat, start = 1L, end = 1L)) %>%
        mutate(subgrp = str_sub(treat, start = 2L, end = 4L)) %>%
        # filter(subgrp == "___") %>%
        mutate(phase = case_when(
          year <= 2002 ~ "Phase I",
          year >= 2010 ~ "Phase III",
          TRUE ~ "Phase II",
        )) %>%
        filter(phase == grp) %>%
        select(trt, subgrp, year, plot, species, abund)
    }
    if (trt == "Watering") {
      dat_exp <- dat_community %>%
        filter(site == "jrgce", year >= 1999) %>%
        mutate(trt = str_sub(treat, start = 2L, end = 2L)) %>%
        mutate(subgrp = str_c(
          str_sub(treat, start = 1L, end = 1L),
          str_sub(treat, start = 3L, end = 4L)
        )) %>%
        # filter(subgrp == "___") %>%
        select(trt, subgrp, year, plot, species, abund)
    }
  }
  if (exp == "mclexp") {
    dat_exp <- dat_community %>%
      filter(site == exp, year > 2015) %>%
      separate(treat, c("treat", "soil"), sep = 2) %>%
      mutate(soil = case_when(
        soil == "S" ~ "Serpentine",
        soil == "N" ~ "Non-serpentine"
      )) %>%
      filter(soil == grp)


    if (trt == "Watering") {
      dat_exp <- dat_exp %>%
        mutate(treat_W = case_when(
          treat == "_X" ~ "_",
          treat == "WX" ~ "P"
        )) %>%
        mutate(trt = factor(treat_W, levels = c("_", "P"))) %>%
        drop_na(trt) %>%
        select(trt, year, plot, species, abund)
    }

    if (trt == "Drought") {
      dat_exp <- dat_exp %>%
        mutate(treat_D = case_when(
          treat == "X_" ~ "_",
          treat == "XD" ~ "D"
        )) %>%
        mutate(trt = factor(treat_D, levels = c("_", "D"))) %>%
        drop_na(trt) %>%
        select(trt, year, plot, species, abund)
    }
  }
  if (exp == "scide") {
    dat_exp <- dat_community %>%
      filter(str_detect(site, "scide")) %>%
      mutate(site = str_replace(site, "scide_", "")) %>%
      mutate(site = case_when(
        site == "arboretum" ~ "Arboretum",
        site == "marshallfield" ~ "Marshall Field",
        site == "ylr" ~ "Younger Lagoon"
      )) %>%
      filter(site == grp) %>%
      mutate(trt = factor(treat, levels = c("_", "D"))) %>%
      select(trt, year, plot, species, abund)
  }

  dat_lme <- dat_exp %>%
    group_by(trt, plot, year) %>%
    mutate(weight = abund / sum(abund)) %>% # convert all abundance (absolute or relative) to percentage, such that all plots get equal weight later in lme
    ungroup() %>%
    inner_join(
      dat_niche %>%
        select(species, value = !!sym(str_c(trait, "_occ_median"))),
      by = "species"
    )

  return(dat_lme)
}
