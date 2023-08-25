test_trait_change_all <- function(dat_community, dat_niche, type) {
  df_test_trait_change_exp <- test_trait_change_comb(dat_community, type) %>%
    mutate(test_trait_change(dat_community, dat_niche, var, grouping, type)) %>%
    select(grouping, everything()) %>%
    unnest(grouping) %>%
    ungroup()

  return(df_test_trait_change_exp)
}


test_trait_change <- function(dat_community, dat_niche, var, grouping, type) {
  # subset data
  dat_lme <- test_trait_change_data(dat_community, dat_niche, var, grouping, type)

  # fit lme
  mod_lme <- test_trait_change_model(dat_lme, var, type)
  # summarize results

  df_lme <- test_trait_change_summ(mod_lme)


  return(df_lme)
}

test_trait_change_summ <- function(mod_lme) {
  df_summ <- data.frame(
    estimate = mod_lme$summary$coefficients[2, 1],
    p.value = mod_lme$summary$coefficients[2, ncol(mod_lme$summary$coefficients)] # good for both lm and lme
  ) %>%
    mutate(sig = gtools::stars.pval(p.value))

  return(df_summ)
}

test_trait_change_model <- function(dat_lme, var, type) {
  # if (var == "ppt") {
  #   dat_lme <- dat_lme %>%
  #     mutate(value = rank(value))
  # }

  if (type == "obs") {
    if (dat_lme %>% pull(site) %>% unique() %>% length() > 1) {
      model <- lmerTest::lmer(value ~ year + (1 | site),
        weights = dat_lme %>% pull(weight),
        data = dat_lme
      )
    } else {
      model <- lm(value ~ year,
        weights = dat_lme %>% pull(weight),
        data = dat_lme
      )
    }
  }

  if (type == "exp") {
    # if ("subgrp" %in% colnames(dat_lme)) {
    #   model <- lmerTest::lmer(value ~ trt + subgrp + (1 | year),
    #                           weights = dat_lme %>% pull(weight),
    #                           data = dat_lme
    #   )
    # }
    # else {
    model <- lmerTest::lmer(value ~ trt + (1 | year),
      weights = dat_lme %>% pull(weight),
      data = dat_lme
    )
    # }
  }

  res <- list(model = model, summary = summary(model))
  return(res)
}

test_trait_change_data <- function(dat_community, dat_niche, var, grouping, type) {
  if (type == "obs") {
    return(test_trait_change_data_obs(dat_community$obs, dat_niche, var,
      siteoi = grouping$site
    ))
  }
  if (type == "exp") {
    return(test_trait_change_data_exp(dat_community$exp, dat_niche, var,
      exp = grouping$exp,
      trt = grouping$trt,
      grp = grouping$grp
    ))
  }
}

test_trait_change_data_obs <- function(dat_community, dat_niche, var, siteoi = "all") {
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
    left_join(
      dat_niche %>%
        select(species, value = !!sym(str_c(var, "_occ_mean"))),
      by = "species"
    )

  return(dat_lme)
}
test_trait_change_data_exp <- function(dat_community, dat_niche, var, exp, trt, grp) {
  if (exp == "jrgce") {
    if (trt == "Warming") {
      dat_exp <- dat_community %>%
        filter(site == exp, year >= 1999) %>%
        mutate(trt = str_sub(treat, start = 1L, end = 1L)) %>%
        mutate(subgrp = str_sub(treat, start = 2L, end = 4L)) %>%
        filter(subgrp == "___") %>%
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
        filter(subgrp == "___") %>%
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
    group_by(plot, year) %>%
    mutate(weight = abund / sum(abund)) %>% # convert all abundance (absolute or relative) to percentage, such that all plots get equal weight later in lme
    ungroup() %>%
    left_join(
      dat_niche %>%
        select(species, value = !!sym(str_c(var, "_occ_mean"))),
      by = "species"
    )

  return(dat_lme)
}


test_trait_change_comb <- function(dat_community, type) {
  if (type == "obs") {
    return(test_trait_change_comb_obs(dat_community$obs))
  }
  if (type == "exp") {
    return(test_trait_change_comb_exp(dat_community$exp))
  }
}
test_trait_change_comb_obs <- function(dat_community_obs) {
  df_trait_change_obs <- expand_grid(
    site = dat_community_obs %>% pull(site) %>% unique() %>% c("all"),
    var = c("tmp", "ppt")
  ) %>%
    mutate(var = factor(var, levels = c("tmp", "ppt"))) %>%
    arrange(site, var) %>%
    rowwise() %>%
    mutate(grouping = list(tibble(site))) %>%
    select(var, grouping)

  return(df_trait_change_obs)
}
test_trait_change_comb_exp <- function(dat_community_exp) {
  ls_exp <- dat_community_exp %>%
    rowwise() %>%
    mutate(site = str_split(site, "_", simplify = T)[1]) %>%
    ungroup() %>%
    pull(site) %>%
    unique()
  ls_df_trait_change_exp_comb <- vector(mode = "list")
  for (exp in ls_exp) {
    if (exp == "jrgce") {
      for (trt in c("Warming", "Watering")) {
        if (trt == "Warming") {
          v_grp <- c("Phase I", "Phase II", "Phase III")
          for (grp in v_grp) {
            ls_df_trait_change_exp_comb[[str_c(exp, " ", trt, " ", grp)]] <- data.frame(exp, trt, grp)
          }
        }
        if (trt == "Watering") {
          ls_df_trait_change_exp_comb[[str_c(exp, " ", trt)]] <- data.frame(exp, trt, grp = NA)
        }
      }
    }
    if (exp == "mclexp") {
      for (trt in c("Watering", "Drought")) {
        if (trt == "Watering") {
          v_grp <- c("Serpentine", "Non-serpentine")
          for (grp in v_grp) {
            ls_df_trait_change_exp_comb[[str_c(exp, " ", trt, " ", grp)]] <- data.frame(exp, trt, grp)
          }
        }
        if (trt == "Drought") {
          grp <- "Serpentine"
          ls_df_trait_change_exp_comb[[str_c(exp, " ", trt)]] <- data.frame(exp, trt, grp)
        }
      }
    }
    if (exp == "scide") {
      trt <- "Drought"
      for (grp in c("Arboretum", "Marshall Field", "Younger Lagoon")) {
        ls_df_trait_change_exp_comb[[str_c(exp, " ", trt, " ", grp)]] <- data.frame(exp, trt, grp)
      }
    }
    if (!exp %in% c("jrgce", "mclexp", "scide")) {
      message(str_c("Experiment ", exp, " not recognized."))
    }
  }
  df_trait_change_exp <- bind_rows(ls_df_trait_change_exp_comb) %>%
    cross_join(data.frame(var = c("tmp", "ppt"))) %>%
    mutate(exp = factor(exp,
      levels = c(
        "jrgce",
        "mclexp",
        "scide"
      )
    )) %>%
    mutate(var = factor(var, levels = c("tmp", "ppt"))) %>%
    mutate(trt = factor(trt, levels = c("Warming", "Watering", "Drought"))) %>%
    mutate(grp = factor(grp, levels = c("Phase I", "Phase II", "Phase III", "Serpentine", "Non-serpentine", "Arboretum", "Marshall Field", "Younger Lagoon"))) %>%
    select(exp, trt, grp, var, everything()) %>%
    arrange(exp, trt, grp, var) %>%
    rowwise() %>%
    mutate(grouping = list(tibble(exp, trt, grp))) %>%
    select(var, grouping)
  return(df_trait_change_exp)
}
