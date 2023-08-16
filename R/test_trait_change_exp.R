test_trait_change_exp_all <- function(dat_community_exp, dat_niche) {
  df_test_trait_change_exp <- test_trait_change_exp_comb() %>%
    rowwise() %>%
    mutate(test_trait_change_exp(dat_community_exp, dat_niche, exp, trt, grp, var)) %>%
    ungroup()

  return(df_test_trait_change_exp)
}


test_trait_change_exp <- function(dat_community, dat_niche, exp, trt, grp, var) {
  # subset data
  dat_lme <- test_trait_change_exp_data(dat_community, dat_niche, exp, trt, grp, var)

  # fit lme
  mod_lme <- test_trait_change_exp_model(dat_lme, var)
  # summarize results

  df_lme_exp <- test_trait_change_summ(mod_lme)

  c(exp, trt, grp, var) %>%
    str_replace_na() %>%
    str_c(collapse = "_") %>%
    print()

  return(df_lme_exp)
}

test_trait_change_summ <- function(mod_lme) {
  df_summ <- data.frame(
    estimate = mod_lme$summary$coefficients[2, 1],
    p.value = mod_lme$summary$coefficients[2, 5]
  ) %>%
    mutate(sig = gtools::stars.pval(p.value))

  return(df_summ)
}

test_trait_change_exp_model <- function(dat_lme, var) {
  if (var == "ppt") {
    dat_lme <- dat_lme %>%
      mutate(value = log(value))
  }
  if ("subgrp" %in% colnames(dat_lme)) {
    model <- lmerTest::lmer(value ~ trt + (1 | year) + (1 | subgrp),
      weights = dat_lme %>% pull(weight),
      data = dat_lme
    )
  } else {
    model <- lmerTest::lmer(value ~ trt + (1 | year),
      weights = dat_lme %>% pull(weight),
      data = dat_lme
    )
  }

  res <- list(model = model, summary = summary(model))
  return(res)
}

test_trait_change_exp_data <- function(dat_community, dat_niche, exp, trt, grp, var) {
  if (exp == "jrgce") {
    if (trt == "Warming") {
      dat_exp <- dat_community %>%
        filter(site == exp, year >= 1999) %>%
        mutate(trt = str_sub(treat, start = 1L, end = 1L)) %>%
        mutate(subgrp = str_sub(treat, start = 2L, end = 4L)) %>%
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
      mutate(treat_W = case_when(
        treat == "_X" ~ "_",
        treat == "WX" ~ "P"
      )) %>%
      mutate(treat_W = factor(treat_W, levels = c("_", "P"))) %>%
      mutate(treat_D = case_when(
        treat == "X_" ~ "_",
        treat == "XD" ~ "D"
      )) %>%
      mutate(treat_D = factor(treat_D, levels = c("_", "D"))) %>%
      filter(soil == grp) %>%
      select(trt = treat_W, year, plot, species, abund) %>%
      drop_na(trt)
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

test_trait_change_exp_comb <- function() {
  ls_df_trait_change_exp_comb <- vector(mode = "list")
  for (exp in c("jrgce", "mclexp", "scide")) {
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
    arrange(exp, trt, grp, var)
  return(df_trait_change_exp)
}
