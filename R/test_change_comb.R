test_index_change_comb <- function(dat_community, option) {
  df <- test_trait_change_comb(dat_community, option) %>%
    mutate(index = case_when(
      trait == "tmp" ~ "cti",
      trait == "ppt" ~ "cpi",
      trait == "cwd" ~ "cdi"
    )) %>%
    mutate(index = factor(index, levels = c("cti", "cpi", "cdi"))) %>%
    select(index, grouping)
  return(df)
}

test_trait_change_comb <- function(dat_community, option) {
  if (option == "obs") {
    return(test_change_comb_obs(dat_community$obs))
  }
  if (option == "exp") {
    return(test_change_comb_exp(dat_community$exp))
  }
}
test_change_comb_obs <- function(dat_community_obs) {
  df_trait_change_obs <- expand_grid(
    site = dat_community_obs %>% pull(site) %>% unique() %>% c("all"),
    trait = c("tmp", "ppt")
  ) %>%
    mutate(trait = factor(trait, levels = c("tmp", "ppt"))) %>%
    arrange(site, trait) %>%
    rowwise() %>%
    mutate(grouping = list(tibble(site))) %>%
    select(trait, grouping)

  return(df_trait_change_obs)
}
test_change_comb_exp <- function(dat_community_exp) {
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
    cross_join(data.frame(trait = c("tmp", "ppt"))) %>%
    mutate(exp = factor(exp,
      levels = c(
        "jrgce",
        "mclexp",
        "scide"
      )
    )) %>%
    mutate(trait = factor(trait, levels = c("tmp", "ppt"))) %>%
    mutate(trt = factor(trt, levels = c("Warming", "Watering", "Drought"))) %>%
    mutate(grp = factor(grp, levels = c("Phase I", "Phase II", "Phase III", "Serpentine", "Non-serpentine", "Arboretum", "Marshall Field", "Younger Lagoon"))) %>%
    select(exp, trt, grp, trait, everything()) %>%
    arrange(exp, trt, grp, trait) %>%
    rowwise() %>%
    mutate(grouping = list(tibble(exp, trt, grp))) %>%
    select(trait, grouping)
  return(df_trait_change_exp)
}
