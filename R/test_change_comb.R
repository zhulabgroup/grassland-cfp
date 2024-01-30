#' @export
test_index_change_comb <- function(dat_index, option, response = "index", levels = c("cti", "cpi")) {
  if (option == "obs") {
    return(test_change_comb_obs(dat_index$obs, response = response, levels = levels))
  }
  if (option == "exp") {
    return(test_change_comb_exp(dat_index$exp, response = response, levels = levels))
  }
}

#' @export
test_trait_change_comb <- function(dat_community, option, response = "trait", levels = c("tmp", "ppt")) {
  if (option == "obs") {
    return(test_change_comb_obs(dat_community$obs, response = response, levels = levels))
  }
  if (option == "exp") {
    return(test_change_comb_exp(dat_community$exp, response = response, levels = levels))
  }
}

test_change_comb_obs <- function(dat, response = "trait", levels = c("tmp", "ppt")) {
  df_change_obs <- expand_grid(
    site = dat %>% pull(site) %>% unique() %>% c("all"),
    response = levels
  ) %>%
    mutate(response = factor(response, levels = levels)) %>%
    arrange(site, response) %>%
    rowwise() %>%
    mutate(grouping = list(tibble(site))) %>%
    select(response, grouping)

  colnames(df_change_obs) <- c(response, "grouping")

  return(df_change_obs)
}

test_change_comb_exp <- function(dat, response = "trait", levels = c("tmp", "ppt")) {
  ls_exp <- dat %>%
    rowwise() %>%
    mutate(site = str_split(site, "_", simplify = T)[1]) %>%
    ungroup() %>%
    pull(site) %>%
    unique()
  ls_df_change_exp_comb <- vector(mode = "list")
  for (exp in ls_exp) {
    if (exp == "jrgce") {
      for (trt in c("Warming", "Watering")) {
        #   if (trt == "Warming") {
        #     v_grp <- c("Phase I", "Phase II", "Phase III")
        #     for (grp in v_grp) {
        #       ls_df_change_exp_comb[[str_c(exp, " ", trt, " ", grp)]] <- data.frame(exp, trt, grp)
        #     }
        #   }
        #   if (trt == "Watering") {
        ls_df_change_exp_comb[[str_c(exp, " ", trt)]] <- data.frame(exp, trt)
        #   }
      }
    }
    if (exp == "mclexp") {
      for (trt in c("Watering", "Drought")) {
        # if (trt == "Watering") {
        #   v_grp <- c("Serpentine", "Non-serpentine")
        #   for (grp in v_grp) {
        #     ls_df_change_exp_comb[[str_c(exp, " ", trt, " ", grp)]] <- data.frame(exp, trt, grp)
        #   }
        # }
        # if (trt == "Drought") {
        #   grp <- "Serpentine"
        ls_df_change_exp_comb[[str_c(exp, " ", trt)]] <- data.frame(exp, trt)
        # }
      }
    }
    if (exp == "scide") {
      trt <- "Drought"
      # for (grp in c("Arboretum", "Marshall Field", "Younger Lagoon")) {
      ls_df_change_exp_comb[[str_c(exp, " ", trt)]] <- data.frame(exp, trt)
      # }
    }
    if (!exp %in% c("jrgce", "mclexp", "scide")) {
      message(str_c("Experiment ", exp, " not recognized."))
    }
  }
  df_change_exp <- bind_rows(ls_df_change_exp_comb) %>%
    cross_join(data.frame(response = levels)) %>%
    mutate(exp = factor(exp,
      levels = c(
        "jrgce",
        "mclexp",
        "scide"
      )
    )) %>%
    mutate(response = factor(response, levels = levels)) %>%
    mutate(trt = factor(trt, levels = c("Warming", "Watering", "Drought"))) %>%
    # mutate(grp = factor(grp, levels = c("Phase I", "Phase II", "Phase III", "Serpentine", "Non-serpentine", "Arboretum", "Marshall Field", "Younger Lagoon"))) %>%
    select(exp, trt, grp, response, everything()) %>%
    arrange(exp, trt, grp, response) %>%
    rowwise() %>%
    mutate(grouping = list(tibble(exp, trt, grp))) %>%
    select(response, grouping)

  colnames(df_change_exp) <- c(response, "grouping")

  return(df_change_exp)
}
