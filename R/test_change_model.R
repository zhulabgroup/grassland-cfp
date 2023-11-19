#' @export
test_index_change_model <- function(dat_model, option) {
  # if (index == "cpi") {
  #   dat_model <- dat_model %>%
  #     mutate(value = rank(value))
  # }

  if (option == "obs") {
    if (dat_model %>% pull(site) %>% unique() %>% length() > 1) {
      model <- lmerTest::lmer(value ~ year + (1 | site),
        data = dat_model
      )
    } else {
      model <- lm(value ~ year,
        data = dat_model
      )
    }
  }

  if (option == "exp") {
    if (dat_model %>% pull(year) %>% unique() %>% length() > 1) {
      model <- lmerTest::lmer(value ~ trt + (1 | year),
        data = dat_model
      )
    } else {
      model <- lm(value ~ trt,
        data = dat_model
      )
    }
  }

  # sample <- sample(residuals(model), 1000,prob =dat_model %>% drop_na() %>% pull(weight) )
  # shapiro.test(sample)
  res <- list(model = model, summary = summary(model))
  return(res)
}

#' @export
test_trait_change_model <- function(dat_model, option) {
  # if (trait == "ppt") {
  #   dat_model <- dat_model %>%
  #     mutate(value = rank(value))
  # }

  if (option == "obs") {
    if (dat_model %>% pull(site) %>% unique() %>% length() > 1) {
      model <- lmerTest::lmer(value ~ year + (1 | site),
        weights = dat_model %>% pull(weight),
        data = dat_model
      )
    } else {
      model <- lm(value ~ year,
        weights = dat_model %>% pull(weight),
        data = dat_model
      )
    }
  }

  if (option == "exp") {
    if (dat_model %>% pull(year) %>% unique() %>% length() > 1) {
      model <- lmerTest::lmer(value ~ trt + (1 | year),
        weights = dat_model %>% pull(weight),
        data = dat_model
      )
    } else {
      model <- lm(value ~ trt,
        weights = dat_model %>% pull(weight),
        data = dat_model
      )
    }
  }

  # sample <- sample(residuals(model), 1000,prob =dat_model %>% drop_na() %>% pull(weight) )
  # shapiro.test(sample)
  res <- list(model = model, summary = summary(model))
  return(res)
}
