#' @export
test_index_change_model <- function(dat_model, option) {

  if (option == "obs") {
    if (dat_model %>% pull(site) %>% unique() %>% length() > 1) {
      model <- lmerTest::lmer(value ~ year + (1 + year | site),
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

  res <- list(model = model, summary = summary(model))
  return(res)
}

#' @export
test_trait_change_model <- function(dat_model, option) {

  if (option == "obs") {
    if (dat_model %>% pull(site) %>% unique() %>% length() > 1) {
      model <- lmerTest::lmer(value ~ year + (1 + year | site),
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

  res <- list(model = model, summary = summary(model))
  return(res)
}
