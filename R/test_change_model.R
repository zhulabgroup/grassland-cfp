test_index_change_model <- function(dat_lme, index, option) {
  # if (index == "cpi") {
  #   dat_lme <- dat_lme %>%
  #     mutate(value = rank(value))
  # }

  if (option == "obs") {
    if (dat_lme %>% pull(site) %>% unique() %>% length() > 1) {
      model <- lmerTest::lmer(value ~ year + (1 | site),
        data = dat_lme
      )
    } else {
      model <- lm(value ~ year,
        data = dat_lme
      )
    }
  }

  if (option == "exp") {
    model <- lmerTest::lmer(value ~ trt + (1 | year),
      data = dat_lme
    )
  }

  # sample <- sample(residuals(model), 1000,prob =dat_lme %>% drop_na() %>% pull(weight) )
  # shapiro.test(sample)
  res <- list(model = model, summary = summary(model))
  return(res)
}


test_trait_change_model <- function(dat_lme, trait, option) {
  # if (trait == "ppt") {
  #   dat_lme <- dat_lme %>%
  #     mutate(value = rank(value))
  # }

  if (option == "obs") {
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

  if (option == "exp") {
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

  # sample <- sample(residuals(model), 1000,prob =dat_lme %>% drop_na() %>% pull(weight) )
  # shapiro.test(sample)
  res <- list(model = model, summary = summary(model))
  return(res)
}
