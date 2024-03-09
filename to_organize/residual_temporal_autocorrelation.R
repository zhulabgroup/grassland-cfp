model <- lmerTest::lmer(
  tmp_com_mean ~ year + (1 + year | site),
  data = dat_index$obs %>% mutate(year = year - 1983)
)

simulationOutput <- DHARMa::simulateResiduals(fittedModel = model)
plot(simulationOutput)

v_site <- dat_index$obs %>%
  pull(site) %>%
  unique() %>%
  sort()
ls_df_ar <- vector(mode = "list")
for (siteoi in v_site) {
  id <- dat_index$obs %>%
    mutate(id = row_number()) %>%
    filter(site == siteoi) %>%
    pull(id)

  simulationOutput_site <- DHARMa::createDHARMa(
    simulatedResponse = simulationOutput$simulatedResponse[id, ],
    observedResponse = dat_index$obs$tmp_com_mean[id],
    fittedPredictedResponse = simulationOutput$fittedPredictedResponse[id]
  )

  simulationOutput_agg <- DHARMa::recalculateResiduals(simulationOutput_site, group = dat_index$obs$year[id], aggregateBy = mean)

  p_value <- tryCatch(
    {
      autocorrelationTest <- DHARMa::testTemporalAutocorrelation(simulationOutput = simulationOutput_agg, time = dat_index$obs$year[id] %>% `-`(1983) %>% unique())
      autocorrelationTest$p.value
    },
    error = function(e) {
      NA
    }
  )

  ls_df_ar[[siteoi]] <- data.frame(site = siteoi, p_value = p_value) %>%
    mutate(sig = if_else(p_value <= 0.05, "*", "ns")) %>%
    mutate(p_value = round(p_value, digits = 4))
}

bind_rows(ls_df_ar)
