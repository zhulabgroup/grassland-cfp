source("_setup.R")

# estimate tree temp niche ------------------------------------------------
# read tree occurrences
tree_occ <- .path$tree_niche %>%
  str_c("occurrences.csv") %>%
  read_csv(col_types = "icdddddf")

# BIO1 = Annual Mean Temperature
# BIO5 = Max Temperature of Warmest Month
# BIO6 = Min Temperature of Coldest Month

# plot temp distributions
tree_occ %>%
  # filter(species %in% c("Picea mariana", "Pinus elliottii")) %>%
  ggplot(aes(bio1, color = source)) +
  geom_density() +
  facet_wrap(~species)

# summarize temp distributions
niche_sum <- tree_occ %>%
  group_by(species, source) %>%
  summarize(
    mean = mean(bio1), sd = sd(bio1),
    mid = median(bio1), lwr = quantile(bio1, .01), upr = quantile(bio1, .99),
    .groups = "drop"
  )

niche_sum %>%
  ggplot(aes(mean, mid, color = source)) +
  geom_point()

niche_sum %>%
  # filter(species %in% c("Picea mariana", "Pinus elliottii")) %>%
  ggplot(aes(x = mean, y = source)) +
  geom_pointrange(aes(xmin = lwr, xmax = upr)) +
  facet_wrap(~species)

niche_sum %>%
  ggplot(aes(x = mean)) +
  geom_histogram()
