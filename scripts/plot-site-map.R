cfp_sf <- st_read(.path$geo_cfp) %>%
  filter(
    NAME == "California Floristic Province",
    Type == "hotspot area"
  )

site_gg <- (ggplot() +
  geom_sf(
    data = rnaturalearth::ne_states(
      country = c("Mexico", "United States of America"),
      returnclass = "sf"
    ),
    fill = NA,
    color = alpha("black", .1)
  ) +
  geom_sf(data = cfp_sf, fill = "white", alpha = .5) +
  geom_sf(data = site_sf, color = "red", aes(text = name)) +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(-125, -115), ylim = c(28, 44))
) %>%
  plotly::ggplotly(tooltip = "text")
