aggregate_hexagon <- function(fia_tree, cellsize = 1) {
  # Convert to sf object
  coordinates <- st_as_sf(fia_tree %>% select(lon, lat), coords = c("lon", "lat"), crs = 4326) # Use appropriate CRS (e.g., WGS84, EPSG:4326)

  hex_grid <- st_make_grid(
    maps::map("state", plot = F, fill = T) %>%
      st_as_sf() %>%
      st_transform(crs = st_crs(coordinates)),
    cellsize = 1, square = FALSE
  ) %>%
    st_as_sf() %>%
    mutate(id = row_number())
  # plot(hex_grid)

  # Identify which points fall within which hexagons
  point_in_hex <- st_intersects(coordinates, hex_grid, sparse = T)

  fia_tree_hex <- fia_tree %>%
    mutate(hex_id = point_in_hex %>% as.numeric())

  hexagons <- st_centroid(hex_grid) %>%
    st_coordinates() %>%
    as.data.frame() %>%
    rename(hex_lon = X, hex_lat = Y) %>%
    mutate(hex_id = row_number())

  fia_tree_hex <- fia_tree_hex %>%
    left_join(hexagons, by = "hex_id")

  return(fia_tree_hex)
}




fia_cti %>%
  group_by(hexagon) %>%
  summarise(n = n())
