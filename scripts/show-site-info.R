# read site data
site_sf <- read_rds(.path$geo_site) %>%
  filter(abbr %in% c("angelo", "carrizo", "elkhorn", "jasper", "mclann", "morganterritory", "pleasantonridge", "sunol", "swanton", "ucsc", "vascocaves")) %>%
  arrange(abbr) %>%
  add_column(lab = c(LETTERS[1:4], "E/F", LETTERS[7:12]))

# make site table
site_tbl <- site_sf %>%
  extract(geometry, c("latitude", "longitude"), "\\((.*), (.*)\\)", convert = TRUE) %>%
  select(label = lab, name, latitude, longitude, grass_type, data_method) %>%
  mutate(
    name = if_else(label == "E/F", str_c(name, "/Serpentine"), name),
    grass_type = if_else(label == "E/F", str_c(grass_type, "/Serpentine"), grass_type)
  )

# read cfp data
cfp_sf <- st_read(.path$geo_cfp, quiet = TRUE) %>%
  filter(
    NAME == "California Floristic Province",
    Type == "hotspot area"
  )

# read grassland raster
sds <- terra::sds(paste0(.path$geo_grass, "MCD12C1.A2020001.006.2021362215328.hdf"))
lc_ras <- raster::raster(sds[1])
raster::extent(lc_ras) <- c(-180, 180, -90, 90)
raster::projection(lc_ras) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
lc_ras <- raster::flip(lc_ras)
lc_ras <- raster::crop(lc_ras, raster::extent(cfp_sf))
lc_ras <- raster::mask(lc_ras, cfp_sf)

grass_ras <- lc_ras
grass_ras[grass_ras != 10] <- NA # User guide at https://lpdaac.usgs.gov/documents/101/MCD12_User_Guide_V6.pdf
grass_ras[grass_ras == 10] <- 1
grass_df <- raster::as.data.frame(grass_ras, xy = T) %>%
  drop_na()

# make map
set.seed(618)
site_gg <-
  ggplot() +
  geom_sf(
    data = rnaturalearth::ne_states(
      country = c("Mexico", "United States of America"),
      returnclass = "sf"
    ),
    fill = NA,
    color = alpha("black", .1)
  ) +
  geom_sf(
    data = cfp_sf,
    color = alpha("black", .3),
    fill = alpha("white", .1)
  ) +
  geom_raster(
    data = grass_df,
    aes(x = x, y = y),
    fill = "yellow green"
  ) +
  geom_sf(data = site_sf, color = "red") +
  ggrepel::geom_label_repel(
    data = site_sf,
    mapping = aes(
      label = lab, # name,
      geometry = geometry
    ),
    stat = "sf_coordinates",
    size = 3,
    color = "red",
    fill = NA,
    min.segment.length = 0,
    max.overlaps = Inf,
    label.padding = unit(.25, "lines"),
    label.size = NA
  ) +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(-126, -114), ylim = c(28, 44)) +
  scale_x_continuous(breaks = c(-125, -120, -115)) +
  scale_y_continuous(breaks = c(30, 35, 40))
