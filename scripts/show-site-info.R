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
