# download CHELSA data
param_list <- c("bio1", "bio12", "vpd_max")
for (param in param_list) {
  url <- str_c(
    "https://envicloud.wsl.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/CHELSA_",
    param,
    "_1981-2010_V.2.1.tif"
  )
  httr::GET(
    url = url,
    httr::write_disk(
      path = str_c(
        .path$cli_chelsa, param, ".tif"
      ), overwrite = TRUE
    )
  )
  print(param)
}

# https://chelsa-climate.org/wp-admin/download-page/CHELSA_tech_specification_V2.pdf
param_list_more <- c("bio5", "bio6", "bio8", "bio9", "bio10", "bio11",
                     "bio13", "bio14", "bio16", "bio17", "bio18", "bio19",
                     "vpd_mean", "vpd_min")
for (param in param_list_more) {
  url <- str_c(
    "https://envicloud.wsl.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/CHELSA_",
    param,
    "_1981-2010_V.2.1.tif"
  )
  httr::GET(
    url = url,
    httr::write_disk(
      path = str_c(
        .path$cli_chelsa, param, ".tif"
      ), overwrite = TRUE
    )
  )
  print(param)
}
