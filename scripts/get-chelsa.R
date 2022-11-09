# download CHELSA climatology data, 1981-2010
# https://chelsa-climate.org/wp-admin/download-page/CHELSA_tech_specification_V2.pdf
param_list <- c(str_c("bio", 1:19), str_c("vpd_", c("max", "mean", "min", "range")))

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
