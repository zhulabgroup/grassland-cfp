# download CHELSA data
param_list <- c("bio1", "bio12", "vpd_max")
for (param in param_list) {
  url <- str_c(
    "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/CHELSA_",
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
