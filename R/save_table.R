save_table <- function(out, dir = "alldata/output/tables/") {
  for (i in 1:length(out)) {
    tbl_name <- names(out)[i]
    param <- save_table_param(tbl_name)
    filename <- str_c(dir, "/", param$filename)
    write_csv(out[[i]], filename)
    file.copy(from = filename, to = str_c("inst/tables/"), recursive = T)
  }
}


save_table_param <- function(tbl_name) {
  if (tbl_name == "niche") {
    filename <- "species-climate-niche.csv"
  }
  if (tbl_name == "gainloss") {
    filename <- "species-abundance-change.csv"
  }

  out <- list(filename = filename)
  return(out)
}
