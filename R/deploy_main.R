# render word docx and upload/update to google drive
rmarkdown::render(
  knit_root_dir = "..", # from R/ to project dir
  input = "../vignettes/main_figures.Rmd",
  output_format = "bookdown::word_document2",
  output_file = paste0(tempdir(), "/Main figures.docx")
)
googledrive::drive_put(
  media = paste0(tempdir(), "/Main figures.docx"),
  path = googledrive::as_id("1cEXTonrqpdGTY-FNnI7LsY4MBX9Amkip")
)
