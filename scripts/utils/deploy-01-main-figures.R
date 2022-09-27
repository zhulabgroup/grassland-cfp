# render word docx and upload/update to google drive
# ok to run in console; background jobs set working dir to project dir
Sys.setenv(RSTUDIO_PANDOC = "/usr/lib/rstudio-server/bin/pandoc")
rmarkdown::render(
  input = "01-main-figures.Rmd",
  output_format = "bookdown::word_document2",
  output_file = paste0(tempdir(), "/Main figures.docx")
  # knit_root_dir = ".." # set to project dir
)
googledrive::drive_put(
  media = paste0(tempdir(), "/Main figures.docx"),
  path = googledrive::as_id("1cEXTonrqpdGTY-FNnI7LsY4MBX9Amkip")
)
