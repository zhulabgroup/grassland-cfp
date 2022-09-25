# render word docx and upload/update to google drive
# ok to run in console; background jobs set working dir to project dir
rmarkdown::render(
  input = "02-supp-figures.Rmd",
  output_format = "bookdown::word_document2",
  output_file = paste0(tempdir(), "/Supplementary figures.docx")
  # knit_root_dir = ".." # set to project dir
)
googledrive::drive_put(
  media = paste0(tempdir(), "/Supplementary figures.docx"),
  path = googledrive::as_id("1cEXTonrqpdGTY-FNnI7LsY4MBX9Amkip")
)
