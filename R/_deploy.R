# render word docx and upload/update to google drive
rmarkdown::render(
  input = "../figures-main.Rmd",
  output_format = "bookdown::word_document2",
  output_dir = "../tests"
)
googledrive::drive_put(
  media = "../tests/figures-main.docx",
  path = googledrive::as_id("1cEXTonrqpdGTY-FNnI7LsY4MBX9Amkip")
)
