# upload/update figure docs to google drive
library(googledrive)
drive_put("figures-main.docx",
  path = "1cEXTonrqpdGTY-FNnI7LsY4MBX9Amkip" %>%
    as_id() %>%
    as_dribble()
)
drive_put("figures-supp.docx",
  path = "1cEXTonrqpdGTY-FNnI7LsY4MBX9Amkip" %>%
    as_id() %>%
    as_dribble()
)
