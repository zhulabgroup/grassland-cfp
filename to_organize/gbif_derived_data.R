library(grassland)
v_key <- read_rds("alldata/input/biogeography/gbif-2023-08-02.rds") %>%
  pull(key) %>%
  as.numeric()

num_part <- 35
ls_v_key <- split(v_key, cut(seq_along(v_key), num_part, labels = FALSE))

library(doSNOW)
cl <- makeCluster(num_part)
registerDoSNOW(cl)

ls_df_dataset_key <-
foreach(v_key_sub = ls_v_key,
        .packages = c("tidyverse", "rgbif"))%dopar% {
  ls_df_dataset_key_sub <- vector(mode = "list")
  for (i in 1:length(v_key_sub)) {
    res <- tryCatch(
      {
        rgbif::occ_get_verbatim(key = v_key_sub[i], fields = c("key","datasetKey"), curlopts = list(verbose = F))
      },
      error = function(e) {
        cat(sprintf("Error in operation %d: %s\n", i, e$message))
        return(NULL)
      }
    )
    ls_df_dataset_key_sub[[i]] <- res
    print(i)
  }

  ls_df_dataset_key_sub %>%
    bind_rows()
}

df_dataset_key <- ls_df_dataset_key %>%
  bind_rows() %>%
  as_tibble()

df_dataset_key %>% write_csv("alldata/input/biogeography/gbif-derived-data.csv")

df_dataset_key%>%
  group_by(datasetKey) %>%
  summarise(count = n()) %>%
  write_csv("alldata/input/biogeography/gbif-derived-data-summ.csv")
