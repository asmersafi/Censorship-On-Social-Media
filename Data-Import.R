
files <- list.files(path = "~/Raw-Data/Removal Requests/", pattern = "*.csv", full.names = T)

tbl <- sapply(files, read_csv, simplify=FALSE) %>% 
  bind_rows(.id = "id")
