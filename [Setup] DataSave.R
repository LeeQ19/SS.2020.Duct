# credentials
credentials <- data.frame(
  user = c("admin"), 
  password = c("ss"), 
  stringsAsFactors = FALSE
)
saveRDS(credentials, file = "DB/credentials.rds")

# search.data
data <- read.csv("DB/Data.csv", stringsAsFactors = FALSE, fileEncoding = "euc-kr")
data$품명 <- gsub("[[:blank:][:punct:]]", "", data$품명)
search.data <- list()
search.data[[format(Sys.time(), tz = "Asia/Seoul")]] <- data
saveRDS(search.data, file = "DB/search.data.rds")

# cntrt.example
example <- read.csv("DB/AnalysisExample.csv", stringsAsFactors = FALSE, fileEncoding = "euc-kr")
saveRDS(example, file = "DB/analy.example.rds")

### Test
# rm(list = ls())
# search.data <- readRDS("DB/search.data.rds")
# example <- readRDS("DB/cntrt.example.rds")
