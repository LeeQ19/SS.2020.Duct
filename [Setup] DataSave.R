# credentials
credentials <- data.frame(user = c("admin"), 
                          password = c(scrypt::hashPassword("ss")), 
                          is_hashed_password = TRUE, 
                          comment = c("관리자"), 
                          stringsAsFactors = FALSE)
saveRDS(credentials, file = "DB/credentials.rds")

# Log
logs <- data.frame(time = c(format(Sys.time(), tz = "Asia/Seoul")), 
                   auth = c("admin"), 
                   action = c("Publish"), 
                   object = c(NA), 
                   to = c(NA))
saveRDS(logs, file = "DB/logs.rds")

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
