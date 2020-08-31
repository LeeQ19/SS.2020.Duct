# search.data
data <- read.csv("DB/Data.csv", stringsAsFactors = FALSE, fileEncoding = "euc-kr")
search.data <- list()
search.data[[format(Sys.time(), tz = "Asia/Seoul")]] <- data
saveRDS(search.data, file = "DB/search.data.rds")

# price.info
info <- read.csv("DB/Price Info.csv", stringsAsFactors = FALSE, fileEncoding = "euc-kr")
price.info <- list()
price.info[[format(Sys.time())]] <- info
saveRDS(price.info, file = "DB/price.info.rds")

# cntrt.example
example <- read.csv("DB/Contract Example.csv", stringsAsFactors = FALSE, fileEncoding = "euc-kr")
saveRDS(example, file = "DB/cntrt.example.rds")

### Test
# rm(list = ls())
# search.data <- readRDS("DB/search.data.rds")
# price.info <- readRDS("DB/price.info.rds")
