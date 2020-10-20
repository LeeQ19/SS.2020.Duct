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
name <- "Origin"
data <- read.csv("DB/Data.csv", stringsAsFactors = FALSE, fileEncoding = "euc-kr")
data$품명 <- gsub("[[:blank:][:punct:]]", "", data$품명)
search.data <- list()
search.data[[name]] <- data
saveRDS(search.data, file = "DB/search.data.rds")
index <- make_index(data, c("대분류", "규격", "연도", "현장", "협력사", "계약번호", "계약여부"))
search.index <- list()
search.index[[name]] <- index
saveRDS(search.index, file = "DB/search.index.rds")

# example to get index overlapped
Reduce(intersect, list(unlist(index[[1]][["대분류"]][c("덕트", "덕트_보온")], use.names = FALSE), unlist(index[[1]][["연도"]][c("2019")], use.names = FALSE)))

# cntrt.example
example <- read.csv("DB/AnalysisExample.csv", stringsAsFactors = FALSE, fileEncoding = "euc-kr")
saveRDS(example, file = "DB/analy.example.rds")
