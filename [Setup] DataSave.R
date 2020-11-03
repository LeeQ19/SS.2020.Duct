# credentials
credentials <- data.frame(user = c("admin"), 
                          password = c(scrypt::hashPassword("ss")), 
                          is_hashed_password = TRUE, 
                          comment = c("관리자"), 
                          stringsAsFactors = FALSE)
save_data(credentials)

# logs
logs <- data.frame(time = c(format(Sys.time(), tz = "Asia/Seoul")), 
                   auth = c("admin"), 
                   action = c("Publish"), 
                   object = c(NA), 
                   to = c(NA))
save_data(logs)

# search.data
name <- "Origin"
data <- read.csv("DB/Data.csv", stringsAsFactors = FALSE, fileEncoding = "euc-kr")
data$품명 <- gsub("[[:blank:][:punct:]]", "", data$품명)
search.data <- list()
search.data[[name]] <- data
save_data(search.data)

# search.index
index <- make_index(data, c("대분류", "규격", "연도", "현장", "협력사", "계약번호", "계약여부"))
search.index <- list()
search.index[[name]] <- index
save_data(search.index)

# example to get index overlapped
Reduce(intersect, list(unlist(index[[1]][["대분류"]][c("덕트", "덕트_보온")], use.names = FALSE), unlist(index[[1]][["연도"]][c("2019")], use.names = FALSE)))

# analy.example
analy.example <- read.csv("DB/AnalysisExample.csv", stringsAsFactors = FALSE, fileEncoding = "euc-kr")
save_data(analy.example)

# labor.data
labor.data <- readRDS("DB/labor.data.rds")
save_data(labor.data)

# cmp.example
cmp.example <- readRDS("DB/cmp.example.rds")
save_data(cmp.example)

# analy.example
analy.example <- readRDS("DB/analy.example.rds")
save_data(analy.example)
