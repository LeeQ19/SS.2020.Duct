# Declare function to make indexes of data to filter
make_index <- function (data, labels) {
  n <- nrow(data)
  index <- list()
  for (label in labels) {
    x <- as.character(data[[label]])
    for (i in 1:n) {
      if (is.na(x[i]) | x[i] == "")
        index[[label]][["없음"]] <- c(index[[label]][["없음"]], i)
      else
        index[[label]][[x[i]]] <- c(index[[label]][[x[i]]], i)
    }
  }
  return(index)
}

# Declare function to update picker input by selected data
update_picker <- function (session, id, data, index, selected, old) {
  n.filter <- length(id)
  label <- names(index)
  choices.new <- old$choices
  selected.new <- selected
  if (any(sapply(selected, length) == 0)) 
    return(list(choices = choices.new, selected = selected.new))
  id.changed <- which(sapply(1:n.filter, function (i) {length(selected[[i]]) != length(old$selected[[i]])}))
  index.each <- lapply(1:n.filter, function (i) {unlist(index[[i]][selected[[i]]], use.names = FALSE)})
  for (i in c(1:n.filter)) {
    if (all(sapply(c(1:n.filter)[-i], function (j) {length(selected[[j]]) == length(old$choices[[j]])}))) {
      choices.temp <- sort(names(index[[i]]))
    } else {
      index.temp <- sort(Reduce(intersect, index.each[-i]))
      choices.temp <- unique(data[[label[i]]][index.temp])
      choices.temp[which(choices.temp == "")] <- "없음"
      choices.temp <- sort(choices.temp)
    }
    if (length(id.changed) != 0 && i != id.changed && length(old$selected[[i]]) == length(old$choices[[i]])) 
      selected.temp <- choices.temp
    else 
      selected.temp <- intersect(selected[[i]], choices.temp)
    updatePickerInput(session, inputId = id[i], choices = choices.temp, selected = selected.temp)
    choices.new[[i]] <- choices.temp
    selected.new[[i]] <- selected.temp
  }
  return(list(choices = choices.new, selected = selected.new))
}

# Declare function to make table by filtering data
make_table <- function (data, index, selected) {
  index.each <- lapply(1:length(index), function (i) {unlist(index[[i]][selected[[i]]], use.names = FALSE)})
  index.res <- sort(Reduce(intersect, index.each))
  return(data[index.res, ])
}

# Declare function to match classes of product by name and standard
match_class <- function (sheet, data, options = NULL) {
  if (!is.null(options)) {
    data <- data[data$연도 >= options$year[1] & data$연도 <= options$year[2], ]
    if (!is.null(options$sign) && options$sign) 
      data <- data[data$계약여부 == "계약", ]
    if (!is.null(options$site)) 
      data <- data[data$현장 %in% options$site, ]
    if (!is.null(options$coop)) 
      data <- data[data$협력사 %in% options$coop, ]
  }
  names <- gsub("[[:blank:][:punct:]]", "", sheet$품명)
  sheet.class <- c()
  for (i in 1:nrow(sheet)) {
    temp <- data[data$품명 == names[i], ]$대분류
    sheet.class <- c(sheet.class, temp[1])
  }
  return(cbind(대분류 = sheet.class, sheet))
}

# Declare function to make stat by matching with data
make_stat <- function (sheet, data, options = NULL, Download = TRUE) {
  if (!is.null(options)) {
    data <- data[data$연도 >= options$year[1] & data$연도 <= options$year[2], ]
    if (!is.null(options$sign) && options$sign) 
      data <- data[data$계약여부 == "계약", ]
    if (!is.null(options$site)) 
      data <- data[data$현장 %in% options$site, ]
    if (!is.null(options$coop)) 
      data <- data[data$협력사 %in% options$coop, ]
  }
  stat <- cbind(sheet[, c(1:3, 6)], data.frame(최저가 = NA, 평균가 = NA, 중간가 = NA, 최고가 = NA, 가격차이 = NA))
  if(Download == FALSE) stattemp <- stat
  for (i in 1:nrow(stat)) {
    temp <- na.omit(as.numeric(data[data$대분류 == stat[i, ]$대분류 & data$규격 == stat[i, ]$규격, ]$자재비.단가))
    if (length(temp) > 0){
      if(Download == TRUE)
        stat[i, 5:9] <- round(c(min(temp), mean(temp), median(temp), max(temp), as.integer(stat$자재비.단가[i]) - median(temp)))
      else {
        stattemp[i, 5:9] <- round(c(min(temp), mean(temp), median(temp), max(temp), as.integer(stat$자재비.단가[i]) - median(temp)))
        temptemp <- data[data$대분류 == stat[i, ]$대분류 & data$규격 == stat[i, ]$규격, ]
        stat[i, 5:9] <- c(paste0('<span title="', 
                                 temptemp[which.min(temp),]$연도, '\n',
                                 temptemp[which.min(temp),]$현장, '\n',
                                 temptemp[which.min(temp),]$협력사, '\n',
                                 temptemp[which.min(temp),]$계약번호, '">', paste0(comma(round(min(temp)), format = 'd'), ' ￦'), '</span>'), 
                          paste0(comma(round(mean(temp)), format = 'd'), ' ￦'), 
                          paste0(comma(round(median(temp)), format = 'd'), ' ￦'), 
                          paste0('<span title="', 
                                 temptemp[which.max(temp),]$연도, '\n',
                                 temptemp[which.max(temp),]$현장, '\n',
                                 temptemp[which.max(temp),]$협력사, '\n',
                                 temptemp[which.max(temp),]$계약번호, '">', paste0(comma(round(max(temp)), format = 'd'), ' ￦'), '</span>'),
                          round(as.integer(stat$자재비.단가[i]) - median(temp)))
      }
      
    }
    
  }
  if(Download == FALSE){
    for(j in 4:9){
      stat[, j] <- factor(stat[, j], levels = unique(stat[order(stattemp[ ,j]), j]))
    }
  }
  
  return(stat)
}

# Declare function to save log
save_log <- function (logs, auth, action, object = NA, to = NA) {
  if (is.null(reactiveValuesToList(auth)$user)) return(logs)
  substr(action, 1, 1) <- toupper(substr(action, 1, 1))
  logs <- rbind(logs, c(format(Sys.time(), tz = "Asia/Seoul"), reactiveValuesToList(auth)$user, action, object, to))
  saveRDS(logs, file = "DB/logs.rds")
  return(logs)
}

# Declare function to print logs
print_log <- function (logs) {
  text <- c()
  for (i in 1:nrow(logs)) {
    log <- logs[i, ]
    if (is.na(log$object))
      text <- c(text, paste(paste0("[", log[1], "]"), paste0(log[2], ":"), log[3]))
    else if (is.na(log$to))
      text <- c(text, paste(paste0("[", log[1], "]"), paste0(log[2], ":"), log[3], log[4]))
    else
      text <- c(text, paste(paste0("[", log[1], "]"), paste0(log[2], ":"), log[3], log[4], "to", log[5]))
  }
  return(paste0(text, collapse = "\n"))
}

# Declare function to read pdf and extract data
read_pdf <- function (path, session = session) {
  tmp_data <- pdf_data(path)
  result <- list()
  form <- readRDS("DB/labor.data.rds")[[1]]
  chapter_list <- names(form)
  for (ch in 1:length(chapter_list)) {
    chapter <- chapter_list[ch]
    chapter <- strsplit(chapter,split= " ")[[1]][1]
    first <- 0
    duct_list <- NULL
    for (i in 1:length(tmp_data)) {
      if (!is.na(tmp_data[[i]][1, 6])) {
        if (first == 0) {
          if (sum(tmp_data[[i]][, 6] == "적용기준") > 0) {
            page_difference <- i - 1
            main_main_x <-  as.integer(tmp_data[[i]][(which(tmp_data[[i]][, 6] == "적용기준") - 1) ,3])
            main_x <- as.integer(tmp_data[[i]][(which(tmp_data[[i]][, 6] == "적용기준") + 2) ,3])
            first <- 1
            for (j in (which(tmp_data[[i]][, 6] == "적용기준") + 2):nrow(tmp_data[[i]])) {
              if (grepl("·", tmp_data[[i]][j, 6])) {
                if(nchar(gsub("·", "", tmp_data[[i]][j ,6])) > 0){
                  sub_x <- as.integer(tmp_data[[i]][j + 1, 3])
                } else sub_x <- as.integer(tmp_data[[i]][j + 2, 3])
                break()
              }
            }
          }
        }
        if(sum(tmp_data[[i]][ ,6] == chapter) > 0 && sum(tmp_data[[i]][(which(tmp_data[[i]][ ,6] == chapter)-1), 3] == main_main_x) > 0){
          duct_list <- i
          
          if(which(tmp_data[[i]][ ,3] == main_main_x)[length(which(tmp_data[[i]][ ,3] == main_main_x))] %in% (which(tmp_data[[i]][ ,6] == chapter)-1)){
            contents <- tmp_data[[i]][(1+which(tmp_data[[i]][ ,3] == main_main_x)[length(which(tmp_data[[i]][ ,3] == main_main_x))]):nrow(tmp_data[[i]]), ]
            next()
          } else {
            contents <- tmp_data[[i]][which(tmp_data[[i]][ ,6] == chapter):which(tmp_data[[i]][ ,3] == main_main_x)[length(which(tmp_data[[i]][ ,3] == main_main_x))], ]
            break()
          }
          
        }
        if(!is.null(duct_list)){
          
          if(sum(tmp_data[[i]][ ,3] == main_main_x) > 0){
            contents <- rbind(contents, tmp_data[[i]][1:which(tmp_data[[i]][ ,3] == main_main_x)[1], ])
            break()
            
            
          } else {
            contents <- rbind(contents, tmp_data[[i]])
          }
        }
        
      }
    }
    table_list <- list()
    table_list_x <- form[[chapter_list[ch]]]
    cn <- 1
    for(i in 1:length(which(contents[ ,3] == main_x))){
      if(! grepl('-', contents[which(contents[ ,3] == main_x)[i], 6])){
        next()
      }
      ii <- which(contents[ ,3] == main_x)[i]
      for(j in which(contents[ ,3] == sub_x)[which(contents[ ,3] == sub_x) > ii]){
        if(i != length(which(contents[ ,3] == main_x))){
          if(j > which(contents[ ,3] == main_x)[(i+1)]){
            break()
          }
        }
        tmp <- contents[(ii+1) ,6]
        for(iii in (ii+2):j){
          if(grepl('·', contents[(iii) ,6])){
            break()
          }
          
          if(!grepl('[[:digit:]]', contents[(iii) ,6])){
            tmp <- paste(tmp, contents[(iii) ,6])
          }
          
        }
        tmp <- paste(tmp, contents[(j+1) ,6])
        k <- j+1
        while(1){
          if(grepl('·', contents[(k+1) ,6])){
            if(nchar(gsub('·', "", contents[(k+1) ,6])) > 0){
              page <- gsub('·', "", contents[(k+1) ,6])
            } else{
              page <- contents[(k+2) ,6]
            }
            break()
          }
          tmp <- paste(tmp, contents[(k+1) ,6])
          k <- k + 1
        }
        if(chapter == '보온공사' & i != 3){
          next()
        }
        table_tmp <- table_list_x[[cn]]
        
        data_list <- tmp_data[[(as.integer(page) + page_difference)]]
        data_list <- data_list[which(data_list$y > as.integer(data_list[which(data_list$text == as.character(contents[(j),6])), 4][1, ])), ]
        
        rep <- list()
        for(k in 1:nrow(table_tmp)){
          if(chapter != '보온공사'){
            if(grepl(" ", table_tmp[k, which(colnames(table_tmp) == '규격')]) ){
              stv <- strsplit(table_tmp[k, which(colnames(table_tmp) == '규격')], " ")[[1]][2]
              t <- 0
            } else {
              stv <- table_tmp[k, which(colnames(table_tmp) == '규격')]
              t <- 0
            }
            if(stv == ""){
              t <- 1
            }
          } else {
            t <- 1
          }
          
          if(t == 0) {
            if(length(which(data_list$text == stv)) == 0) {
              if(!length(which(data_list$text == gsub("″", "", stv))) == 0){
                stv <- gsub("″", "", stv)
              } else {
                data_list <- tmp_data[[1 + (as.integer(page) + page_difference)]]
              }
            }
            
            yval <- data_list[which(data_list$text == stv), ]$y
            if(length(yval) > 1){
              if(is.null(rep[[stv]])){
                yval <- yval[1]
                rep[[stv]] <- 1
              } else {
                rep[[stv]] <- rep[[stv]] + 1
                yval <- yval[rep[[stv]]]
              }
            }
            
            for(jj in  which(colnames(table_tmp) == '규격'):(ncol(table_tmp)-1)){
              xval <- data_list[data_list$y == yval, ]$x
              xvalc <- which(xval %in% data_list[which(data_list$text == stv), ]$x) + jj - which(colnames(table_tmp) == '규격') + 1
              if(!xvalc > length(xval)){
                table_tmp[k,(jj+1)] <- data_list[which(data_list$y == yval & data_list$x == xval[xvalc] ), 6]
              } else {
                table_tmp[k,(jj+1)] <- 0
              }
            } 
          } else {
            if(chapter == '덕트공사'){
              if(grepl(" ", table_tmp[k, which(colnames(table_tmp) == '구분')]) ){
                stv <- strsplit(table_tmp[k, which(colnames(table_tmp) == '구분')], " ")[[1]][2]
              } else {
                stv <- table_tmp[k, which(colnames(table_tmp) == '구분')]
              }
              data_list_t <- data_list[which(data_list$y > as.integer(data_list[which(data_list$text == stv), 4])), ]
              for(jj in  which(colnames(table_tmp) == '규격'):(ncol(table_tmp)-1)){
                yval <- data_list_t[which(data_list_t$text == substr(colnames(table_tmp)[(jj+1)], start = 1, stop = 1)), ]$y
                xval <- data_list_t[data_list_t$y == yval, ]$x
                table_tmp[k,(jj+1)] <- data_list_t[which(data_list_t$y == yval & data_list_t$x == xval[length(xval)] ), 6]
              }
            } else{
              for(jj in (1+which(colnames(table_tmp) == '규격')):ncol(table_tmp)){
                yval <- c(data_list$y[which((data_list$text == substr(colnames(table_tmp)[(jj)], start = 1, stop = 1)))])
                for(nc in 2:nchar(colnames(table_tmp)[(jj)])){
                  yval_t <- yval
                  yval <- c(yval_t[which(yval_t %in% data_list$y[which((data_list$text == substr(colnames(table_tmp)[(jj)], start = nc, stop = nc)))])])
                }
                text_list <- data_list$text[which(data_list$y ==yval[1])]
                table_tmp[, jj] <- text_list[(length(text_list)+1-nrow(table_tmp)):length(text_list)]
              }
            }
          }
        }
        table_list[[tmp]] <- table_tmp
        cn <- cn + 1
      }
    }
    result[[chapter_list[ch]]] <- table_list
  }
  return(result)
}

match_table <- function(table, data){
  table[,1] <- as.factor(as.character(table[,1]))
  table[,2] <- as.factor(as.character(table[,2]))
  fileinfo <- c(ncol(table)-1, ncol(table))
  while(sum(duplicated(table[, c(1, 2)])) > 0){
    from <- which(duplicated(table[,c(1:2)]))[1]
    temp <- table[-from, ]
    to <- which(temp[ ,1] == table[from, 1] & temp[ ,2] == table[from, 2])[1]
    
    filefrom <- strsplit(as.character(table[from, fileinfo[1]]), split=",")[[1]]
    rowfrom <- strsplit(as.character(table[from, fileinfo[2]]), split=",")[[1]]
    fileto <- strsplit(as.character(temp[to, fileinfo[1]]), split=",")[[1]]
    rowto <- strsplit(as.character(temp[to, fileinfo[2]]), split=",")[[1]]
    
    valuetable <- mat.or.vec(nr = length(filefrom)+length(fileto), nc = (1 + ncol(table) - 4))
    
    for(i in 1:length(filefrom)){
      filetemp <- as.integer(filefrom[i])
      rowtemp <- as.integer(rowfrom[i])
      col <- seq(from = (1 + filetemp), by = length(data), length.out = 3)
      valuetable[i, c(1, col)] <- as.integer(valuetable[i, c(1, col)]) + as.integer(data[[filetemp]][rowtemp, c(5, 6, 8, 10)])
    }
    for(i in 1:length(fileto)){
      filetemp <- as.integer(fileto[i])
      rowtemp <- as.integer(rowto[i])
      col <- seq(from = (1+filetemp), by = length(data), length.out = 3)
      valuetable[(length(filefrom)+i), c(1, col)] <- as.integer(valuetable[(length(filefrom)+i), c(1, col)]) + as.integer(data[[filetemp]][rowtemp, c(5, 6, 8, 10)])
    }
    temp[to, -c(1:2, fileinfo)] <-  as.integer(colSums(valuetable[ ,1] * valuetable[ ,-1])/sum(valuetable[ ,1]))
    temp[to, fileinfo] <- paste0(temp[to, fileinfo], ',', table[from, fileinfo])
    table <- temp
  }
  table[,1] <- as.character(table[,1])
  table[,2] <- as.character(table[,2])
  return(table)
}

# Add column for graph and select row
make_graphdata <- function(data){
  return(data %>% (function (x) {x[(complete.cases(x[ , c("자재비.단가")]) & x[ , c("자재비.단가")] > 0), ]}) %>% mutate(newsite = paste(현장, '-', 협력사, '-', 연도)))
}
