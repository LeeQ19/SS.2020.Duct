# Declare function to update picker input by selected data
update_picker <- function (session, data, total, choices.old, labels, id, selected) {
  con1 <- sapply(selected, function (x) length(x) == 0)
  if (any(con1)) 
    return(list(choices = choices.old, selected = selected))
  con2 <- sapply(1:length(labels), function (i) length(choices.old[[i]]) == length(selected[[i]]))
  choices <- list()
  selected.new <- list()
  for (i in 1:length(labels)) {
    temp <- data
    for (j in c(1:length(labels))[-i]) {
      if (!con2[j])
        temp <- temp[temp[[labels[j]]] %in% selected[[j]], ]
    }
    choices[[i]] <- sort(unique(as.character(temp[[labels[i]]])))
    selected.new[[i]] <- if (con2[i]) choices[[i]] else selected[[i]][selected[[i]] %in% choices[[i]]]
    updatePickerInput(session, inputId = id[i], choices = choices[[i]], selected = selected.new[[i]])
  }
  return(list(choices = choices, selected = selected.new))
}

# Declare function to make table by filtering data
make_table <- function (data, labels, selected) {
  table <- data
  for (i in 1:length(labels))
    table <- table[table[[labels[i]]] %in% selected[[i]], ]
  return(table)
}

# Declare function to match classes of product by name and standard
match_class <- function (sheet, data) {
  sheet.class <- c()
  for (i in 1:nrow(sheet)) {
    temp <- data[data$품명 == sheet[i, ]$품명, ]$대분류
    sheet.class <- c(sheet.class, temp[1])
  }
  return(cbind(대분류 = sheet.class, sheet))
}

# Declare function to make stat by matching with data
make_stat <- function (sheet, data) {
  stat <- cbind(sheet[, c(1:3, 6)], data.frame(최저가 = NA, 평균가 = NA, 중간가 = NA, 최고가 = NA))
  for (i in 1:nrow(stat)) {
    temp <- as.numeric(data[data$대분류 == stat[i, ]$대분류 & data$규격 == stat[i, ]$규격, ]$자재비)
    if (length(temp) > 0)
      stat[i, 5:8] <- round(c(min(temp), mean(temp), median(temp), max(temp)))
  }
  return(stat)
}

# Declare function to read pdf and extract data
table_making <- function(path, session = session){
  tmp_data <- pdf_data(path)
  result <- list()
  form <- readRDS("DB/labor.data.rds")[[1]]
  chapter_list <- names(form)
  for(ch in 1:length(chapter_list)){
    chapter <- chapter_list[ch]
    chapter <- strsplit(chapter,split= " ")[[1]][1]
    first <- 0
    duct_list <- NULL
    for(i in 1:length(tmp_data)){
      if(!is.na(tmp_data[[i]][1, 6])){
        if(first == 0){
          if(sum(tmp_data[[i]][ ,6] == '적용기준') > 0){
            page_difference <- i - 1
            main_main_x <-  as.integer(tmp_data[[i]][(which(tmp_data[[i]][ ,6] == '적용기준')-1) ,3])
            main_x <- as.integer(tmp_data[[i]][(which(tmp_data[[i]][ ,6] == '적용기준')+2) ,3])
            first <- 1
            for(j in (which(tmp_data[[i]][ ,6] == '적용기준')+2):nrow(tmp_data[[i]])){
              if(grepl("·", tmp_data[[i]][j ,6])){
                if(nchar(gsub('·', "", tmp_data[[i]][j ,6])) > 0){
                  sub_x <- as.integer(tmp_data[[i]][(j+1) ,3])
                } else sub_x <- as.integer(tmp_data[[i]][(j+2) ,3])
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
