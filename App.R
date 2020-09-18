#########################################################################################################################
### Set up environment
#########################################################################################################################
# Load library
library("dplyr")
library("DT")
library("pdftools")
library("shiny")
library("shinydashboard")
library("shinyWidgets")
library("shinyjs")
library("shinyalert")
library("xlsx")
options(shiny.maxRequestSize = 30*1024^2)

# Load function
source("Module.R", encoding = "UTF-8")

# Load data
search.data   <- readRDS("DB/search.data.rds")
search.labels <- c("대분류", "규격", "연도", "현장", "협력사", "계약번호", "계약여부")
search.id     <- c("search.major", "search.std", "search.year", "search.site", "search.coop", "search.num", "search.cntrt")
search.total  <- sapply(search.labels, function (x) sort(unique(search.data[[length(search.data)]][[x]])))
search.width  <- c(2, 2, 1, 2, 2, 2, 1)

labor.data <- readRDS("DB/labor.data.rds")

#########################################################################################################################
### Define UI
#########################################################################################################################
ui <- dashboardPage(
  #########################################################
  ### Header content
  #########################################################
  dashboardHeader(title = "덕트 구매지원 시스템"), 
  
  #########################################################
  ### Sidebar content (tab)
  #########################################################
  dashboardSidebar(
    sidebarMenu(
      menuItem("기계약 검색", tabName = "search", icon = icon("search")), 
      menuItem("입찰 분석",   tabName = "bid",    icon = icon("chart-bar")), 
      menuItem("기계약 분석", tabName = "cntrt",  icon = icon("dashboard")), 
      menuItem("품셈 정보",   tabName = "labor",  icon = icon("file-invoice"))
    ), 
    tags$footer("ver 2.6.0", align = "right", style = "font-size: 15px; position:absolute; bottom:0; width:100%; padding:10px")
  ), 
  
  #########################################################
  ### Body content
  #########################################################
  dashboardBody(
    useShinyjs(), 
    useShinyalert(), 
    tabItems(
      ############################
      ### Search tab content
      ############################
      tabItem(
        tabName = "search", 
        # Select box with check boxes (excel-like filter)
        fluidRow(box(
          lapply(c(1:7), function (i) column(search.width[i], 
                                             pickerInput(search.id[i], paste0(search.labels[i], ":"), choices = search.total[[i]], selected = search.total[[i]], 
                                                         options = list(`actions-box` = TRUE, `selected-text-format` = "count > 2", `count-selected-text` = "선택: {0} / 전체: {1}", 
                                                                        `select-all-text` = "전체 선택", `deselect-all-text` = "선택 해제", `live-search`  = TRUE), multiple = TRUE))), 
          width = NULL)),
        
        # Create a new row for table
        DT::dataTableOutput("search.table"), 
        
        # Modify data
        fluidRow(
          box(
            column(3, 
                   fileInput("search.upload", "엑셀 파일 업로드", accept = ".xlsx"), 
                   div(style = "margin-top: -20px;"), 
                   disabled(actionButton("search.update", "업데이트", icon = icon("cloud-upload"))), 
                   div(style = "float: right;", disabled(actionButton("search.reset", "초기화", icon = icon("undo"))))
            ), 
            column(6), 
            column(3, 
                   selectInput("search.index", "데이터 선택", choices = rev(names(search.data))), 
                   disabled(actionButton("search.save", "수정내용 저장", icon = icon("save"))), 
                   disabled(actionButton("search.discard", "수정내용 초기화", icon = icon("undo"))), 
                   div(style = "float: right;", 
                       downloadButton("search.download", "다운로드"), 
                       actionButton("search.remove", "데이터 삭제", icon = icon("trash")))
            ), 
            width = NULL
          )
        ), 
        
        # Create a new row for uploaded sheet
        DT::dataTableOutput("search.sheet")
      ), 
      
      ############################
      ### Bid tab content
      ############################
      tabItem(
        tabName = "bid", 
        h1("입찰 분석 탭 내용")
      ), 
      
      ############################
      ### Contract tab content
      ############################
      tabItem(
        tabName = "cntrt", 
        # Upload file
        fluidRow(box(
          # Input: select a file
          column(6, 
                 fileInput("cntrt.sheet", "엑셀 파일 업로드", accept = ".xlsx"), 
                 downloadButton("cntrt.example", "예시파일")
          ), 
          
          # Options
          column(2, 
                 div(style = "margin-top: 20px; margin-bottom: 30px;", checkboxInput("cntrt.sign", "계약건 한정",   value = FALSE)), 
                 tags$style(".irs-grid-pol.small {height: 0px;}"), 
                 sliderInput("cntrt.year", "분석기간", min = min(search.total[[3]]), max = max(search.total[[3]]), value = c(min(search.total[[3]]), max(search.total[[3]])), step = 1, width = "75%", sep = "", post = "년")
          ),  
          column(1, 
                 fluidRow(pickerInput("cntrt.site", "현장:", choices = search.total[[4]], selected = search.total[[4]], 
                                      options = list(`actions-box` = TRUE, `selected-text-format` = "count > 2", `count-selected-text` = "선택: {0} / 전체: {1}", 
                                                     `select-all-text` = "전체 선택", `deselect-all-text` = "선택 해제", `live-search`  = TRUE), multiple = TRUE), 
                          pickerInput("cntrt.coop", "협력사:", choices = search.total[[5]], selected = search.total[[5]], 
                                      options = list(`actions-box` = TRUE, `selected-text-format` = "count > 2", `count-selected-text` = "선택: {0} / 전체: {1}", 
                                                     `select-all-text` = "전체 선택", `deselect-all-text` = "선택 해제", `live-search`  = TRUE), multiple = TRUE))
          ), 
          width = NULL)), 
        
        # Create a new row for table
        DT::dataTableOutput("cntrt.stat"), 
        
        # Download button for table
        box(
          div(style = "float: right;", disabled(downloadButton("cntrt.download", "다운로드"))), 
          width = NULL
        )
      ), 
      
      ############################
      ### Labor tab content
      ############################
      tabItem(
        tabName = "labor", 
        # Select box to search data
        fluidRow(box(
          column(3, selectInput("labor.chapter", "챕터 선택", choices = names(labor.data[[length(labor.data)]]))),
          column(3, selectInput("labor.detail", "상세 선택", choices = names(labor.data[[length(labor.data)]][[1]]))),
          width = NULL)),
        
        # Create a new row for table
        DT::dataTableOutput("labor.table"), 
        div(style = "margin-top: 50px;"), 
        
        # Update data
        fluidRow(
          # tags$style("#labor.upload .shiny-file-input-progress {display: none}"),
          box(
            column(9, 
                   fileInput("labor.upload", "PDF 파일 업로드", accept = ".pdf"), 
                   progressBar(id = "labor.progress", value = 0, display_pct = T), 
                   disabled(actionButton("labor.update", "업데이트", icon = icon("cloud-upload-alt"))), 
                   div(style = "float: right;", disabled(actionButton("labor.reset", "초기화", icon = icon("undo"))))
            ),
            column(3, 
                   selectInput("labor.index", "데이터 선택", choices = rev(names(labor.data))), 
                   div(style = "float: right;", actionButton("labor.remove", "데이터 삭제", icon = icon("trash")))
            ), 
            width = NULL
          )
        )
      )
    )
  )
)

#########################################################################################################################
### Define server logic to summarize and view selected data
#########################################################################################################################
server <- function(input, output, session) {
  #########################################################
  ### Set environment
  #########################################################
  # Custom sidebar collapse
  runjs({'
          var el2 = document.querySelector(".skin-blue");
          el2.className = "skin-blue sidebar-mini";
          var clicker = document.querySelector(".sidebar-toggle");
          clicker.id = "switchState";
    '})
  
  onclick('switchState', runjs({'
                                 var title = document.querySelector(".logo")
                                 if (title.style.visibility == "hidden") {
                                   title.style.visibility = "visible";
                                 } else {
                                   title.style.visibility = "hidden";
                                 }
  '}))
  
  # Define memory
  memory <- reactiveValues(search.old   = list(choices = search.total, selected = search.total, total = search.total), 
                           search.table = search.data[[length(search.data)]], 
                           search.data  = search.data[[length(search.data)]])
  
  #########################################################
  ### Search tab
  #########################################################
  # Button preset
  if (length(search.data) == 1) 
    disable("search.remove")
  
  # Update select box contents
  observeEvent(c(sapply(search.id, function (x) list(input[[x]])), search.data), {
    search.selected <- sapply(search.id, function (x) list(input[[x]]))
    memory$search.old <- update_picker(session, memory$search.data[search.labels], memory$search.old$total, memory$search.old$choices, search.labels, search.id, search.selected)
  })
  
  # Update table
  observeEvent(c(memory$search.old$selected, input$search.index), {
    # Filter data based on selections
    memory$search.table <- make_table(memory$search.data[-c(2)], search.labels, memory$search.old$selected)
    
    # Render table
    output$search.table <- DT::renderDataTable(datatable(isolate(memory$search.table), extensions = "FixedHeader", editable = list(target = "cell", disable = list(columns = c(0))), 
                                                         options = list(fixedHeader = TRUE, lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "전체")), pageLength = 10))
                                               %>% formatCurrency(c("자재비.단가", "노무비.단가", "경비.단가"), currency = ' ￦', interval = 3, mark = ',', digit = 0, before = FALSE))
    memory$search.table.proxy <- dataTableProxy("search.table")
  })
  
  # Edit table
  observeEvent(input$search.table_cell_edit, {
    disable("search.download")
    
    # Reload table
    search.edit.temp <- input$search.table_cell_edit
    memory$search.table[search.edit.temp$row, search.edit.temp$col] <- DT::coerceValue(search.edit.temp$value, memory$search.table[search.edit.temp$row, search.edit.temp$col])
    replaceData(memory$search.table.proxy, memory$search.table, resetPaging = FALSE)
    
    # Replace data
    search.edit.temp$row <- as.integer(rownames(memory$search.table)[search.edit.temp$row])
    if (search.edit.temp$col >= 2) search.edit.temp$col <- search.edit.temp$col + 1
    memory$search.data[search.edit.temp$row, search.edit.temp$col] <- DT::coerceValue(search.edit.temp$value, memory$search.data[search.edit.temp$row, search.edit.temp$col])
    
    enable("search.save")
    enable("search.discard")
  })
  
  # Edit archive - display popup message
  observeEvent(input$search.save, {
    shinyalert(title = "수정내용을 저장하시겠습니까?", text = "데이터 이름 입력", type = "input", closeOnClickOutside = TRUE, showCancelButton = TRUE, 
               inputType = "text", inputValue = format(Sys.time(), tz = "Asia/Seoul"), inputPlaceholder = "데이터 이름", 
               confirmButtonText = "확인", cancelButtonText = "취소", callbackR = function (x) memory$response$search.save <- x)
  })
  
  # Edit archive - confirm
  observeEvent(memory$response$search.save, {
    if (!is.null(memory$response$search.save)) {
      if (memory$response$search.save == "")
        memory$response$search.save <- format(Sys.time(), tz = "Asia/Seoul")
      search.data[[memory$response$search.save]] <<- memory$search.data
      saveRDS(search.data, file = "DB/search.data.rds")
      
      updateSelectInput(session, inputId = "search.index", choices = rev(names(search.data)), selected = rev(names(search.data))[1])
      enable("search.remove")
      disable("search.save")
      disable("search.discard")
      
      memory$response$search.save <- NULL
      shinyalert(title = "수정내용이 저장되었습니다.", text = rev(names(search.data))[1], type = "success", closeOnClickOutside = TRUE, confirmButtonText = "확인")
    }
  })
  
  # Discard edited contents - display popup message
  observeEvent(input$search.discard, {
    shinyalert(title = "수정내용을 초기화하시겠습니까?", type = "warning", closeOnClickOutside = TRUE, showCancelButton = TRUE, 
               confirmButtonText = "확인", cancelButtonText = "취소", callbackR = function (x) memory$response$search.discard <- x)
  })
  
  # Discard edited contents - confirm
  observeEvent(memory$response$search.discard, {
    if (memory$response$search.discard) {
      memory$search.data <- search.data[[input$search.index]]
      memory$search.table <- make_table(memory$search.data[-c(2)], search.labels, memory$search.old$selected)
      replaceData(memory$search.table.proxy, memory$search.table, resetPaging = FALSE)
      
      disable("search.save")
      disable("search.discard")
      
      memory$response$search.discard <- FALSE
      shinyalert(title = "수정내용이 초기화되었습니다.", text = rev(names(search.data))[1], type = "success", closeOnClickOutside = TRUE, confirmButtonText = "확인")
    }
  })
  
  # Upload new sheet
  observeEvent(input$search.upload, {
    disable("search.update")
    
    filename <- unlist(strsplit(gsub(".xlsx", "", input$search.upload$name), "_"))
    memory$search.sheet <- cbind(read.xlsx2(input$search.upload$datapath, sheetIndex = 1, stringsAsFactors = FALSE, colClasses = NA) 
                                 %>% select_if(names(.) %in% colnames(search.data[[length(search.data)]])) %>% match_class(search.data[[length(search.data)]]), 
                                 연도 = as.integer(unlist(strsplit(filename[1], "-"))[1]), 현장 = filename[2], 협력사 = filename[3], 계약번호 = filename[1], 계약여부 = filename[4])
    id.na <- which(is.na(memory$search.sheet$대분류))
    if (length(id.na) > 0) {
      memory$search.sheet <- rbind(memory$search.sheet[id.na, ], memory$search.sheet[-id.na, ])
      rownames(memory$search.sheet) <- 1:nrow(memory$search.sheet)
    }
    
    if (any(is.na(memory$search.sheet$대분류)))
      shinyalert(title = "확인되지 않은 품목이 존재합니다!", text = input$search.upload$name, type = "warning", closeOnClickOutside = TRUE, confirmButtonText = "확인")
    
    output$search.sheet <- DT::renderDataTable(datatable(isolate(memory$search.sheet), extensions = "FixedHeader", editable = list(target = "cell", disable = list(columns = c(0))), 
                                                         options = list(fixedHeader = TRUE, pageLength = -1, dom = "tir"))
                                               %>% formatCurrency(c("자재비.단가", "노무비.단가", "경비.단가"), currency = ' ￦', interval = 3, mark = ',', digit = 0, before = FALSE)
                                               %>% formatStyle("대분류", target = "row", backgroundColor = styleEqual(c(NA, ""), rep("yellow", 2))))
    memory$search.sheet.proxy <- dataTableProxy("search.sheet")
    
    enable("search.update")
    enable("search.reset")
  })
  
  # Edit sheet
  observeEvent(input$search.sheet_cell_edit, {
    disable("search.update")
    disable("search.reset")
    
    # Replace sheet
    search.edit.temp <- input$search.sheet_cell_edit
    memory$search.sheet[search.edit.temp$row, search.edit.temp$col] <- DT::coerceValue(search.edit.temp$value, memory$search.sheet[search.edit.temp$row, search.edit.temp$col])
    if (search.edit.temp$col == 2)
      memory$search.sheet[search.edit.temp$row, ] <- match_class(memory$search.sheet[search.edit.temp$row, -c(1)], search.data[[length(search.data)]])
    replaceData(memory$search.sheet.proxy, memory$search.sheet, resetPaging = FALSE)
    
    enable("search.update")
    enable("search.reset")
  })
  
  # Update archive - display popup message
  observeEvent(input$search.update, {
    shinyalert(title = "데이터를 추가하시겠습니까?", text = "데이터 이름 입력", type = "input", closeOnClickOutside = TRUE, showCancelButton = TRUE, 
               inputType = "text", inputValue = format(Sys.time(), tz = "Asia/Seoul"), inputPlaceholder = "데이터 이름", 
               confirmButtonText = "확인", cancelButtonText = "취소", callbackR = function (x) memory$response$search.update <- x)
  })
  
  # Update archive - confirm
  observeEvent(memory$response$search.update, {
    if (!is.null(memory$response$search.update)) {
      if (memory$response$search.update == "")
        memory$response$search.update <- format(Sys.time(), tz = "Asia/Seoul")
      memory$search.sheet$품명 <- gsub("[[:blank:][:punct:]]", "", memory$search.sheet$품명)
      memory$search.sheet <- rbind(memory$search.sheet, search.data[[length(search.data)]])
      search.data[[memory$response$search.update]] <<- memory$search.sheet
      saveRDS(search.data, file = "DB/search.data.rds")
      
      updateSelectInput(session, inputId = "search.index", choices = rev(names(search.data)), selected = rev(names(search.data))[1])
      memory$search.sheet <- NULL
      output$search.sheet <- NULL
      reset("search.upload")
      enable("search.remove")
      disable("search.update")
      disable("search.reset")
      
      memory$response$search.update <- NULL
      shinyalert(title = "데이터가 추가되었습니다.", text = rev(names(search.data))[1], type = "success", closeOnClickOutside = TRUE, confirmButtonText = "확인")
    }
  })
  
  # Reset uploaded data - display popup message
  observeEvent(input$search.reset, {
    shinyalert(title = "업로드를 초기화하시겠습니까?", text = input$search.upload$name, type = "warning", closeOnClickOutside = TRUE, showCancelButton = TRUE, 
               confirmButtonText = "확인", cancelButtonText = "취소", callbackR = function (x) memory$response$search.reset <- x)
  })
  
  # Reset uploaded data - confirm
  observeEvent(memory$response$search.reset, {
    if (memory$response$search.reset) {
      memory$search.sheet <- NULL
      output$search.sheet <- NULL
      reset("search.upload")
      disable("search.update")
      disable("search.reset")
      memory$response$search.reset <- FALSE
    }
  })
  
  # Select data
  observeEvent(input$search.index, {
    memory$search.old$total <- sapply(search.labels, function (x) sort(unique(as.character(search.data[[input$search.index]][[x]]))))
    memory$search.old <- update_picker(session, search.data[[input$search.index]][search.labels], memory$search.old$total, NULL, search.labels, search.id, memory$search.old$total)
    memory$search.data <- search.data[[input$search.index]]
  })
  
  # Download data
  output$search.download <- downloadHandler(filename = paste0("기계약정보 ", format(as.POSIXct(input$search.index), "%Y_%m_%d_%H_%M_%S"), ".xlsx"), 
                                            content = function(file) write.xlsx2(search.data[[input$search.index]], file, row.names = FALSE))
  
  # Remove data - display popup message
  observeEvent(input$search.remove, {
    shinyalert(title = "데이터를 삭제하시겠습니까?", text = input$search.index, type = "warning", closeOnClickOutside = TRUE, showCancelButton = TRUE, 
               confirmButtonText = "확인", cancelButtonText = "취소", callbackR = function (x) memory$response$search.remove <- x)
  })
  
  # Remove data - confirm
  observeEvent(memory$response$search.remove, {
    if (memory$response$search.remove) {
      search.data[input$search.index] <<- NULL
      saveRDS(search.data, file = "DB/search.data.rds")
      
      updateSelectInput(session, inputId = "search.index", choices = rev(names(search.data)), selected = rev(names(search.data))[1])
      if (length(search.data) == 1) 
        disable("search.remove")
      
      memory$response$search.remove <- FALSE
      shinyalert(title = "데이터가 삭제되었습니다.", text = input$search.index, type = "success", closeOnClickOutside = TRUE, confirmButtonText = "확인")
    }
  })
  
  #########################################################
  ### Contract tab
  #########################################################
  # Download example sheet
  output$cntrt.example <- downloadHandler(filename = paste0("2019-5-1808_동우화인켐 평택_세현이엔지_계약", ".xlsx"), content = function(file) write.xlsx2(readRDS("DB/cntrt.example.rds"), file, row.names = FALSE))
  
  # Analyze
  observeEvent(input$cntrt.sheet, {
    disable("cntrt.download")
    
    # Read uploaded sheet
    memory$cntrt.sheet <- read.xlsx2(input$cntrt.sheet$datapath, sheetIndex = 1, stringsAsFactors = FALSE, colClasses = NA) %>% match_class(search.data[[length(search.data)]])
    
    # Make analytics using previous contract data
    memory$cntrt.stat <- make_stat(memory$cntrt.sheet, search.data[[length(search.data)]], 
                                   options = list(sign = input$cntrt.sign, year = input$cntrt.year, site = input$cntrt.site, coop = input$cntrt.coop))
    
    # Render stat
    output$cntrt.stat <- DT::renderDataTable(datatable(memory$cntrt.stat, extensions = "FixedHeader", editable = list(target = "cell", disable = list(columns = c(0, 4:8))), 
                                                       options = list(fixedHeader = TRUE, lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "전체")), pageLength = -1))
                                             %>% formatCurrency(c("자재비.단가", "최저가", "평균가", "중간가", "최고가"), currency = ' ￦', interval = 3, mark = ',', digit = 0, before = FALSE)
                                             %>% formatStyle("가격차이", target = "row", color = JS("value < 0 ? 'blue' : (value > 0 ? 'red' : 'green')")))
    memory$cntrt.stat.proxy <- dataTableProxy("cntrt.stat")
    
    # Download stat
    output$cntrt.download <- downloadHandler(filename = paste0(input$cntrt.sheet$name, "_분석", ".xlsx"), content = function(file) write.xlsx2(memory$cntrt.stat, file, row.names = FALSE))
    enable("cntrt.download")
  })
  
  # Options changed
  observeEvent(c(input$cntrt.sign, input$cntrt.year, input$cntrt.site, input$cntrt.coop), {
    if (!is.null(memory$cntrt.stat.proxy)) {
      disable("cntrt.download")
      
      # Make analytics using previous contract data
      memory$cntrt.stat <- make_stat(memory$cntrt.sheet, search.data[[length(search.data)]], 
                                     options = list(sign = input$cntrt.sign, year = input$cntrt.year, site = input$cntrt.site, coop = input$cntrt.coop))
      
      # Reload stat
      replaceData(memory$cntrt.stat.proxy, memory$cntrt.stat, resetPaging = FALSE)
      
      # Download stat
      output$cntrt.download <- downloadHandler(filename = paste0("기계약_분석", ".xlsx"), content = function(file) write.xlsx2(memory$cntrt.stat, file, row.names = FALSE))
      enable("cntrt.download")
    }
  })
  
  # Update options list
  observeEvent(memory$search.old$total, {
    disable("cntrt.download")
    
    updateSliderInput(session, inputId = "cntrt.year", min = min(memory$search.old$total[[3]]), max = max(memory$search.old$total[[3]]), 
                      value = c(min(memory$search.old$total[[3]]), max(memory$search.old$total[[3]])))
    updatePickerInput(session, inputId = "cntrt.site", choices = memory$search.old$total[[4]], selected = memory$search.old$total[[4]])
    updatePickerInput(session, inputId = "cntrt.coop", choices = memory$search.old$total[[5]], selected = memory$search.old$total[[5]])
    
    enable("cntrt.download")
  })
  
  # Edit table
  observeEvent(input$cntrt.stat_cell_edit, {
    disable("cntrt.download")
    
    # Replace data of sheet
    cntrt.edited <- input$cntrt.stat_cell_edit
    memory$cntrt.sheet[cntrt.edited$row, cntrt.edited$col] <- DT::coerceValue(cntrt.edited$value, memory$cntrt.sheet[cntrt.edited$row, cntrt.edited$col])
    
    # Reload stat
    if (cntrt.edited$col == 2)
      memory$cntrt.sheet[cntrt.edited$row, ] <- match_class(memory$cntrt.sheet[cntrt.edited$row, -c(1)], search.data[[length(search.data)]], 
                                                            options = list(sign = input$cntrt.sign, year = input$cntrt.year, site = input$cntrt.site, coop = input$cntrt.coop))
    memory$cntrt.stat[cntrt.edited$row, ] <- make_stat(memory$cntrt.sheet[cntrt.edited$row, ], search.data[[length(search.data)]], 
                                                       options = list(sign = input$cntrt.sign, year = input$cntrt.year, site = input$cntrt.site, coop = input$cntrt.coop))
    replaceData(memory$cntrt.stat.proxy, memory$cntrt.stat, resetPaging = FALSE)
    enable("cntrt.download")
  })
  
  #########################################################
  ### labor tab
  #########################################################
  
  # Update select box contents
  observeEvent(input$labor.chapter, {
    updateSelectInput(session, inputId = "labor.detail", choices = names(labor.data[[input$labor.index]][[input$labor.chapter]]))
  })
  
  # Update select box contents
  observeEvent(input$labor.detail, {
    output$labor.table <- DT::renderDataTable(datatable(labor.data[[input$labor.index]][[input$labor.chapter]][[input$labor.detail]]))
  })
  
  
  # Upload data
  observeEvent(input$labor.upload, {
    disable("labor.update")
    memory$labor.sheet <- table_making(input$labor.upload$datapath)
    updateProgressBar(session = session, id = "labor.progress", value = 100)
    enable("labor.update")
    enable("labor.reset")
  })
  
  # Update archive - display popup message
  observeEvent(input$labor.update, {
    shinyalert(title = "데이터를 추가하시겠습니까?", text = input$labor.upload$name, type = "warning", closeOnClickOutside = TRUE, showCancelButton = TRUE, 
               confirmButtonText = "확인", cancelButtonText = "취소", callbackR = function (x) memory$response$labor.update <- x)
  })
  
  # Update archive - confirm
  observeEvent(memory$response$labor.update, {
    if (memory$response$labor.update) {
      labor.data[[format(Sys.time(), tz = "Asia/Seoul")]] <<- memory$labor.sheet
      saveRDS(labor.data, file = "DB/labor.data.rds")
      updateSelectInput(session, inputId = "labor.index", choices = rev(names(labor.data)))
      enable("labor.remove")
      disable("labor.update")
      disable("labor.reset")
      memory$labor.sheet <- NULL
      reset("search.upload")
      memory$response$labor.update <- FALSE
      shinyalert(title = "데이터가 추가되었습니다.", text = rev(names(labor.data))[1], type = "success", closeOnClickOutside = TRUE, confirmButtonText = "확인")
    }
  })
  
  # Reset uploaded data - display popup message
  observeEvent(input$labor.reset, {
    shinyalert(title = "업로드를 초기화하시겠습니까?", text = input$labor.upload$name, type = "warning", closeOnClickOutside = TRUE, showCancelButton = TRUE, 
               confirmButtonText = "확인", cancelButtonText = "취소", callbackR = function (x) memory$response$labor.reset <- x)
  })
  
  # Reset uploaded data - confirm
  observeEvent(memory$response$labor.reset, {
    if (memory$response$labor.reset) {
      disable("labor.update")
      disable("labor.reset")
      memory$labor.sheet <- NULL
      reset("labor.upload")
      memory$response$labor.reset <- FALSE
    }
  })
  
  # Remove data - display popup message
  observeEvent(input$labor.remove, {
    shinyalert(title = "데이터를 삭제하시겠습니까?", text = input$labor.index, type = "warning", closeOnClickOutside = TRUE, showCancelButton = TRUE, 
               confirmButtonText = "확인", cancelButtonText = "취소", callbackR = function (x) memory$response$labor.remove <- x)
  })
  
  # Remove data - confirm
  observeEvent(memory$response$labor.remove, {
    if (memory$response$labor.remove) {
      if (length(labor.data) > 1) {
        labor.data[input$labor.index] <<- NULL
        saveRDS(labor.data, file = "DB/labor.data.rds")
        shinyalert(title = "데이터가 삭제되었습니다.", text = input$labor.index, type = "success", closeOnClickOutside = TRUE, confirmButtonText = "확인")
        updateSelectInput(session, inputId = "labor.index", choices = rev(names(labor.data)))
      } else {
        shinyalert(title = "마지막 데이터는\n삭제할 수 없습니다!", text = input$labor.index, type = "error", closeOnClickOutside = TRUE, confirmButtonText = "확인")
      }
      if (length(labor.data) == 1) 
        disable("labor.remove")
      memory$response$labor.remove <- FALSE
    }
  })
}

#########################################################################################################################
### Run app
#########################################################################################################################
shinyApp(ui, server)
