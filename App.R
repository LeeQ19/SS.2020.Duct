#########################################################################################################################
### Set up environment
#########################################################################################################################
# Load library
library("aws.s3")
library("dplyr")
library("DT")
library("formattable")
library("ggrepel")
library("pdftools")
library("plotly")
library("RColorBrewer")
library("shiny")
library("shinyalert")
library("shinydashboard")
library("shinyjs")
library("shinymanager")
library("shinythemes")
library("shinyWidgets")
library("showtext")
library("xlsx")

# Load function
source("Module.R", encoding = "UTF-8")

# Set environment
options(shiny.maxRequestSize = 30*1024^2)
showtext_auto()
showtext_opts(dpi = 112)

# data.frame with credentials info
credentials <- load_data("credentials")

# Load data
search.data    <- load_data("search.data")
search.index   <- load_data("search.index")
search.default <- length(search.index)
search.label   <- names(search.index[[search.default]])
search.id      <- c("search.major", "search.std", "search.year", "search.site", "search.coop", "search.num", "search.anly")
search.width   <- c(2, 2, 1, 2, 2, 2, 1)

labor.data <- load_data("labor.data")

site.selected <- search.data[[search.default]][search.data[[search.default]]$대분류 == '덕트', ]$현장
coop.selected <- search.data[[search.default]][search.data[[search.default]]$대분류 == '덕트', ]$협력사
combi.selected <- make_graphdata(search.data[[search.default]])[make_graphdata(search.data[[search.default]])$대분류 == '덕트', ]$combi
contnum.selected <- search.data[[search.default]][search.data[[search.default]]$대분류 == '덕트', ]$계약번호

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
      id = "tabs", 
      menuItem("통합검색",    tabName = "search", icon = icon("search")), 
      menuItem("견적서 비교", tabName = "cmp",    icon = icon("chart-bar")), 
      menuItem("견적서 검토", tabName = "anly",  icon = icon("dashboard")), 
      menuItem("품셈",        tabName = "labor",  icon = icon("file-invoice")), 
      menuItem("분석 그래프", tabName = "graph",  icon = icon("list")), 
      menuItem("사용자 로그", tabName = "log",    icon = icon("list"))
    ), 
    tags$footer("ver 4.3.0", align = "right", style = "font-size: 15px; position:absolute; bottom:0; width:100%; padding:10px")
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
        fluidRow(
          box(
            lapply(c(1:7), function (i) column(search.width[i], 
                                               pickerInput(search.id[i], paste0(search.label[i], ":"), 
                                                           choices = sort(names(search.index[[search.default]][[i]])), selected = sort(names(search.index[[search.default]][[i]])), 
                                                           options = list(`actions-box` = TRUE, `selected-text-format` = "count > 2", `count-selected-text` = "선택: {0} / 전체: {1}", 
                                                                          `select-all-text` = "전체 선택", `deselect-all-text` = "선택 해제", `live-search`  = TRUE), multiple = TRUE))), 
            width = NULL
          )
        ),
        
        # Create a new row for table
        DT::dataTableOutput("search.table"), 
        
        # Calculate labor cost
        fluidRow(
          box(
            column(2, 
                   materialSwitch(inputId = "search.laborswitch", 
                                  label = "노무비합계", 
                                  status = "primary")
            ),
            column(10), 
            width = NULL
          )
        ), 
        
        DT::dataTableOutput("search.labortable"),
        DT::dataTableOutput("search.laborsummary"), 
        
        # Modify data
        fluidRow(
          box(
            column(3, 
                   fileInput("search.upload", "엑셀 파일 업로드", accept = ".xlsx"), 
                   div(style = "margin-top: -20px;"), 
                   disabled(actionButton("search.update", "업데이트", icon = icon("cloud-upload"))), 
                   div(style = "float: right;", 
                       disabled(actionButton("search.reset", "초기화", icon = icon("undo"))))), 
            column(6), 
            column(3, 
                   selectInput("search.list", "데이터 선택", choices = rev(names(search.data))), 
                   disabled(actionButton("search.save", "수정내용 저장", icon = icon("save"))), 
                   disabled(actionButton("search.discard", "수정내용 초기화", icon = icon("undo"))), 
                   div(style = "float: right;", 
                       downloadButton("search.download", "다운로드"), 
                       actionButton("search.remove", "데이터 삭제", icon = icon("trash")))), 
            width = NULL
          )
        ), 
        
        # Create a new row for uploaded sheet
        DT::dataTableOutput("search.sheet")
      ), 
      
      ############################
      ### Compare tab content
      ############################
      tabItem(
        tabName = "cmp",
        # Upload file
        fluidRow(
          box(
            # Input: select a file
            column(6, 
                   fileInput("cmp.sheet", "엑셀 파일 업로드", accept = ".xlsx", multiple = TRUE), 
                   div(style = "margin-top: -20px;"), 
                   downloadButton("cmp.example1", "예시파일1"), 
                   downloadButton("cmp.example2", "예시파일2"), 
                   downloadButton("cmp.example3", "예시파일3")), 
            column(6, 
                   div(style = "margin-top: 22px;"), 
                   htmlOutput("cmp.text")), 
            width = NULL
          )
        ),
        DT::dataTableOutput("cmp.table")
      ), 
      
      ############################
      ### Analysis tab content
      ############################
      tabItem(
        tabName = "anly", 
        # Upload file
        fluidRow(
          box(
            # Input: select a file
            column(6, 
                   fileInput("anly.sheet", "엑셀 파일 업로드", accept = ".xlsx"), 
                   downloadButton("anly.example", "예시파일")), 
            
            # Options
            column(2, 
                   div(style = "margin-top: 20px; margin-bottom: 30px;", checkboxInput("anly.sign", "계약건 한정",   value = FALSE)), 
                   tags$style(".irs-grid-pol.small {height: 0px;}"), 
                   sliderInput("anly.year", "분석기간", 
                               min = min(as.integer(names(search.index[[search.default]][["연도"]]))), max = max(as.integer(names(search.index[[search.default]][["연도"]]))), 
                               value = c(min(as.integer(names(search.index[[search.default]][["연도"]]))), max(as.integer(names(search.index[[search.default]][["연도"]])))), 
                               step = 1, width = "75%", sep = "", post = "년")),  
            column(1, 
                   fluidRow(pickerInput("anly.site", "현장:", 
                                        choices = sort(names(search.index[[search.default]][["현장"]])), selected = sort(names(search.index[[search.default]][["현장"]])), 
                                        options = list(`actions-box` = TRUE, `selected-text-format` = "count > 2", `count-selected-text` = "선택: {0} / 전체: {1}", 
                                                       `select-all-text` = "전체 선택", `deselect-all-text` = "선택 해제", `live-search`  = TRUE), multiple = TRUE), 
                            pickerInput("anly.coop", "협력사:", 
                                        choices = sort(names(search.index[[search.default]][["협력사"]])), selected = sort(names(search.index[[search.default]][["협력사"]])), 
                                        options = list(`actions-box` = TRUE, `selected-text-format` = "count > 2", `count-selected-text` = "선택: {0} / 전체: {1}", 
                                                       `select-all-text` = "전체 선택", `deselect-all-text` = "선택 해제", `live-search`  = TRUE), multiple = TRUE))), 
            width = NULL
          )
        ), 
        
        # Create a new row for table
        DT::dataTableOutput("anly.stat"), 
        DT::dataTableOutput("anly.labortable"),
        DT::dataTableOutput("anly.laborsummary"),
        
        # Download button for table
        box(
          div(style = "float: right;", disabled(downloadButton("anly.download", "다운로드"))), 
          width = NULL
        )
      ), 
      
      ############################
      ### Labor tab content
      ############################
      tabItem(
        tabName = "labor", 
        # Select box to search data
        fluidRow(
          box(
            column(3, selectInput("labor.chapter", "챕터 선택", choices = names(labor.data[[length(labor.data)]]))),
            column(3, selectInput("labor.detail", "상세 선택", choices = names(labor.data[[length(labor.data)]][[1]]))),
            width = NULL
          )
        ),
        
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
                   div(style = "float: right;", disabled(actionButton("labor.reset", "초기화", icon = icon("undo"))))),
            column(3, 
                   selectInput("labor.index", "데이터 선택", choices = rev(names(labor.data))), 
                   div(style = "float: right;", actionButton("labor.remove", "데이터 삭제", icon = icon("trash")))), 
            width = NULL
          )
        )
      ), 
      
      ############################
      ### Graph tab content
      ############################
      tabItem(
        tabName = "graph",
        
        fluidPage(
          tabsetPanel(
            # First sub tab
            tabPanel("규격별 업체별 그래프",
                     br(),
                     fluidRow(
                       box(
                         column(2,
                                selectizeInput("graph.category1", label = "대분류:",
                                               choices = sort(unique(make_graphdata(search.data[[search.default]])$대분류)),  
                                               selected = "덕트", 
                                               multiple = FALSE)
                         ), 
                         column(2, 
                                radioGroupButtons("graph.contract1", "계약 여부",
                                                  choiceNames = list("계약", "비계약"),
                                                  choiceValues = list("계약", "비계약"))
                         ),
                         column(2,
                                pickerInput(
                                  inputId = "graph.site1",
                                  label = "현장:",
                                  choices = site.selected,
                                  selected = site.selected,
                                  options = list(`actions-box` = TRUE, 
                                                 `count-selected-text` = "전체",
                                                 `live-search`  = TRUE),
                                  multiple = TRUE)
                         ),
                         column(2,
                                pickerInput(
                                  inputId = "graph.coop1",
                                  label = "협력사:",
                                  choices = coop.selected,
                                  selected = coop.selected,
                                  options = list(`actions-box` = TRUE, 
                                                 `count-selected-text` = "전체",
                                                 `live-search`  = TRUE),
                                  multiple = TRUE)
                         ),
                         
                         column(2,
                                pickerInput(
                                  inputId = "graph.combi1", 
                                  label = "상세 선택",
                                  choices = combi.selected,
                                  selected = combi.selected,
                                  options = list(`actions-box` = TRUE, 
                                                 `count-selected-text` = "전체",
                                                 `live-search`  = TRUE),
                                  multiple = TRUE)
                         ),
                         column(2, 
                                pickerInput(
                                  inputId = "graph.contnum1", 
                                  label = "계약번호:",
                                  choices = contnum.selected,
                                  selected = contnum.selected,
                                  options = list(`actions-box` = TRUE, 
                                                 `count-selected-text` = "전체",
                                                 `live-search`  = TRUE),
                                  multiple = TRUE)
                         ),
                         width = NULL
                       )
                     ),
                     br(),
                     h3('선택 데이터 확인'),
                     DT::dataTableOutput("graph.selected1"),
                     br(), br(),
                     plotlyOutput("graph.by.stand"),
                     br(), br(),
                     h3("데이터 기본 통계 요약"),
                     DT::dataTableOutput("graph.summary"),
                     br(), br(),
                     
            ),
            
            # Second sub tab
            tabPanel("년도별 업체별 그래프",
                     br(),
                     fluidRow(
                       box(
                         column(2,
                                selectizeInput("graph.category2", label = "대분류:",
                                               choices = sort(unique(make_graphdata(search.data[[search.default]])$대분류)),  
                                               selected = "덕트", multiple = FALSE)
                         ), 
                         column(2, 
                                radioGroupButtons("graph.contract2", "계약 여부",
                                                  choiceNames = list("계약", "비계약"),
                                                  choiceValues = list("계약", "비계약"))
                         ),
                         column(2,
                                pickerInput("graph.standard2", 
                                            label = "규격:",
                                            choices = sort(unique(make_graphdata(search.data[[search.default]])[make_graphdata(search.data[[search.default]])$대분류 == '덕트', ]$규격)),  
                                            selected = "0.8T", 
                                            multiple = FALSE)
                         ), 
                         column(2,
                                pickerInput(
                                  inputId = "graph.site2",
                                  label = "현장:",
                                  choices = site.selected,
                                  selected = site.selected,
                                  options = list(`actions-box` = TRUE, 
                                                 `count-selected-text` = "전체",
                                                 `live-search`  = TRUE),
                                  multiple = TRUE)
                         ),
                         column(2,
                                pickerInput(
                                  inputId = "graph.coop2",
                                  label = "협력사:",
                                  choices = coop.selected,
                                  selected = coop.selected,
                                  options = list(`actions-box` = TRUE, 
                                                 `count-selected-text` = "전체",
                                                 `live-search`  = TRUE),
                                  multiple = TRUE)
                         ),
                         width = NULL
                       )
                     ),
                     br(),
                     h3('선택 데이터 확인'),
                     DT::dataTableOutput("graph.selected2"),
                     br(), br(),
                     plotlyOutput("graph.by.year"),
            )
          )
        )
      ), 
      
      ############################
      ### Log tab content
      ############################
      tabItem(
        tabName = "log", 
        fluidRow(
          box(
            fluidRow(div(style = "float: right; margin-right: 16px; margin-bottom: 15px;", downloadButton("log.download", "다운로드"))), 
            verbatimTextOutput("log"), 
            width = NULL
          )
        )
      )
    ), 
    div(style = "margin-bottom: 50px;")
  )
)

#########################################################################################################################
### Define server logic to summarize and view selected data
#########################################################################################################################
server <- function(input, output, session) {
  #########################################################
  ### Set environment
  #########################################################
  # Check authority
  auth <- secure_server(check_credentials = check_credentials(credentials))
  
  
  # Custom sidebar collapse
  runjs({'var el2 = document.querySelector(".skin-blue");
          el2.className = "skin-blue sidebar-mini";
          var clicker = document.querySelector(".sidebar-toggle");
          clicker.id = "switchState";'})
  
  onclick('switchState', runjs({'var title = document.querySelector(".logo")
                                 if (title.style.visibility == "hidden") {
                                   title.style.visibility = "visible";
                                 } else {
                                   title.style.visibility = "hidden";
                                 }'}))
  
  # Define memory
  memory <- reactiveValues(logs = load_data("logs"), 
                           search.table = search.data[[search.default]], 
                           search.data  = search.data[[search.default]], 
                           search.index = search.index[[search.default]], 
                           search.picker = list(choices = lapply(search.index[[search.default]], function (x) {sort(names(x))}), 
                                                selected = lapply(search.index[[search.default]], function (x) {sort(names(x))})),
                           graph.data = make_graphdata(search.data[[search.default]]),
                           search.laborswitch = F
  )
  
  # Log in
  observeEvent(auth, {
    memory$logs <- save_log(memory$logs, auth, "login")
  })
  
  # Tab select
  observeEvent(input$tabs, {
    memory$logs <- save_log(memory$logs, auth, "select", object = input$tabs)
  })
  
  #########################################################
  ### Search tab
  #########################################################
  # Button preset
  if (length(search.data) == 1) 
    disable("search.remove")
  
  # Update select box contents
  observeEvent(c(lapply(search.id, function (x) {input[[x]]}), memory$search.index), {
    memory$search.picker <- update_picker(session, search.id, memory$search.data, memory$search.index, lapply(search.id, function (x) {input[[x]]}), memory$search.picker)
    memory$logs <- save_log(memory$logs, auth, "select", object = "search.filter")
  })
  
  # Update table
  observeEvent(c(memory$search.picker$selected, memory$search.data), {
    # Filter data based on selections
    memory$search.table <- make_table(memory$search.data, memory$search.index, memory$search.picker$selected)
    
    # Render table
    output$search.table <- DT::renderDataTable(datatable(isolate(memory$search.table), extensions = "FixedHeader", editable = list(target = "cell", disable = list(columns = c(0))), colnames = c("번호" = 1), 
                                                         options = list(fixedHeader = TRUE, lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "전체")), pageLength = 10, 
                                                                        columnDefs = list(list(targets = 2, visible = FALSE))))
                                               %>% (function (dt) {dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]); return(dt)})
                                               %>% formatCurrency(c("자재비.단가", "노무비.단가", "경비.단가"), currency = ' ￦', interval = 3, mark = ',', digit = 0, before = FALSE))
    memory$search.table.proxy <- dataTableProxy("search.table")
    
    if(memory$search.laborswitch == T){
      labortable <- labor_making(memory$search.table, memory$labor.data)
      laborsummary <- data.frame(
        덕트공 = sum(as.numeric(labortable$덕트공), na.rm = T),
        보통인부 = sum(as.numeric(labortable$보통인부), na.rm = T),
        보온공 = sum(as.numeric(labortable$보온공), na.rm = T),
        노무비_총합 = sum(as.numeric(labortable$덕트공), na.rm = T) * 168742 + sum(as.numeric(labortable$보통인부), na.rm = T) * 138290 + sum(as.numeric(labortable$보온공), na.rm = T) * 180707, 
        실제_노무비_총합 = sum(as.numeric(memory$search.table$노무비.단가) * as.numeric(memory$search.table$수량), na.rm = T))
      output$search.labortable <- DT::renderDataTable(datatable(labortable))
      output$search.laborsummary <- DT::renderDataTable(datatable(laborsummary) %>% formatCurrency(c("노무비_총합", "실제_노무비_총합"), currency = ' ￦', interval = 3, mark = ',', digit = 0, before = FALSE))  
      
    }
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
    memory$search.data[search.edit.temp$row, search.edit.temp$col] <- DT::coerceValue(search.edit.temp$value, memory$search.data[search.edit.temp$row, search.edit.temp$col])
    
    if(input$search.laborswitch == T){
      labortable <- labor_making(memory$search.table, memory$labor.data)
      laborsummary <- data.frame(
        덕트공 = sum(as.numeric(labortable$덕트공), na.rm = T),
        보통인부 = sum(as.numeric(labortable$보통인부), na.rm = T),
        보온공 = sum(as.numeric(labortable$보온공), na.rm = T),
        노무비_총합 = sum(as.numeric(labortable$덕트공), na.rm = T) * 168742 + sum(as.numeric(labortable$보통인부), na.rm = T) * 138290 + sum(as.numeric(labortable$보온공), na.rm = T) * 180707, 
        실제_노무비_총합 = sum(as.numeric(memory$search.table$노무비.단가) * as.numeric(memory$search.table$수량), na.rm = T))
      output$search.labortable <- DT::renderDataTable(datatable(labortable))
      output$search.laborsummary <- DT::renderDataTable(datatable(laborsummary) %>% formatCurrency(c("노무비_총합", "실제_노무비_총합"), currency = ' ￦', interval = 3, mark = ',', digit = 0, before = FALSE))  
      
    } else {
      output$search.labortable <-NULL
      output$search.laborsummary <-NULL
    }
    
    enable("search.save")
    enable("search.discard")
    memory$logs <- save_log(memory$logs, auth, "edit", object = paste0("search.table[", search.edit.temp$row, ", ", search.edit.temp$col, "]"), to = search.edit.temp$value)
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
      save_data(search.data)
      search.index[[memory$response$search.save]] <<- make_index(memory$search.data, search.label)
      save_data(search.index)
      
      updateSelectInput(session, inputId = "search.list", choices = rev(names(search.data)), selected = rev(names(search.data))[1])
      if (length(search.data) > 1)
        enable("search.remove")
      disable("search.save")
      disable("search.discard")
      
      memory$response$search.save <- NULL
      shinyalert(title = "수정내용이 저장되었습니다.", text = rev(names(search.data))[1], type = "success", closeOnClickOutside = TRUE, confirmButtonText = "확인")
      memory$logs <- save_log(memory$logs, auth, "confirm", object = "edit contents", to = "search.table")
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
      memory$search.data <- search.data[[input$search.list]]
      memory$search.table <- make_table(memory$search.data, memory$search.index, memory$search.picker$selected)
      replaceData(memory$search.table.proxy, memory$search.table, resetPaging = FALSE)
      
      disable("search.save")
      disable("search.discard")
      
      memory$response$search.discard <- FALSE
      shinyalert(title = "수정내용이 초기화되었습니다.", text = input$search.list, type = "success", closeOnClickOutside = TRUE, confirmButtonText = "확인")
      memory$logs <- save_log(memory$logs, auth, "discard", object = "search.table edit")
    }
  })
  
  # Upload new sheet
  observeEvent(input$search.upload, {
    disable("search.update")
    
    filename <- unlist(strsplit(gsub(".xlsx", "", input$search.upload$name), "_"))
    memory$search.sheet <- cbind(read.xlsx2(input$search.upload$datapath, sheetIndex = 1, stringsAsFactors = FALSE, colClasses = NA) 
                                 %>% select_if(names(.) %in% colnames(search.data[[search.default]])) %>% match_class(search.data[[search.default]]), 
                                 연도 = as.integer(unlist(strsplit(filename[1], "-"))[1]), 현장 = filename[2], 협력사 = filename[3], 계약번호 = filename[1], 계약여부 = filename[4])
    search.data.complete <- search.data[[search.default]][complete.cases(search.data[[search.default]]$자재비.단가), ]
    memory$search.sheet <- cbind(memory$search.sheet, 색=ifelse(memory$search.sheet$대분류 %in%  unique(search.data[[search.default]]$대분류),"T","F"), 단가색 = apply(memory$search.sheet, 1,  function (x) {ifelse(as.numeric(x["자재비.단가"]) <= max(search.data.complete[search.data.complete$대분류 == x["대분류"] & search.data.complete$규격 == x["규격"], ]$자재비.단가) & as.numeric(x["자재비.단가"]) >= min(search.data.complete[search.data.complete$대분류 == x["대분류"] & search.data.complete$규격 == x["규격"], ]$자재비.단가), "T", "F")}))
    
    id.na <- which(is.na(memory$search.sheet$대분류))
    if (length(id.na) > 0) {
      memory$search.sheet <- rbind(memory$search.sheet[id.na, ], memory$search.sheet[-id.na, ])
      rownames(memory$search.sheet) <- 1:nrow(memory$search.sheet)
    }
    
    if (any(is.na(memory$search.sheet$대분류)))
      shinyalert(title = "확인되지 않은 품목이\n존재합니다!", text = input$search.upload$name, type = "warning", closeOnClickOutside = TRUE, confirmButtonText = "확인")
    
    output$search.sheet <- DT::renderDataTable(datatable(isolate(memory$search.sheet), extensions = "FixedHeader", editable = list(target = "cell", disable = list(columns = c(0))), 
                                                         options = list(fixedHeader = TRUE, pageLength = -1, dom = "tir", columnDefs = list(list(targets = c((ncol(memory$search.sheet)-1),ncol(memory$search.sheet)), visible = FALSE))))
                                               %>% formatCurrency(c("자재비.단가", "노무비.단가", "경비.단가"), currency = ' ￦', interval = 3, mark = ',', digit = 0, before = FALSE)
                                               %>% formatStyle("색", target = "row", backgroundColor = styleEqual("F", "red"))
                                               %>% formatStyle("대분류", target = "row", backgroundColor = styleEqual(c("",NA), c("yellow","yellow")))
                                               %>% formatStyle('자재비.단가', '단가색', color = styleEqual("F", "red")))
    
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
    if (search.edit.temp$col == 2){
      memory$search.sheet[search.edit.temp$row, ] <- match_class(memory$search.sheet[search.edit.temp$row, -c(1)], search.data[[search.default]])
    }
    search.data.complete <- search.data[[search.default]][complete.cases(search.data[[search.default]]$자재비.단가), ]
    memory$search.sheet <- cbind(memory$search.sheet[,-c((ncol(memory$search.sheet)-1),ncol(memory$search.sheet))] , 색 = ifelse(memory$search.sheet$대분류 %in%  unique(search.data[[search.default]]$대분류),"T","F"), 단가색 = apply(memory$search.sheet, 1,  function (x) {ifelse(as.numeric(x["자재비.단가"]) <= max(search.data.complete[search.data.complete$대분류 == x["대분류"] & search.data.complete$규격 == x["규격"], ]$자재비.단가) & as.numeric(x["자재비.단가"]) >= min(search.data.complete[search.data.complete$대분류 == x["대분류"], ]$자재비.단가), "T", "F")}))
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
    if (!isFALSE(memory$response$search.update)) {
      if (memory$response$search.update == "")
        memory$response$search.update <- format(Sys.time(), tz = "Asia/Seoul")
      memory$search.sheet$품명 <- gsub("[[:blank:][:punct:]]", "", memory$search.sheet$품명)
      data.temp <- rbind(memory$search.sheet[colnames(search.data[[search.default]])], search.data[[search.default]])
      search.data[[memory$response$search.update]] <<- data.temp
      save_data(search.data)
      search.index[[memory$response$search.update]] <<- make_index(data.temp, search.label)
      save_data(search.index)
      
      updateSelectInput(session, inputId = "search.list", choices = rev(names(search.data)), selected = rev(names(search.data))[1])
      memory$search.sheet <- NULL
      output$search.sheet <- NULL
      reset("search.upload")
      enable("search.remove")
      disable("search.update")
      disable("search.reset")
      
      memory$response$search.update <- NULL
      shinyalert(title = "데이터가 추가되었습니다.", text = rev(names(search.data))[1], type = "success", closeOnClickOutside = TRUE, confirmButtonText = "확인")
      memory$logs <- save_log(memory$logs, auth, "add", object = rev(names(search.data))[1], to = "search.table")
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
  observeEvent(input$search.list, {
    memory$search.table <- search.data[[input$search.list]]
    memory$search.data  <- search.data[[input$search.list]]
    memory$search.index <- search.index[[input$search.list]]
    memory$logs <- save_log(memory$logs, auth, "select", object = "search.list", to = input$search.list)
    memory$graph.data <- make_graphdata(search.data[[input$search.list]])
  })
  
  # Download data
  output$search.download <- downloadHandler(filename = paste0("기계약정보_", input$search.list, ".xlsx"), 
                                            content = function(file) write.xlsx2(search.data[[input$search.list]], file, row.names = FALSE))
  
  # Remove data - display popup message
  observeEvent(input$search.remove, {
    shinyalert(title = "데이터를 삭제하시겠습니까?", text = input$search.list, type = "warning", closeOnClickOutside = TRUE, showCancelButton = TRUE, 
               confirmButtonText = "확인", cancelButtonText = "취소", callbackR = function (x) memory$response$search.remove <- x)
  })
  
  # Remove data - confirm
  observeEvent(memory$response$search.remove, {
    if (memory$response$search.remove) {
      delete.name <- input$search.list
      search.data[[delete.name]] <<- NULL
      save_data(search.data)
      search.index[[delete.name]] <<- NULL
      save_data(search.index)
      
      updateSelectInput(session, inputId = "search.list", choices = rev(names(search.data)), selected = rev(names(search.data))[1])
      if (length(search.data) == 1) 
        disable("search.remove")
      
      memory$response$search.remove <- FALSE
      shinyalert(title = "데이터가 삭제되었습니다.", text = delete.name, type = "success", closeOnClickOutside = TRUE, confirmButtonText = "확인")
      memory$logs <- save_log(memory$logs, auth, "delete", object = paste0("search.table: ", delete.name))
    }
  })
  
  # Labor table
  observeEvent(input$search.laborswitch, {
    if(input$search.laborswitch == T){
      memory$search.laborswitch <- T
      labortable <- labor_making(memory$search.table, memory$labor.data)
      laborsummary <- data.frame(
        덕트공 = sum(as.numeric(labortable$덕트공), na.rm = T),
        보통인부 = sum(as.numeric(labortable$보통인부), na.rm = T),
        보온공 = sum(as.numeric(labortable$보온공), na.rm = T),
        노무비_총합 = sum(as.numeric(labortable$덕트공), na.rm = T) * 168742 + sum(as.numeric(labortable$보통인부), na.rm = T) * 138290 + sum(as.numeric(labortable$보온공), na.rm = T) * 180707, 
        실제_노무비_총합 = sum(as.numeric(memory$search.table$노무비.단가) * as.numeric(memory$search.table$수량), na.rm = T))
      output$search.labortable <- DT::renderDataTable(datatable(labortable))
      output$search.laborsummary <- DT::renderDataTable(datatable(laborsummary) %>% formatCurrency(c("노무비_총합", "실제_노무비_총합"), currency = ' ￦', interval = 3, mark = ',', digit = 0, before = FALSE))  
      
    } else {
      memory$search.laborswitch <- F
      output$search.labortable <-NULL
      output$search.laborsummary <-NULL
    }
  })
  
  #########################################################
  ### Compare tab
  #########################################################
  cmp.example <- load_data("cmp.example")
  output$cmp.example1 <- downloadHandler(filename = paste0("2017-5-0544_NCK_그린빌더스", ".xlsx"), content = function(file) write.xlsx2(cmp.example[[1]], file, row.names = FALSE))
  output$cmp.example2 <- downloadHandler(filename = paste0("2017-5-0544_NCK_성현기공", ".xlsx"), content = function(file) write.xlsx2(cmp.example[[2]], file, row.names = FALSE))
  output$cmp.example3 <- downloadHandler(filename = paste0("2017-5-0544_NCK_유창이엔지니어링", ".xlsx"), content = function(file) write.xlsx2(cmp.example[[3]], file, row.names = FALSE))
  
  observeEvent(input$cmp.sheet, {
    memory$tempdata <- list()
    text.temp <- NULL
    memory$table.temp <- data.frame(matrix(nrow=0, ncol=(4 + nrow(input$cmp.sheet) * 3)))
    for (i in 1:nrow(input$cmp.sheet)) {
      memory$tempdata[[i]] <- match_class(read.xlsx2(input$cmp.sheet$datapath[i], sheetIndex = 1, stringsAsFactors = FALSE, colClasses = NA),  search.data[[search.default]])
      memory$tempdata[[i]][, c(5:10)] <- apply(memory$tempdata[[i]][,c(5:10)], 2, function (x) {x[is.na(as.integer(x))] <- 0; return(x)})
      
      
      table.temp.temp <- cbind(memory$tempdata[[i]][ , c(1, 3)], 
                               mat.or.vec(nr=nrow(memory$tempdata[[i]]), nc = (i - 1)), 
                               memory$tempdata[[i]][, 6], 
                               mat.or.vec(nr=nrow(memory$tempdata[[i]]), nc = (nrow(input$cmp.sheet) - i)),
                               mat.or.vec(nr=nrow(memory$tempdata[[i]]), nc = (i - 1)), 
                               memory$tempdata[[i]][, 8], 
                               mat.or.vec(nr=nrow(memory$tempdata[[i]]), nc = (nrow(input$cmp.sheet) - i)),
                               mat.or.vec(nr=nrow(memory$tempdata[[i]]), nc = (i - 1)), 
                               memory$tempdata[[i]][, 10], 
                               mat.or.vec(nr=nrow(memory$tempdata[[i]]), nc = (nrow(input$cmp.sheet) - i)), i, c(1:nrow(memory$tempdata[[i]])))
      colnames(table.temp.temp) <- colnames(memory$table.temp)
      memory$table.temp <- data.frame(rbind(memory$table.temp, table.temp.temp))
      memory$table.temp[, -c(1, 2)][is.na(memory$table.temp[, -c(1, 2)])] <- 0
      text.temp <- paste0(text.temp, "견적서 ", i, ": ", stringi::stri_extract_first(str = input$cmp.sheet[i, 1], regex = ".*(?=\\.)"), "</br>")
    }
    
    
    output$cmp.text <- renderText({text.temp})
    memory$table.temp <- match_table(memory$table.temp,memory$tempdata)
    table.show <- data.frame(memory$table.temp[, c(1:(2 + nrow(input$cmp.sheet) * 3))])
    colnames(table.show) <- c("대분류", "규격", paste("자재비 단가", 1:nrow(input$cmp.sheet)), paste("노무비 단가", 1:nrow(input$cmp.sheet)), paste("경비 단가", 1:nrow(input$cmp.sheet)))
    rownames(table.show) <- c(1:nrow(table.show))
    temp.show <- mat.or.vec(nr = nrow(table.show), nc = ncol(table.show) - 2)
    for (i in 1:nrow(table.show)) {
      for (j in 1:3) {
        col <- as.integer(c((3 + (j - 1) * ((ncol(table.show) - 2) / 3)):(2 + j * ((ncol(table.show) - 2) / 3))))
        temp.temp.show <- rep(0, length(col))
        if ((sum(table.show[i, col] == max(as.integer(table.show[i, col])))) == 1) {
          temp.temp.show[which.max(table.show[i, col])] <- 1
        }
        if ((sum(table.show[i, col] == min(as.integer(table.show[i, col])))) == 1) {
          temp.temp.show[which.min(table.show[i, col])] <- -1
        }
        temp.show[i, c((1 + (j - 1) * ((ncol(table.show) - 2) / 3)):(j * (ncol(table.show) - 2) / 3))] <- temp.temp.show
      }
    }
    table.show <- cbind(table.show, temp.show)
    temp.temp.table.show <- datatable(table.show, extensions = "FixedHeader", editable = list(target = "cell", disable = list(columns = c(0, 3:ncol(table.show)))),
                                      options = list(fixedHeader = TRUE, lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "전체")), pageLength = 10, 
                                                     columnDefs = list(list(targets = c((1 + ncol(table.show) - ncol(temp.show)):ncol(table.show)), visible = FALSE))) 
    )
    temp.temp.table.show <- temp.temp.table.show %>% formatStyle(c("자재비 단가 1", "노무비 단가 1", "경비 단가 1"), borderLeft = "solid 1px black") %>% formatCurrency(c(3:(ncol(table.show) - ncol(temp.show))), currency = ' ￦', interval = 3, mark = ',', digit = 0, before = FALSE)
    for (i in 1:(ncol(table.show) - 2) / 2) {
      temp.temp.table.show <- temp.temp.table.show %>% formatStyle(columns = colnames(table.show)[c(3:(ncol(table.show)-ncol(temp.show)))][i], 
                                                                   valueColumns = colnames(table.show)[c((1+ncol(table.show)-ncol(temp.show)):ncol(table.show))][i], 
                                                                   color = JS("value < 0 ? 'blue' : (value > 0 ? 'red' : 'green')"))
    }
    output$cmp.table <- DT::renderDataTable(temp.temp.table.show)
    memory$cmp.table.proxy <- dataTableProxy("cmp.table")
  }
  )
  
  # Edit table
  observeEvent(input$cmp.table_cell_edit, {
    cmp.edited <- input$cmp.table_cell_edit
    diff <- nrow(memory$table.temp)
    memory$table.temp[cmp.edited$row, cmp.edited$col] <- DT::coerceValue(cmp.edited$value, memory$table.temp[cmp.edited$row, cmp.edited$col])
    memory$table.temp <- match_table(memory$table.temp, memory$tempdata)
    table.show <- data.frame(memory$table.temp[, c(1:(2 + nrow(input$cmp.sheet) * 3))])
    colnames(table.show) <- c("대분류", "규격", paste("자재비 단가", 1:nrow(input$cmp.sheet)), paste("노무비 단가", 1:nrow(input$cmp.sheet)), paste("경비 단가", 1:nrow(input$cmp.sheet)))
    rownames(table.show) <- c(1:nrow(table.show))
    temp.show <- mat.or.vec(nr = nrow(table.show), nc = ncol(table.show) - 2)
    for (i in 1:nrow(table.show)) {
      for (j in 1:3) {
        col <- as.integer(c((3 + (j - 1) * ((ncol(table.show) - 2) / 3)):(2 + j * ((ncol(table.show) - 2) / 3))))
        temp.temp.show <- rep(0, length(col))
        if ((sum(table.show[i, col] == max(as.integer(table.show[i, col])))) == 1) {
          temp.temp.show[which.max(table.show[i, col])] <- 1
        }
        if ((sum(table.show[i, col] == min(as.integer(table.show[i, col])))) == 1) {
          temp.temp.show[which.min(table.show[i, col])] <- -1
        }
        temp.show[i ,c((1 + (j - 1) * ((ncol(table.show) - 2) / 3)):(j * (ncol(table.show) - 2) / 3))] <- temp.temp.show
      }
    }
    table.show <- cbind(table.show, temp.show)
    if (nrow(table.show) !=  diff) {
      temp.temp.table.show <- datatable(table.show, extensions = "FixedHeader", editable = list(target = "cell", disable = list(columns = c(0, 3:ncol(table.show)))),
                                        options = list(fixedHeader = TRUE, lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "전체")), pageLength = 10, 
                                                       columnDefs = list(list(targets = c((1+ncol(table.show)-ncol(temp.show)):ncol(table.show)), visible = FALSE))) 
      )
      temp.temp.table.show <- {temp.temp.table.show %>% formatStyle(c("자재비 단가 1", "노무비 단가 1", "경비 단가 1"), borderLeft = "solid 1px black") %>% formatCurrency(c(3:(ncol(table.show)-ncol(temp.show))), currency = ' ￦', interval = 3, mark = ',', digit = 0, before = FALSE)}
      for (i in 1:(ncol(table.show) - 2) / 2) {
        temp.temp.table.show <- temp.temp.table.show %>% formatStyle(columns = colnames(table.show)[c(3:(ncol(table.show) - ncol(temp.show)))][i], 
                                                                     valueColumns = colnames(table.show)[c((1+ncol(table.show) - ncol(temp.show)):ncol(table.show))][i], 
                                                                     color = JS("value < 0 ? 'blue' : (value > 0 ? 'red' : 'green')"))
      }
      output$cmp.table <- DT::renderDataTable(temp.temp.table.show)
    } else{
      replaceData(memory$cmp.table.proxy, table.show, resetPaging = FALSE)
    }
  })
  
  #########################################################a
  ### Analysis tab
  #########################################################
  # Download example sheet
  anly.example <- load_data("anly.example")
  output$anly.example <- downloadHandler(filename = paste0("2019-5-1808_동우화인켐 평택_세현이엔지_계약", ".xlsx"), 
                                          content = function(file) write.xlsx2(anly.example, file, row.names = FALSE))
  
  # Analyze
  observeEvent(input$anly.sheet, {
    disable("anly.download")
    
    # Read uploaded sheet
    memory$anly.sheet <- read.xlsx2(input$anly.sheet$datapath, sheetIndex = 1, stringsAsFactors = FALSE, colClasses = NA) %>% match_class(search.data[[search.default]])
    
    # Make analytics using previous contract data
    memory$anly.stat <- make_stat(memory$anly.sheet, search.data[[search.default]], 
                                   options = list(sign = input$anly.sign, year = input$anly.year, site = input$anly.site, coop = input$anly.coop), Download = FALSE)
    rownames(memory$anly.stat) <- as.numeric(1:nrow(memory$anly.stat))
    
    # Render stat
    output$anly.stat <- DT::renderDataTable(datatable(isolate(memory$anly.stat), extensions = "FixedHeader", 
                                                       editable = list(target = "cell", disable = list(columns = c(0, 4:9))), escape = FALSE,
                                                       options = list(fixedHeader = TRUE, lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "전체")), 
                                                                      pageLength = -1, columnDefs = list(list(targets = 9, visible = FALSE))), colnames = c('번호' = 1))
                                             %>% (function (dt) {dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]); return(dt)})
                                             %>% formatCurrency(c("자재비.단가", "가격차이"), currency = " ￦", interval = 3, mark = ",", digit = 0, before = FALSE)
                                             %>% formatStyle(columns = "자재비.단가", valueColumns = "가격차이", color = JS("value < 0 ? 'blue' : (value > 0 ? 'red' : 'green')")))
    memory$anly.stat.proxy <- dataTableProxy("anly.stat")
    
    # labortable
    labortable <- labor_making(memory$anly.sheet, memory$labor.data)
    laborsummary <- data.frame(
      덕트공 = sum(as.numeric(labortable$덕트공), na.rm = T),
      보통인부 = sum(as.numeric(labortable$보통인부), na.rm = T),
      보온공 = sum(as.numeric(labortable$보온공), na.rm = T),
      노무비_총합 = sum(as.numeric(labortable$덕트공), na.rm = T) * 168742 + sum(as.numeric(labortable$보통인부), na.rm = T) * 138290 + sum(as.numeric(labortable$보온공), na.rm = T) * 180707, 
      실제_노무비_총합 = sum(as.numeric(memory$anly.sheet$노무비.단가) * as.numeric(memory$anly.sheet$수량), na.rm = T))
    output$anly.labortable <- DT::renderDataTable(datatable(labortable))
    output$anly.laborsummary <- DT::renderDataTable(datatable(laborsummary) %>% formatCurrency(c("노무비_총합", "실제_노무비_총합"), currency = ' ￦', interval = 3, mark = ',', digit = 0, before = FALSE))  
    
    
    # Download stat
    output$anly.download <- downloadHandler(filename = paste0(input$anly.sheet$name, "_분석", ".xlsx"), 
                                             content = function(file) write.xlsx2(make_stat(memory$anly.sheet, search.data[[search.default]], 
                                                                                            options = list(sign = input$anly.sign, year = input$anly.year, site = input$anly.site, coop = input$anly.coop), Download = TRUE), 
                                                                                  file, row.names = FALSE))
    enable("anly.download")
  })
  
  # Options changed
  observeEvent(c(input$anly.sign, input$anly.year, input$anly.site, input$anly.coop), {
    if (!is.null(memory$anly.stat.proxy)) {
      disable("anly.download")
      
      # Make analytics using previous contract data
      memory$anly.stat <- make_stat(memory$anly.sheet, search.data[[search.default]], 
                                     options = list(sign = input$anly.sign, year = input$anly.year, site = input$anly.site, coop = input$anly.coop), Download = FALSE)
      
      # Reload stat
      replaceData(memory$anly.stat.proxy, memory$anly.stat, resetPaging = FALSE)
      
      # Download stat
      output$anly.download <- downloadHandler(filename = paste0("기계약_분석", ".xlsx"), 
                                               content = function(file) write.xlsx2(make_stat(memory$anly.sheet, search.data[[search.default]], 
                                                                                              options = list(sign = input$anly.sign, year = input$anly.year, site = input$anly.site, coop = input$anly.coop)), 
                                                                                    file, row.names = FALSE))
      enable("anly.download")
    }
  })
  
  # Update options list
  observeEvent(memory$search.old$total, {
    disable("anly.download")
    
    updateSliderInput(session, inputId = "anly.year", min = min(memory$search.old$total[[3]]), max = max(memory$search.old$total[[3]]), 
                      value = c(min(memory$search.old$total[[3]]), max(memory$search.old$total[[3]])))
    updatePickerInput(session, inputId = "anly.site", choices = memory$search.old$total[[4]], selected = memory$search.old$total[[4]])
    updatePickerInput(session, inputId = "anly.coop", choices = memory$search.old$total[[5]], selected = memory$search.old$total[[5]])
    
    enable("anly.download")
  })
  
  # Edit table
  observeEvent(input$anly.stat_cell_edit, {
    disable("anly.download")
    
    # Replace data of sheet
    anly.edited <- input$anly.stat_cell_edit
    memory$anly.sheet[anly.edited$row, anly.edited$col] <- DT::coerceValue(anly.edited$value, memory$anly.sheet[anly.edited$row, anly.edited$col])
    
    # Reload stat
    if (anly.edited$col == 2)
      memory$anly.sheet[anly.edited$row, ] <- match_class(memory$anly.sheet[anly.edited$row, -c(1)], search.data[[search.default]], 
                                                            options = list(sign = input$anly.sign, year = input$anly.year, site = input$anly.site, coop = input$anly.coop))
    memory$anly.stat[anly.edited$row, ] <- make_stat(memory$anly.sheet[anly.edited$row, ], search.data[[search.default]], 
                                                       options = list(sign = input$anly.sign, year = input$anly.year, site = input$anly.site, coop = input$anly.coop), Download = FALSE)
    replaceData(memory$anly.stat.proxy, memory$anly.stat, resetPaging = FALSE)
    
    # labortable
    labortable <- labor_making(memory$anly.sheet, memory$labor.data)
    laborsummary <- data.frame(
      덕트공 = sum(as.numeric(labortable$덕트공), na.rm = T),
      보통인부 = sum(as.numeric(labortable$보통인부), na.rm = T),
      보온공 = sum(as.numeric(labortable$보온공), na.rm = T),
      노무비_총합 = sum(as.numeric(labortable$덕트공), na.rm = T) * 168742 + sum(as.numeric(labortable$보통인부), na.rm = T) * 138290 + sum(as.numeric(labortable$보온공), na.rm = T) * 180707, 
      실제_노무비_총합 = sum(as.numeric(memory$anly.sheet$노무비.단가) * as.numeric(memory$anly.sheet$수량), na.rm = T))
    output$anly.labortable <- DT::renderDataTable(datatable(labortable))
    output$anly.laborsummary <- DT::renderDataTable(datatable(laborsummary) %>% formatCurrency(c("노무비_총합", "실제_노무비_총합"), currency = ' ￦', interval = 3, mark = ',', digit = 0, before = FALSE))
    
    enable("anly.download")
  })
  
  #########################################################
  ### labor tab
  #########################################################
  
  observeEvent(input$labor.index, {
    memory$labor.data <- labor.data[[input$labor.index]]
  })
  
  # Update select box contents
  observeEvent(input$labor.chapter, {
    updateSelectInput(session, inputId = "labor.detail", choices = names(memory$labor.data[[input$labor.chapter]]))
  })
  
  # Update select box contents
  observeEvent(input$labor.detail, {
    output$labor.table <- DT::renderDataTable(datatable(memory$labor.data[[input$labor.chapter]][[input$labor.detail]], editable = list(target = "cell", disable = list(columns = c(0)))))
    memory$labor.table.proxy <- dataTableProxy("labor.table")
  })
  
  
  # Upload data
  observeEvent(input$labor.upload, {
    disable("labor.update")
    memory$labor.data <- table_making(input$labor.upload$datapath)
    updateProgressBar(session = session, id = "labor.progress", value = 100)
    updateSelectInput(session, inputId = "labor.chapter", choices = names(memory$labor.data), selected = names(memory$labor.data)[1])
    enable("labor.update")
    enable("labor.reset")
  })
  
  # Edit table
  observeEvent(input$labor.table_cell_edit, {
    disable("labor.update")
    # Replace data of sheet
    labor.edited <- input$labor.table_cell_edit
    memory$labor.data[[input$labor.chapter]][[input$labor.detail]][labor.edited$row, labor.edited$col] <- DT::coerceValue(labor.edited$value, 
                                                                                                                          memory$labor.data[[input$labor.chapter]][[input$labor.detail]][labor.edited$row, labor.edited$col])
    replaceData(memory$labor.table.proxy, memory$labor.data[[input$labor.chapter]][[input$labor.detail]], resetPaging = FALSE)
    enable("labor.update")
  })
  
  # Update archive - display popup message
  observeEvent(input$labor.update, {
    shinyalert(title = "데이터를 추가하시겠습니까?", text = input$labor.upload$name, type = "warning", closeOnClickOutside = TRUE, showCancelButton = TRUE, 
               confirmButtonText = "확인", cancelButtonText = "취소", callbackR = function (x) memory$response$labor.update <- x)
  })
  
  # Update archive - confirm
  observeEvent(memory$response$labor.update, {
    if (memory$response$labor.update) {
      labor.data[[format(Sys.time(), tz = "Asia/Seoul")]] <<- memory$labor.data
      save_data(labor.data)
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
        save_data(labor.data)
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
  
  #########################################################
  ### graph tab
  #########################################################
  
  ### first sub tab - 규격별 업체별 그래프
  # update inputs
  observeEvent(c(input$graph.category1, input$graph.contract1), {
    tmp <- memory$graph.data %>% filter(대분류 == input$graph.category1 &
                                             계약여부 == input$graph.contract1)
    
    updatePickerInput(session = session, 
                      inputId = "graph.site1",
                      choices = unique(tmp$현장),
                      selected = unique(tmp$현장))
  })
  
  observeEvent(c(input$graph.category1, input$graph.contract1, input$graph.site1), {
    tmp <- memory$graph.data %>% filter(대분류 == input$graph.category1 &
                                             계약여부 == input$graph.contract1 &
                                             현장 %in% input$graph.site1)
    
    updatePickerInput(session = session, 
                      inputId = "graph.coop1",
                      choices = unique(tmp$협력사),
                      selected = unique(tmp$협력사))
  })
  
  observeEvent(c(input$graph.category1, input$graph.contract1, input$graph.site1, input$graph.coop1), {
    tmp <- memory$graph.data %>% filter(대분류 == input$graph.category1 &
                                             계약여부 == input$graph.contract1 &
                                             현장 %in% input$graph.site1 &
                                             협력사 %in% input$graph.coop1)
    
    updatePickerInput(session = session, 
                      inputId = "graph.combi1",
                      choices = unique(tmp$combi),
                      selected = unique(tmp$combi))
  })
  
  observeEvent(c(input$graph.category1, input$graph.contract1, input$graph.site1, input$graph.coop1, input$graph.combi1), {
    tmp <- memory$graph.data %>% filter(대분류 == input$graph.category1 &
                                             계약여부 == input$graph.contract1 &
                                             현장 %in% input$graph.site1 &
                                             협력사 %in% input$graph.coop1 &
                                             combi %in% input$graph.combi1)
    
    updatePickerInput(session = session, 
                      inputId = "graph.contnum1",
                      choices = unique(tmp$계약번호),
                      selected = unique(tmp$계약번호))
  })
  
  observeEvent(c(input$graph.category1, input$graph.contract1, input$graph.site1, input$graph.coop1, input$graph.combi1, input$graph.contnum1), {
    memory$graph.data.selected1 <- unique(memory$graph.data %>% filter(현장 %in% input$graph.site1,
                                                                         협력사 %in% input$graph.coop1,
                                                                         combi %in% input$graph.combi1,
                                                                         대분류 == input$graph.category1,
                                                                         계약여부 == input$graph.contract1,
                                                                         계약번호 %in% input$graph.contnum1))
    
  })
  
  # outputs
  observeEvent(memory$graph.data.selected1, {
    output$graph.selected1 <- DT::renderDataTable(datatable(memory$graph.data.selected1 %>% 
                                                              select(-combi, -label, -uniq.label)))
    
  })
  
  output$graph.by.stand <- renderPlotly({
    
    bar <- plot_ly(memory$graph.data.selected1, x=~규격, y=~자재비.단가,
                   color = ~reorder(uniq.label, 자재비.단가))  %>%
      add_bars() %>% 
      layout(title = "규격별 업체별 그래프",
             margin = list(t = 50),
             xaxis=list(title="규격"),
             yaxis=list(title="가격",
                        tickformat = 'digits'))
    
  })
  
  observeEvent(memory$graph.data.selected1, {
    smry <- data.frame(matrix(nrow = 3, ncol = length(unique(memory$graph.data.selected1$규격))))
    smry[is.na(smry)] <- 0
    size_list <- sort(unique(memory$graph.data.selected1$규격))
    colnames(smry) <- size_list
    rownames(smry) <- c('최소값', '중간값', '최대값')
    
    if (nrow(memory$graph.data.selected1) != 0){
      for (i in 1:length(size_list)){
        tmp <- memory$graph.data.selected1 %>% filter(규격 == size_list[i])
        smry[1, i] <- paste(formatC(min(tmp$자재비.단가) , digits = 2, big.mark = ',', format = 'd'), '원')
        smry[2, i] <- paste(formatC(median(tmp$자재비.단가) , digits = 2, big.mark = ',', format = 'd'), '원')
        smry[3, i] <- paste(formatC(max(tmp$자재비.단가) , digits = 2, big.mark = ',', format = 'd'), '원')
      }
    }
    
    output$graph.summary <- DT::renderDataTable(datatable(smry))
    
  })
  
  
  ### Second sub tab - 년도별 업체별 그래프
  # update inputs
  observeEvent(c(input$graph.category2, input$graph.contract2), {
    tmp <- memory$graph.data %>% filter(대분류 == input$graph.category2 &
                                             계약여부 == input$graph.contract2)
    
    updatePickerInput(session = session, 
                      inputId = "graph.standard2",
                      choices = unique(tmp$규격))
  })
  
  observeEvent(c(input$graph.category2, input$graph.contract2, input$graph.standard2), {
    tmp <- memory$graph.data %>% filter(대분류 == input$graph.category2 &
                                             계약여부 == input$graph.contract2 &
                                             규격 == input$graph.standard2)
    
    updatePickerInput(session = session, 
                      inputId = "graph.site2",
                      choices = unique(tmp$현장),
                      selected = unique(tmp$현장))
  })
  
  observeEvent(c(input$graph.category2, input$graph.contract2, input$graph.standard2, input$graph.site2), {
    tmp <- memory$graph.data %>% filter(대분류 == input$graph.category2 &
                                             계약여부 == input$graph.contract2 &
                                             규격 == input$graph.standard2 &
                                             현장 %in% input$graph.site2)
    
    updatePickerInput(session = session, 
                      inputId = "graph.coop2",
                      choices = unique(tmp$협력사),
                      selected = unique(tmp$협력사))
  })
  
  observeEvent(c(input$graph.category2, input$graph.contract2, input$graph.standard2, input$graph.site2, input$graph.coop2), {
    memory$graph.data.selected2 <- unique(memory$graph.data %>% filter(현장 %in% input$graph.site2,
                                                                         협력사 %in% input$graph.coop2,
                                                                         대분류 == input$graph.category2,
                                                                         규격 == input$graph.standard2,
                                                                         계약여부 == input$graph.contract2))
  })
  
  # outputs
  observeEvent(memory$graph.data.selected2, {
    output$graph.selected2 <- DT::renderDataTable(datatable(memory$graph.data.selected2 %>% 
                                                              select(-combi, -label, -uniq.label)))
    
  })
  
  observeEvent(c(input$graph.category2, input$graph.contract2, input$graph.standard2, input$graph.site2, input$graph.coop2), {
    output$graph.by.year <- renderPlotly({
      
      bar <- plot_ly(memory$graph.data.selected2, 
                     x=~연도, 
                     y=~자재비.단가,
                     color = ~reorder(label, 연도))  %>%
        add_bars() %>% 
        layout(title = "년도별 업체별 그래프",
               margin = list(t = 50),
               xaxis=list(title="년도"),
               yaxis=list(title="가격",
                          tickformat = 'digits'))
      
    })
  })
  
  #########################################################
  ### Log tab
  #########################################################
  # Download log
  output$log.download <- downloadHandler(filename = paste0("Log", ".xlsx"), content = function(file) write.xlsx2(memory$logs, file, row.names = FALSE))
  
  # Render log
  output$log <- renderText(print_log(memory$logs))
}

#########################################################################################################################
### Run app
#########################################################################################################################
# Change language
set_labels(
  language = "en", 
  "Please authenticate" = "구매지원 시스템", 
  "Username:" = "ID", 
  "Password:" = "PW", 
  "Login" = "로그인", 
  "Username or password are incorrect" = "올바르지 않은 계정입니다."
)

# Encrypt ui
ui <- secure_app(tags_top = tags$div(tags$img(src = "https://www.shinsungeng.com/resources/images/common/logo.png")), 
                 ui = ui, 
                 tags_bottom = tags$div(tags$p("ID: admin", tags$p("PW: ss"))), 
                 theme = shinytheme("flatly"))

# Run app
shinyApp(ui, server)