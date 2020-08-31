#########################################################################################################################
### Set up environment
#########################################################################################################################
# Load library
library("shiny")
library("shinydashboard")
library("shinyWidgets")
library("shinyjs")
library("shinyalert")
library("DT")
library("xlsx")
library("pdftools")

# Load function
source("Module.R", encoding = "UTF-8")

# Load data
search.data  <- readRDS("DB/search.data.rds")
search.total <- sapply(c("대분류", "규격", "연도", "현장", "협력사", "계약여부"), function (x) sort(unique(as.character(search.data[[length(search.data)]][[x]]))))

labor.data  <- readRDS("DB/labor.data.rds")
labor.chapter <- "덕트공사"
labor.detail <- labor.data[[length(labor.data)]][[labor.chapter]][[1]]

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
      menuItem("품샘 정보",   tabName = "labor",  icon = icon("file-invoice"))
    ), 
    tags$footer("ver 2.5.0", align = "right", style = "font-size: 15px; position:absolute; bottom:0; width:100%; padding:10px")
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
          column(2, pickerInput("search.major", "대분류:",   choices = search.total[[1]], selected = search.total[[1]], 
                                options = list(`actions-box` = TRUE, `selected-text-format` = "count > 2", `count-selected-text` = "선택: {0} / 전체: {1}", 
                                               `select-all-text` = "전체 선택", `deselect-all-text` = "선택 해제", `live-search`  = TRUE), multiple = TRUE)), 
          column(2, pickerInput("search.std",   "규격:",     choices = search.total[[2]], selected = search.total[[2]], 
                                options = list(`actions-box` = TRUE, `selected-text-format` = "count > 2", `count-selected-text` = "선택: {0} / 전체: {1}", 
                                               `select-all-text` = "전체 선택", `deselect-all-text` = "선택 해제", `live-search`  = TRUE), multiple = TRUE)), 
          column(2, pickerInput("search.year",  "연도:",     choices = search.total[[3]], selected = search.total[[3]], 
                                options = list(`actions-box` = TRUE, `selected-text-format` = "count > 2", `count-selected-text` = "선택: {0} / 전체: {1}", 
                                               `select-all-text` = "전체 선택", `deselect-all-text` = "선택 해제", `live-search`  = TRUE), multiple = TRUE)), 
          column(2, pickerInput("search.site",  "현장:",     choices = search.total[[4]], selected = search.total[[4]], 
                                options = list(`actions-box` = TRUE, `selected-text-format` = "count > 2", `count-selected-text` = "선택: {0} / 전체: {1}", 
                                               `select-all-text` = "전체 선택", `deselect-all-text` = "선택 해제", `live-search`  = TRUE), multiple = TRUE)), 
          column(2, pickerInput("search.coop",  "협력사:",   choices = search.total[[5]], selected = search.total[[5]], 
                                options = list(`actions-box` = TRUE, `selected-text-format` = "count > 2", `count-selected-text` = "선택: {0} / 전체: {1}", 
                                               `select-all-text` = "전체 선택", `deselect-all-text` = "선택 해제", `live-search`  = TRUE), multiple = TRUE)), 
          column(2, pickerInput("search.cntrt", "계약여부:", choices = search.total[[6]], selected = search.total[[6]], 
                                options = list(`actions-box` = TRUE, `selected-text-format` = "count > 2", `count-selected-text` = "선택: {0} / 전체: {1}", 
                                               `select-all-text` = "전체 선택", `deselect-all-text` = "선택 해제", `live-search`  = TRUE), multiple = TRUE)), 
          width = NULL)),
        
        # Create a new row for table
        DT::dataTableOutput("search.table"), 
        
        # Modify data
        fluidRow(
          box(
            column(3, 
                   fileInput("search.upload", "엑셀 파일 업로드", multiple = FALSE, accept = ".xlsx"), 
                   div(style = "margin-top: -20px;"), 
                   disabled(actionButton("search.update", "업데이트", icon = icon("cloud-upload-alt"))), 
                   div(style = "float: right;", disabled(actionButton("search.reset", "초기화", icon = icon("undo"))))
            ), 
            column(6), 
            column(3, 
                   selectInput("search.index", "데이터 선택", choices = rev(names(search.data))), 
                   downloadButton("search.download", "다운로드"), 
                   div(style = "float: right;", actionButton("search.remove", "데이터 삭제", icon = icon("trash")))
            ), 
            width = NULL
          )
        )
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
          column(9, fileInput("cntrt.sheet", "엑셀 파일 업로드", multiple = FALSE, accept = ".xlsx")), 
          
          # Check boxes for options
          column(1, fluidRow(checkboxInput("cntrt.site", "동일 현장",   value = FALSE), 
                             checkboxInput("cntrt.coop", "동일 협력사", value = FALSE))), 
          column(2, div(style = "float: right; margin-top: 20px;", downloadButton("cntrt.example", "예시파일"))), 
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
        # Modify data
        fluidRow(
          # tags$style(".shiny-file-input-progress {display: none}"), 
          box(
            column(9, 
                   fileInput("labor.upload", "pdf 파일 업로드", multiple = FALSE, accept = c('.pdf')), 
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
        ),
        
        fluidRow(box(
          column(3, 
                 selectInput("labor.chapter", "챕터 선택", choices = names(labor.data[[length(labor.data)]]), 
                             div(style = "float: right;")
                 )
          ),
          column(3, 
                 selectInput("labor.detail", "상세 선택", choices = names(labor.data[[length(labor.data)]][[labor.chapter]]), 
                             div(style = "float: right;")
                 )
          ),
          column(6),
          width = NULL)),
        # Create a new row for table
        DT::dataTableOutput("labor.table")
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
  memory <- reactiveValues(search.old = list(choices = search.total, selected = search.total))
  
  #########################################################
  ### Search tab
  #########################################################
  # Preset
  search.labels <- c("대분류", "규격", "연도", "현장", "협력사", "계약여부")
  search.id <- c("search.major", "search.std", "search.year", "search.site", "search.coop", "search.cntrt")
  
  # Update select box contents
  observeEvent(sapply(search.id, function (x) list(input[[x]])), {
    search.selected <- sapply(search.id, function (x) list(input[[x]]))
    memory$search.old <- update_picker(session, search.data[[input$search.index]][search.labels], search.total, memory$search.old$choices, search.labels, search.id, search.selected)
  })
  
  # Update table
  observeEvent(memory$search.old$selected, {
    # Filter data based on selections
    memory$search.table <- make_table(search.data[[input$search.index]][-c(2)], search.labels, memory$search.old$selected)
    
    # Render table
    output$search.table <- DT::renderDataTable(datatable(memory$search.table, extensions = "FixedHeader", editable = list(target = "cell"), 
                                                         options = list(fixedHeader = TRUE, lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "전체"))))
                                               %>% formatCurrency(c("자재비", "노무비", "경비"), currency = ' ￦', interval = 3, mark = ',', digit = 0, before = FALSE))
  })
  
  # Edit table
  observeEvent(input$search.table_cell_edit, {
    disable("search.download")
    proxy <- dataTableProxy('search.table')
    
    # Replace data
    search.edited <- input$search.table_cell_edit
    memory$search.table[search.edited$row, search.edited$col] <- DT::coerceValue(search.edited$value, memory$search.table[search.edited$row, search.edited$col])
    
    # Reload table
    replaceData(proxy, memory$search.table, resetPaging = FALSE)
    enable("cntrt.download")
  })
  
  # Upload data
  observeEvent(input$search.upload, {
    disable("search.update")
    memory$search.sheet <- read.xlsx2(input$search.upload$datapath, sheetIndex = 1, stringsAsFactors = FALSE, colClasses = NA)
    enable("search.update")
    enable("search.reset")
  })
  
  # Update archive - display popup message
  observeEvent(input$search.update, {
    shinyalert(title = "데이터를 추가하시겠습니까?", text = input$search.upload$name, type = "warning", closeOnClickOutside = TRUE, showCancelButton = TRUE, 
               confirmButtonText = "확인", cancelButtonText = "취소", callbackR = function (x) memory$response$search.update <- x)
  })
  
  # Update archive - confirm
  observeEvent(memory$response$search.update, {
    if (memory$response$search.update) {
      memory$search.sheet <- rbind(memory$search.sheet, search.data[[length(search.data)]])
      search.data[[format(Sys.time(), tz = "Asia/Seoul")]] <<- memory$search.sheet
      saveRDS(search.data, file = "DB/search.data.rds")
      updateSelectInput(session, inputId = "search.index", choices = rev(names(search.data)), selected = rev(names(search.data))[1])
      enable("search.remove")
      disable("search.update")
      disable("search.reset")
      memory$search.sheet <<- NULL
      reset("search.upload")
      memory$response$search.update <- FALSE
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
      disable("search.update")
      disable("search.reset")
      memory$search.sheet <<- NULL
      reset("search.upload")
      memory$response$search.reset <- FALSE
    }
  })
  
  # Download data
  output$search.download <- downloadHandler(filename = paste0("기계약정보 ", format(as.POSIXct(input$search.index), "%Y_%m_%d_%H_%M_%S"), ".xlsx"), 
                                            content = function(file) write.xlsx2(search.data[[input$search.index]], file, row.names = FALSE))
  
  # Select data
  observeEvent(input$search.index, {
    search.total <- sapply(search.labels, function (x) sort(unique(as.character(search.data[[input$search.index]][[x]]))))
    memory$search.old <- update_picker(session, search.data[[input$search.index]][search.labels], search.total, NULL, search.labels, search.id, search.total)
  })
  
  # Remove data - display popup message
  observeEvent(input$search.remove, {
    shinyalert(title = "데이터를 삭제하시겠습니까?", text = input$search.index, type = "warning", closeOnClickOutside = TRUE, showCancelButton = TRUE, 
               confirmButtonText = "확인", cancelButtonText = "취소", callbackR = function (x) memory$response$search.remove <- x)
  })
  
  # Remove data - confirm
  observeEvent(memory$response$search.remove, {
    if (memory$response$search.remove) {
      if (length(search.data) > 1) {
        search.data[input$search.index] <<- NULL
        saveRDS(search.data, file = "DB/search.data.rds")
        shinyalert(title = "데이터가 삭제되었습니다.", text = input$search.index, type = "success", closeOnClickOutside = TRUE, confirmButtonText = "확인")
        updateSelectInput(session, inputId = "search.index", choices = rev(names(search.data)), selected = rev(names(search.data))[1])
      } else {
        shinyalert(title = "마지막 데이터는\n삭제할 수 없습니다!", text = input$search.index, type = "error", closeOnClickOutside = TRUE, confirmButtonText = "확인")
      }
      if (length(search.data) == 1) 
        disable("search.remove")
      memory$response$search.remove <- FALSE
    }
  })
  
  #########################################################
  ### Contract tab
  #########################################################
  # Download example sheet
  output$cntrt.example <- downloadHandler(filename = paste0("계약서_예시", ".xlsx"), content = function(file) write.xlsx2(readRDS("DB/cntrt.example.rds"), file, row.names = FALSE))
  
  # Analyze
  observeEvent(input$cntrt.sheet, {
    disable("cntrt.download")
    
    # Read uploaded sheet
    memory$cntrt.sheet <- read.xlsx2(input$cntrt.sheet$datapath, sheetIndex = 1, stringsAsFactors = FALSE, colClasses = NA)
    memory$cntrt.sheet <- match_class(memory$cntrt.sheet, search.data[[input$search.index]])
    
    # Make analytics using previous contract data
    memory$cntrt.stat <- make_stat(memory$cntrt.sheet, search.data[[input$search.index]])
    
    # Render stat
    output$cntrt.stat <- DT::renderDataTable(datatable(memory$cntrt.stat, extensions = "FixedHeader", editable = list(target = "cell", disable = list(columns = c(0, 4:8))), 
                                                       options = list(fixedHeader = TRUE, lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "전체")), pageLength = 25))
                                             %>% formatCurrency(c("자재비.단가", "최저가", "평균가", "중간가", "최고가"), currency = ' ￦', interval = 3, mark = ',', digit = 0, before = FALSE))
    
    # Download stat
    output$cntrt.download <- downloadHandler(filename = paste0("기계약_분석", ".xlsx"), content = function(file) write.xlsx2(memory$cntrt.stat, file, row.names = FALSE))
    enable("cntrt.download")
  })
  
  # Edit table
  observeEvent(input$cntrt.stat_cell_edit, {
    disable("cntrt.download")
    proxy <- dataTableProxy("cntrt.stat")
    
    # Replace data of sheet
    cntrt.edited <- input$cntrt.stat_cell_edit
    memory$cntrt.sheet[cntrt.edited$row, cntrt.edited$col] <- DT::coerceValue(cntrt.edited$value, memory$cntrt.sheet[cntrt.edited$row, cntrt.edited$col])
    
    # Reload stat
    if (cntrt.edited$col == 2)
      memory$cntrt.sheet[cntrt.edited$row, ] <- match_class(memory$cntrt.sheet[cntrt.edited$row, -c(1)], search.data[[input$search.index]])
    memory$cntrt.stat[cntrt.edited$row, ] <- make_stat(memory$cntrt.sheet[cntrt.edited$row, ], search.data[[input$search.index]])
    replaceData(proxy, memory$cntrt.stat, resetPaging = FALSE)
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
    labor.sheet <<- table_making(input$labor.upload$datapath)
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
      labor.data[[format(Sys.time(), tz = "Asia/Seoul")]] <<- labor.sheet
      saveRDS(labor.data, file = "DB/labor.data.rds")
      updateSelectInput(session, inputId = "labor.index", choices = rev(names(labor.data)))
      enable("labor.remove")
      disable("labor.update")
      disable("labor.reset")
      labor.sheet <<- NULL
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
      labor.sheet <<- NULL
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
