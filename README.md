Flight Delay and Cancellation Dataset (2019-2023)

https://www.kaggle.com/datasets/patrickzel/flight-delay-and-cancellation-dataset-2019-2023?resource=download

Data Analysis, Exploratory, Time-Series, Viz, ML (~29m total rows; 3m SRS)

================================
CẤU TRÚC
================================
1. global.R
→ chạy 1 lần cho toàn app
→ dùng để:
    - load library
    - đọc data
    - khai báo function dùng chung
→ ko để code reactive ở đây

library(shiny)
library(dplyr)

data <- read.csv("data.csv")
my_function <- function(x) {
  x * 2
}

2. ui.R
→ tạo giao diện (HTML)

ui <- fluidPage(
  titlePanel("My Shiny App"),

  sidebarLayout(
    sidebarPanel(
      sliderInput("n", "Choose number:", 1, 100, 10)
    ),
    mainPanel(
      textOutput("result")
    )
  )
)

3. server.R
→ xử lý logic, reactive, output

server <- function(input, output, session) {

  output$result <- renderText({
    paste("Result:", input$n * 2)
  })
}

4. app.R
→ kết nối ui và server

source("global.R")
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)

================================
LUỒNG HOẠT ĐỘNG
================================
User action → input$xxx 
            → reactive / observe
            → output$xxx
            → UI update

global.R
  ↓
ui.R  → tạo layout
  ↓
server.R
  ├── reactive data
  ├── gọi module
  ↓
modules/
  ├── render plot
  └── render table


---------------------------------
Khi nào dùng reactive, observe, observeEvent
---------------------------------
Cần giá trị thay đổi    → reactive()
Chỉ chạy hành động      → observe()
Chạy khi click / event  → observeEvent()

================================
ĐÃ CÓ EDA
================================
EDA (đã có)
   ↓
global.R  → load + xử lý data
   ↓
server.R  → reactive + logic
   ↓
modules/  → vẽ plot
   ↓
ui.R      → layout hiển thị
   ↓
app.R     → nối tất cả
,




