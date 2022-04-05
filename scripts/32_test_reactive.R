library(shiny)
library(shinydashboard)
library(mapview)
library(leaflet)
library(leafem)

load(file = "processed_data/fish_and_geographical_data.RData")

data_abl <- data %>% 
  st_drop_geometry() %>% 
  filter(code_espece == "ABL")

data_abh <- data %>% 
  st_drop_geometry() %>% 
  filter(code_espece == "ABH")

ui <- fluidPage(
  
  selectInput("select",
              "Select smth",
              list(`Able de Heckel` = data_abh, Ablette = data_abl),
              multiple = FALSE,
              selectize = TRUE,
              width = NULL,
              size = NULL),
  
  hr(),
  fluidRow(column(3, dataTableOutput("data"))))

server <- function(input, output) {
  
  data <- reactive(input$select)
  
  output$data = DT::renderDataTable(
    DT::datatable(data()))
}

shinyApp(ui,server)
