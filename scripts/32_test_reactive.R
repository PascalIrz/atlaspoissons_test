library(shiny)
library(shinydashboard)
library(mapview)
library(leaflet)
library(leafem)
library(tidyverse)
library(lubridate)
library(sf)
library(aspe)
library(atlaspoissons)
library(devtools)

# =================================================================
#                             DATA
# =================================================================

load(file = "processed_data/fish_and_geographical_data.RData")
datapt <- donner_statut_sp_point(data)
datapt <- datapt %>% 
  left_join(data) %>% 
  st_sf
databv <- donner_statut_sp_bv(data)
databv <- databv %>% 
  left_join(bassins_simp) %>%
  st_sf

data_abl <- data %>% 
  st_drop_geometry() %>% 
  filter(code_espece == "ABL")

data_abh <- data %>% 
  st_drop_geometry() %>% 
  filter(code_espece == "ABH")

espece1 <- "ABH"
espece2 <- "ABL"
databv_abh <- filter(databv,code_espece == espece1)
datapt_abh <- filter(datapt,code_espece == espece1)
map_abh <- mapview(databv_abh, zcol="statut", alpha.region = 0.3) +
  mapview(datapt_abh, zcol="presence", cex = "effectif")

databv_abl <- filter(databv,code_espece == espece2)
datapt_abl <- filter(datapt,code_espece == espece2)
map_abl <- mapview(databv_abh, zcol="statut", alpha.region = 0.3) +
  mapview(datapt_abh, zcol="presence", cex = "effectif")

# =================================================================
#                        TEST SHINY
# =================================================================

ui <- fluidPage(
  
  selectInput("select",
              "Select smth",
              list(`Able de Heckel` = map_abh, Ablette = map_abl)),
  
  hr(),
  fluidRow(box(title = "Fish map", leafletOutput("map"))))

server <- function(input, output) {
  
  selection <- reactive(input$select)
  
  output$map = renderLeaflet({ selection() })
}

shinyApp(ui,server)
