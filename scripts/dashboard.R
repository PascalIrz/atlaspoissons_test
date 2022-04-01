# 2 onglets ( ou menus déroulants )
# Welcome, carte avec sélection d'espèces (affichage points et bassins)
# sources de données, méthodologie...

# Essais
# Bassins versants, esssayer de mettre la carte avec une liste déroulante
# des espèces (noms en français plutôt que nom latin)

library(shiny)
library(shinydashboard)
library(mapview)
library(leaflet)
library(leafem)

ui <- dashboardPage(skin = "green",
                    
  dashboardHeader(title = "Atlas des poissons d'eau douce de Bretagne",
                  titleWidth = 440),
  
  sidebar,
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "carto",
              h2("Cartographie"),
              fluidRow(box1,
                       box2)
              ),
    
    tabItem(tabName = "methodo",
            h2("Méthodologie et données"))
    )
  )
)


server <- function(input,output) {
  
  data <- reactive( {rnorm(input$map)})
  
  output$map<-renderLeaflet({ map@map
  })
  
  # output$select
  # 
  # output$mappt<-renderLeaflet({ mapview(datapt_ABH, zcol="presence", cex = "effectif")@map
  # })
  
}

shinyApp(ui,server)
