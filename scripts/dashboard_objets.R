##### sidebar #####

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Cartographie", tabName = "carto", icon = icon("th")),
    menuItem("Méthodologie", tabName = "methodo",icon = icon("th"), 
             badgeLabel = "new", badgeColor = "green")
  )
)

##### body #####
  


##### boxes #####

  
box1 <- box(leafletOutput("map"))

box2 <- box(selectInput("select",
                        "Select smth",
                        list("Choice 1" = 1, "Choice 2" = 2,
                             "Choice 3" = 3),
                        selected = 1,
                        multiple = FALSE,
                        selectize = TRUE,
                        width = NULL,
                        size = NULL)
            )

##### maps #####

maps_ABH <- mapview(databv_ABH, zcol="statut", alpha.region = 0.3) +
  mapview(datapt_ABH, zcol="presence", cex = "effectif")
