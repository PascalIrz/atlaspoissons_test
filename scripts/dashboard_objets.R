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
                        list(data$code_espece),
                        selected = 1,
                        multiple = FALSE,
                        selectize = TRUE,
                        width = NULL,
                        size = NULL
                        )
            )

##### maps #####

databv_abh <- filter(databv,code_espece == "ABH")
datapt_abh <- filter(datapt,code_espece == "ABH")
map_abh <- mapview(databv_abh, zcol="statut", alpha.region = 0.3) +
  mapview(datapt_abh, zcol="presence", cex = "effectif")

