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

box1 <- box("i am a box")

box2 <- box(
  "Box content here", br(), "More box content",
  sliderInput("slider", "Slider input:", 1, 100, 50),
  textInput("text", "Text input:"))

##### maps #####

maps_ABH <- mapview(databv_ABH, zcol="statut", alpha.region = 0.3) +
  mapview(datapt_ABH, zcol="presence", cex = "effectif")
