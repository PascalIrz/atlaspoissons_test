library(tidyverse)
library(sf)
library(mapview)

rm(list = ls())

load(file = "../atlas_poissons_app/atlas/donnees_appli.RData")

mon_espece <- "Lamproie marine"
premiere_annee <- 2010
derniere_annee <- 2021

bv <- bv_map_data %>% 
  summarise(n_abs = sum(statut == "Absent"),
            n_pres = sum(statut == "Présent"),
            n_n_d = sum(statut == "Non détecté")) %>% 
  ungroup() %>% 
  mutate(statut = case_when(
    n_pres > 0 ~ "Présent",
    n_pres == 0 & n_abs > 0 ~ "Absent",
    TRUE ~ "Non détecté"
  ))


bv_geo <- bv_map_geo %>% 
  left_join(bv)

pt <- pt_data%>%
  summarise(n_an_abs = sum(statut == "Absent"),
            n_an_pres = sum(statut == "Présent"),
            n_an_n_d = sum(statut == "Non détecté")) %>%
  ungroup() %>%
  mutate(statut = case_when(
    n_an_pres > 0 ~ "Présent",
    n_an_pres == 0 & n_an_abs > 0 ~ "Absent",
    TRUE ~ "Non détecté"
  ))
# Ne donne plus qu'1 observation

pt_g <- pt_geo %>%
  left_join(pt)   %>% 
  st_sf
# Fait tout planter

pt_g <- pt_g %>% 
  filter(esp_nom_commun == mon_espece,
       annee >= premiere_annee,
       annee <= derniere_annee)

bv_geo <- bv_geo %>%
  filter(esp_nom_commun == mon_espece,
         annee >= premiere_annee,
         annee <= derniere_annee)
  
mapview(bv_geo,
        zcol = "statut",
        layer.name = mon_espece,
        map.types = c("OpenStreetMap", "Esri.WorldImagery"),
        col.regions = c("green", "red", "pink", "grey50"),
        alpha.regions = 0.5) + 
  mapview(pt_g,
          zcol = "statut",
          col.regions = c("red", "pink", "green"),
          cex = 4,
          legend = FALSE)
