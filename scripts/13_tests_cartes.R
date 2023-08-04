library(tidyverse)
library(sf)
library(mapview)
library(leafpop)


rm(list = ls())

load(file = "../../atlas_poissons_app/atlas/donnees_appli.RData")

# ==============================================================================
# Assemblage des données


# Filtration des données pour choisir espèces et années

mon_espece <- "Chabot"
premiere_annee <- 2020
derniere_annee <- 2022

# ==============================================================================
# AU POINT

pt_data_aggr <- pt_data %>% 
  left_join(y = data_passerelle_taxo,
            by = "code_espece")
  
filter(annee >= premiere_annee,
         annee <= derniere_annee,
         esp_nom_commun == mon_espece) %>% 
  group_by(code_coords, esp_nom_commun, localisation, effectif, statut_lr_fr, fiche) %>%
    summarise(statut = max(statut)) %>% # car les statuts sont un facteur ordonné "non detect" < "absence" < "presence"
  ungroup()

pt_map_data <- pt_geo %>% 
  left_join(pt_data_aggr) %>% 
  mutate(
    statut = as.character(statut),
    statut = ifelse(is.na(statut),
                    "Non prospecté",
                    statut))

# mapview(pt_map_data,
#         zcol = "statut",
#         col.region = c("red", "pink", "grey90", "green"),
#         layer.name = mon_espece,
#         map.types = c("OpenStreetMap", "Esri.WorldImagery"))




# ==============================================================================
# AU BV

bv_data_aggr <- bv_data %>% 
  filter(annee >= premiere_annee,
         annee <= derniere_annee,
         esp_nom_commun == mon_espece) %>% 
  group_by(code_exutoire, code_espece, esp_nom_commun) %>%
    summarise(statut = max(statut)) %>% # car les statuts sont un facteur ordonné "non detect" < "absence" < "presence"
  ungroup()


bv_map_data <- bv_simp_geo %>% 
  left_join(bv_data_aggr) %>% 
  mutate(statut = as.character(statut),
         statut = ifelse(is.na(statut),
                          "Non prospecté",
                          statut),
         esp_nom_commun = ifelse(is.na(esp_nom_commun),
                              mon_espece,
                              esp_nom_commun)) 



# ==============================================================================
# Visualisation de la carte

mapview(bv_map_data,
        zcol = "statut",
        layer.name = mon_espece,
        map.types = c("OpenStreetMap", "Esri.WorldImagery"),
        col.regions = c("red", "pink", "grey70", "green"),
        alpha.regions = 0.5) + 
  mapview(pt_map_data,
          zcol = "statut",
          col.region = c("red", "pink", "grey90", "green"),
          map.types = c("OpenStreetMap", "Esri.WorldImagery"),
          cex = ifelse("statut" == "Non prospecté", 1, 3),
          legend = FALSE)



