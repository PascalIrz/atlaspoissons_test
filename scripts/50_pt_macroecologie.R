library(tidyverse)
# library(sf)
# library(vegan)
# library(factoextra)
# library(FactoMineR)
# library(mapview)
library(aspe)

# NB ONLY ON THE ASPE DATA

rm(list = ls())

load(file = "processed_data/data2.rda")
# load(file = "../../atlas_poissons_app/atlas/donnees_appli.RData")

# Calcul des indicateurs de diversité
# slt la dernière année de données pour chaque point
biodiv <- pt_data %>% 
  filter(source_donnee == "Aspe" &
         str_detect(type_peche, "WAMA|Suivi|Stratifiée|partielle|ambiances|complète|Inventaire|Complète|Atlas"),
         effectif > 0) %>% 
  group_by(vars(code_station:y_wgs84)) %>% 
  filter(date_peche == max(date_peche))

presence <- bv_faune %>% 
  filter(statut != "Non détecté") %>% 
  mutate(statut = ifelse(statut == "Présent", 1, 0)) %>% 
  group_by(code_exutoire, code_espece) %>% 
  summarise(statut = max(statut)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "code_espece",
              values_from = "statut")

# Préparation du tableau de données ====
# ________________________________________

# Matrice des presences

matrice_presence <- presence %>%
  column_to_rownames(var = "code_exutoire")

# Calcul des indices

indices <- matrice_presence %>%
  transmute(
    richesse = specnumber(.),
    shannon = diversity(.),
    simpson = diversity(., index = "simpson"),
    pielou = shannon / log(richesse)
  ) %>%
  rownames_to_column(var = "code_exutoire")  

  



