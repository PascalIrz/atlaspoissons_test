library(tidyverse)
library(lubridate)
library(sf)
library(mapview)
library(aspe)
library(atlaspoissons)

rm(list = ls())

load(file = "processed_data/data.RData")

# Liste des espèces à supprimer
especes_a_supprimer <- c("PCC", "ASL", "OCI", "ECR", "MAH", "PCF", "OCV", "ASA",
                         "APP", "APT", "OCL", "GOX", "VAL", "POB", "CRE", "CRC",
                         "GRV", "GRT", "GRI", "LOU", "MUP", "PLI", "ALF", "BRX")

ref_espece <- ref_espece %>% 
  rename(code_espece = esp_code_alternatif)



data <- data %>%
  atlaspoissons::recode_and_filter_species(sp_to_remove = especes_a_supprimer) %>% 
  mutate(code_station = ifelse(
    is.na(code_station),
    paste(round(x_wgs84, 6), round(y_wgs84, 6), sep = "_"),
    code_station
  )) %>%
  mutate(date_peche = as.Date(date_peche),
         ope_id = paste0(code_station, date_peche)) %>%
  mutate_if(is.character, as.factor) %>%
  group_by_at(vars(-effectif)) %>% 
  summarise(effectif = sum(effectif, na.rm = TRUE)) %>% 
  ungroup()%>% 
  left_join(y = ref_espece %>%
              select(code_espece, esp_nom_commun)) %>% 
  # st_as_sf(coords = c("x_wgs84", "y_wgs84"),
  #          crs = 4326) %>%
  filter(annee > 2010 |
           is.na(annee))   # suppression des données anciennes de aspe / wama

especes_a_garder <- data %>% 
#  st_drop_geometry() %>% 
  group_by(code_espece) %>% 
    summarise(effectif = sum(effectif)) %>% 
  filter(effectif > 0) %>% 
  pull(code_espece)

data2 <- data %>% 
  filter(code_espece %in% especes_a_garder) %>% 
  select(-esp_nom_commun) %>% 
  pivot_wider(names_from = "code_espece",
              values_from = "effectif",
              values_fill = 0) %>% 
  pivot_longer(cols = ABH:LPX,
               names_to = "code_espece",
               values_to = "effectif")
  
  

# data3 <- data %>%
#   filter(code_espece %in% especes_a_garder) %>% 
#   ajouter_absence() #%>% 
# #  st_sf()


# compter les pres et abs par esp

prov <- data2 %>% 
#  st_drop_geometry() %>% 
  mutate(presence = effectif > 0) %>% 
  group_by(code_espece, annee, presence) %>% 
    tally() %>% 
  pivot_wider(names_from = presence,
              values_from = n) %>% 
  mutate(somme = `TRUE` + `FALSE`)


prov2 <- data %>% 
  st_drop_geometry() %>% 
  group_by(annee) %>% 
    summarise(n_ope = n_distinct(ope_id))



# attribution sur l'ensemble du jdd des bassins
data <- data %>%
  select(-code_exutoire) %>% 
  sf::st_join(bassins %>% 
                select(code_exutoire, geometry)) %>%
  filter(!is.na(code_exutoire)) # au cas où il resterait des stations hors des bassins

# objet géo des stations
pts_geo <- data %>%
  select(code_station, geometry) %>% 
  group_by(code_station) %>% 
  slice(1) %>% 
  ungroup()

gdata::keep(data,
            pts_geo,
            bv_geo,
            sure = T)

# liste des bassins avec au moins une pêche
liste_bassins_mini_une_peche <- data %>% 
  pull(code_exutoire) %>% 
  unique()

# bassins non prospectés
liste_bassins_sans_peches <- bassins %>% 
  pull(code_exutoire) %>% 
  setdiff(liste_bassins_mini_une_peche)

# carte des bv non prospectés
carte_bv_non_prospectes <- bassins %>%
  filter(code_exutoire %in% liste_bassins_sans_peches) %>%
  mapview()

save(carte_bv_non_prospectes,
     file = 'processed_data/carte_bv_non_prospectes.RData')

# stations totales
liste_stations_tot <- pts_geo %>% 
  pull(code_station) %>% 
  as.character

# nb années de pêche par station
liste_stations_mini_une_peche <- data %>%
  st_drop_geometry() %>% 
  group_by(code_station) %>% 
  summarise(n_annees = n_distinct(annee)) %>% 
  ungroup()

####################################################
#### Référentiel des espèces piscicoles

# data("passerelle_taxo")

# fichier fourni par mail par Thibault
# fish_ref <- readxl::read_xls(path = "raw_data/Codes espèces cemagref.xls")

# on le restreint aux espèces présentes sur le périmètre de l'étude
# prov <- fish %>%
#   filter(effectif > 0) %>%
#   pull(code_espece) %>%
#   unique() %>%
#   droplevels()
# 
# fish_ref <- fish_ref %>% 
#   filter(espoi %in% prov) %>% 
#   rename(nom_espece_FR = esnom, code_espece = espoi, nom_espece_LA = eslat) %>% 
#   mutate_all(as.factor)

# fichier transmis par Thierry Point pour codes Taxref
# corresp <- read.table(file = "raw_data/esp_aspe__cd_nom_2020.12.14.csv",
#                       sep = "",
#                       header = TRUE)



# vérification

# mapview::mapview(bassins, legend = TRUE, layer.name = 'Bassin')


# cartes interactives leaflet

# pour vérification que chaque station ets dans le bon BV - utiliser le tooltip
# mapview::mapview(bassins, zcol = "ID", layer.name = c("Bassins"), alpha.regions = 0.1) +
#  mapview::mapview(stations, zcol = "code_bv", layer.name = c("Stations"))

# Pour savoir combien de couleurs sont nécessaires au minimum pour éviter que deux BV adjacents soient de la même couleur
# on utilise MapColoring::getNColors. Comme il ne fonctionne qu'avec les objets de classe SpatialPolygons* on fait
# une conversion avec as()

# n_colors <- getNColors(as(bassins, 'Spatial'))

# Get Optimal contrast colors
# cand.colors <- rainbow(20)
# opt.colors <- getOptimalContrast(x = as(bassins, 'Spatial'), col = cand.colors)

# Plot
# utiliser https://r-spatial.github.io/mapview/articles/articles/mapview_04-popups.html

# mapview(bassins, zcol = "LIBELLE", layer.name = c("Bassins"), alpha.regions = 0.3, legend = FALSE,
#                  map.types = c("OpenStreetMap", "Esri.WorldImagery", "OpenTopoMap"), col.regions = opt.colors) +
#   mapview(stations, layer.name = c("Stations"), zcol = c("localisation"), legend = FALSE,
#                    popup = popupTable(stations, zcol = c("code_station", "localisation")))


# rm(cand.colors, opt.colors, n_colors)

#fish_non_spatial <- fish %>% 
#  filter(TRUE)

# n_indiv_par_bassin <- data %>% 
#   group_by(code_exutoire) %>% 
#       summarise(n_tot_indiv_captures = sum(effectif, na.rm = T)) %>% 
#   ungroup()

n_stations_par_bassin <- data %>% 
  st_drop_geometry() %>% 
  group_by(code_exutoire) %>% 
  summarise(n_stations_bassin = n_distinct(code_station)) %>% 
  ungroup()

bv_simp_geo <- bassins %>% 
  select(code_exutoire, toponyme, geometry)

n_annees_par_station <- data %>% 
  st_drop_geometry() %>% 
  group_by(code_station) %>% 
  summarise(n_peches = n_distinct(annee)) %>% 
  ungroup()

# ---------------------------------------------
# Donnée au point pour cartes appli

data_pt <- donner_statut_sp_point(data)

# data_pt <- data_pt %>% 
#   left_join(data) %>% 
#   st_sf

data_bv <- donner_statut_sp_bv(data)

# data_bv <- data_bv %>% 
#   left_join(bv_simp_geo) %>%
#   st_sf()


bv_simp_geo <- bassins %>% 
  select(code_exutoire, toponyme, geometry) %>% 
  st_simplify(dTolerance = 50,
              preserveTopology = T)

save.image(file = "processed_data/fish_and_geographical_data.RData")

save(data_pt,
     data_bv,
     pts_geo,
     bv_simp_geo,
     file = "../../atlas_poissons_app/atlas/donnees_appli.RData")
