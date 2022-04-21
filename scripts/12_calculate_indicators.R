

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


n_annees_par_station <- data %>% 
  st_drop_geometry() %>% 
  group_by(code_station) %>% 
  summarise(n_peches = n_distinct(annee)) %>% 
  ungroup()