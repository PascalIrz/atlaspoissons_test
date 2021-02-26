##########################################################################
# inventaires piscicoles & données géo
# il faut homogénéiser les jeux de données pour pouvoir les empiler

library(data.table)
library(tidyverse)
library(sf)
library(MapColoring)
library(mapview)


detach("package:atlas", unload = TRUE)
library(atlas)

rm(list = ls())


############## Données WAMA - NB pas de date de pêche

# base_repo <- "//dr35stoc/partages_$/dr35_projets/PROJETS/ATLAS_POISSONS/donnees_geographiques_reference"
# base_repo <- "raw_data/donnees_geographiques_reference"
# wama_file <- "peches_WAMA_BZH_1978_2019_20200215.shp"
# wama_path <- paste(base_repo, wama_file, sep = "/")
# wama_base <- st_read(wama_path)
# save(wama_base, file = "raw_data/wama.RData")

load(file = "raw_data/wama.RData")

# =========================================================================
# Fonction de récupération - reprojection des coordonnées
# par défaut la sortie est en wgs84
# sf_obj: objet de classe sf dont il s'agit de collecter les coordonnées
# sf_obj_crs: code EPSG du système de coordonnées de cet objet
# transf_crs: code EPSG du système de coordonnées de sortie (par défaut 4326 pour le WGS84)
# colnames: vecteur indiquant les noms des colonnes de la sortie (par défaut c("x_wgs84", "y_wgs84"))
# get_coords <- function(sf_obj, sf_obj_crs, transf_crs = 4326, colnames = c("x_wgs84", "y_wgs84")) {
#   sf_obj %>% 
#     `st_crs<-`(sf_obj_crs) %>% 
#     st_transform(crs = transf_crs) %>% 
#     st_coordinates() %>% 
#     as.data.frame() %>% 
#     magrittr::set_colnames(colnames)
# } 
# =========================================================================

wama <- wama_base %>% 
  atlas::clean_wama()

# coords <- atlas::get_coords(sf_obj = wama_base,
#                             crs_init = 2154)
# 
# wama <- wama_base %>% 
#   bind_cols(coords) %>% # ajout des coordonnées en wgs84
#   st_drop_geometry() %>% # suppression de la colonnes géométrie
#   filter(!stringr::str_detect(CD_STAT, pattern = "Total")) %>% # suppression du total
#   pivot_longer(cols = ABH:VAX, names_to = "code_espece", values_to = "effectif") %>% 
#   mutate(date_peche = stringr::str_sub(CD_STAT, -4, -1),
#          code_station = stringr::str_sub(CD_STAT, 1, -6),
#          organisme = "WAMA",
#          type_peche = "WAMA",
#          localisation = NA) %>% 
#   select(code_exutoire = IDD,  code_station, localisation, x_wgs84 = X, y_wgs84 = Y, date_peche,
#          organisme, type_peche, code_espece, effectif) %>% 
#   mutate_at(vars(code_station, localisation, date_peche), as.character)
# 
# 
# rm(wama_file, wama_path, wama_base)

save(wama, file = 'processed_data/wama.RData')

############## Données SD

# sd_file <- "peche_georect_sd_2015_2019_20200215.shp"
# sd_path <- paste(base_repo, sd_file, sep = "/")
# sd_base <- st_read(dsn = sd_path)
# save(sd_base, file = "raw_data/sd.RData")
load(file = "raw_data/sd.RData")

sd <- atlas::clean_sd(sd_base)

# coords <- atlas::get_coords(sf_obj = sd_base,
#                             crs_init = 2154)
# sd <- sd_base %>% 
#   st_drop_geometry() %>% 
#   bind_cols(coords) %>% 
#   pivot_longer(cols = ABH:VAX, names_to = "code_espece", values_to = "effectif") %>% 
#   mutate(code_station = NA,
#          date_peche = Date,
#          organisme = "SD OFB",
#          type_peche = "Atlas",
#          localisation = NA) %>% 
#   select(code_exutoire = IDD,  code_station, localisation, x_wgs84, y_wgs84, date_peche,
#          organisme, type_peche, code_espece, effectif) %>% 
#   mutate_at(vars(code_station, localisation, date_peche), as.character)

save(sd, file = 'processed_data/sd.RData')


rm(sd_file, sd_path, sd_base)

############## Données fédé 56


# fede_file <- "peche_fede_56_20200215.shp"
# fede_path <- paste(base_repo, fede_file, sep = "/")
# fede_base <- st_read(dsn = fede_path)
# save(fede_base, file = "raw_data/fede.RData")
load(file = "raw_data/fede.RData")

fede <- atlas::clean_fede(fede_base)
   
coords <- get_coords(sf_obj = fede_base,
                     crs_init = 2154)
  
fede <- fede_base %>% 
  st_drop_geometry() %>% 
  bind_cols(coords) %>% 
  pivot_longer(cols = ABH:VAX, names_to = "code_espece", values_to = "effectif") %>% 
  mutate(code_station = NA, date_peche = NA, localisation = NA, organisme = "Fédé 56") %>% 
  select(code_exutoire = IDD, code_station, localisation, x_wgs84, y_wgs84, date_peche, organisme,
         type_peche = Ctxte_Pech, code_espece, effectif) %>% 
  mutate_at(vars(code_station, localisation, date_peche), as.character)

save(fede, file = 'processed_data/fede.RData')

rm(fede_file, fede_path, fede_base, coords, base_repo)

#################################################################################
##### DONNEES ASPE

load ('raw_data/aspe.RData')

# pour le géoréférencement, le sont les points de prélèvement (préfixe 'pop') qui sont systématiquement référencés
# et non les stations ; par contre il y a un mélange Lambert II étendu / Lambert93.
# les nb d'individus sont la variable lop_effectif
# ATTENTION : comme il y a plusieurs mesures individuelles sur les individus d'un même lot, on ne peut pas simplement sommer
# les effectifs par lot (sinon on multiplie )
# suppression des espèces : mulet porc, plie et alose feinte

# =====================================================================
# Fonction de simplification du dataframe
simplif_aspe_occur <- function(aspe_df) {
  aspe_df %>% 
    select(sta_code_sandre, pop_code_sandre:proj_pop, protocole_peche, ope_date, esp_nom_latin, esp_code_sandre, lop_id,
           lop_effectif) %>% 
    group_by(sta_code_sandre, pop_coordonnees_x, pop_coordonnees_y, proj_pop, protocole_peche, ope_date, esp_nom_latin,
             esp_code_sandre, lop_id, lop_effectif) %>% 
      slice(1) %>% 
      rename(effectif = lop_effectif,
           code_station = sta_code_sandre,
           type_peche = protocole_peche) %>% 
    group_by(code_station, pop_coordonnees_x, pop_coordonnees_y, proj_pop, type_peche, ope_date,
             esp_nom_latin, esp_code_sandre) %>% 
      summarise(effectif = sum(effectif, na.rm = TRUE)) %>% 
    ungroup()
}
# =====================================================================

aspe_occurence <- aspe %>%
  simplif_aspe_occur()

# =====================================================================
# Fonction du conversion de CRS pour un dataframe qui contient des colonnes de coordonnées
transform_crs <- function(aspe_df,
                          coords = c("pop_coordonnees_x", "pop_coordonnees_y"),
                          init_crs,
                          final_crs = 4326,
                          coord_names = c("x_wgs84", "y_wgs84")) {
  
  prov <- aspe_df %>% 
    sf::st_as_sf(coords = coords, crs = init_crs) %>% 
    st_transform(crs = final_crs)
  
  coords <- st_coordinates(prov) %>% 
    as.data.frame() %>% 
    magrittr::set_colnames(coord_names)
  
  bind_cols(prov, coords)
  
}
# =====================================================================

# sous-jeu de données en Lambert II - reprojection en WGS84
aspe_l2 <- aspe_occurence %>% 
  filter(proj_pop == "Lambert II Etendu") %>% 
  transform_crs(init_crs = 27572, final_crs = 4326)

# sous-jeu de données en Lambert 93 - reprojection en WGS84
aspe_l93 <- aspe_occurence %>% 
  filter(proj_pop == "RGF93 / Lambert 93") %>% 
  transform_crs(init_crs = 2154, final_crs = 4326)
  
  
  
#   sf::st_as_sf(coords = c("pop_coordonnees_x", "pop_coordonnees_y"), crs = 2154) %>% 
#   st_transform(crs = 4326)
# 
# coords <- st_coordinates(aspe_l93) %>% 
#   as.data.frame() %>% 
#   magrittr::set_colnames(c("x_wgs84", "y_wgs84"))
# 
# aspe_l93 <- bind_cols(aspe_l93, coords)


# on empile des Lambert 93 et Lambert II, et on ne conserve que la Bretagne
# la fonction rbind fonctionne en géo mais pas bind_rows

fish_aspe <- rbind(aspe_l93, aspe_l2) %>% 
  mutate(esp_code_sandre = as.character(esp_code_sandre)) %>% 
  filter(x_wgs84 < (-0.9), x_wgs84 > (-5.3), y_wgs84 < 49, y_wgs84 > 47) %>% 
  select(-proj_pop)

save(fish_aspe, file = "processed_data/fish_aspe.RData")

# stations ASPE. Pour dédoublonner, pas trouvé mieux que de convertir sf -> df -> sf
# stations_aspe <- fish_aspe %>% 
#  select(code_station, x_wgs84, y_wgs84) %>% 
#  st_drop_geometry() %>% 
#  distinct() %>% 
#  st_as_sf(coords = c("x_wgs84", "y_wgs84"), crs = 4326) %>% 
#  mutate(code_exutoire = NA) %>% 
#  select(code_station, code_exutoire)

# save(fish_aspe, stations_aspe, file = "../processed_data/aspe_bzh.RData")


rm(aspe, aspe_l2, aspe_l93, aspe_occurence, fish_aspe)
  

#################### gestion des codes espèces manquants, recodages, filtres

# comme les codes espèces à trois lettres ne sont pas indiqués, besoin de les récupérer depuis le référentiel
# recodage des carpes, carassins, vandoises etc + de l'épinochette dans l'extrême ouest
# suppression des codes espèces d'écrevisses, crabes, grenouilles, du loup, plie, alose feinte, mulet porc ...
# suppression des brèmes indéterminées BRX


load(file = "processed_data/fish_aspe.RData")

fish_ref <- readxl::read_xlsx(path = "raw_data/ASPE_table_ref_taxon.xlsx") %>% 
  select(code_espece = `Code alternatif`, esp_code_sandre = `Code sandre`) %>% 
  mutate(esp_code_sandre = as.character(esp_code_sandre))

fish_aspe <- fish_aspe %>% 
  left_join(y = fish_ref, by = "esp_code_sandre") %>%
  st_drop_geometry()

# =====================================================================
# Fonction de recodage des codes espèces et d'exclusion de taxons comme les écrevisses
# Par exemple les carpes cuir, miroir, etc. sont regroupées sous un unique code CCX. Idem pour les vandoises en VAX
# Dans l'ouest Finistère, il n'y a que de l'épinoche => recodage de l'épinochette sur cette zone
recode_and_filter_species <- function(df, sp_to_remove) {
  df %>% 
    filter(!code_espece %in% sp_to_remove) %>% 
    mutate(code_espece = str_replace(code_espece, pattern = "CCU", replacement = "CCX"),
           code_espece = str_replace(code_espece, pattern = "CMI", replacement = "CCX"),
           code_espece = str_replace(code_espece, pattern = "CCO", replacement = "CCX"),
           code_espece = str_replace(code_espece, pattern = "CAG", replacement = "CAX"),
           code_espece = str_replace(code_espece, pattern = "CAD", replacement = "CAX"),
           code_espece = str_replace(code_espece, pattern = "CAA", replacement = "CAX"),
           code_espece = str_replace(code_espece, pattern = "CAS", replacement = "CAX"),
           code_espece = str_replace(code_espece, pattern = "VAN", replacement = "VAX"),
           code_espece = str_replace(code_espece, pattern = "VAR", replacement = "VAX"),
           code_espece = ifelse(code_espece == "EPT" & x_wgs84 < (-4.1), "EPI", code_espece))
  }
# =====================================================================

# Liste des codes espèces à supprimer
especes_a_supprimer <- c("PCC", "ASL", "OCI", "ECR", "MAH", "PCF", "OCV", "ASA",
                         "APP", "APT", "OCL", "GOX", "VAL", "POB", "CRE", "CRC", "GRV",
                         "GRT", "GRI", "LOU", "MUP", "PLI", "ALF", "BRX")

fish_aspe <- fish_aspe %>% 
  recode_and_filter_species (sp_to_remove = especes_a_supprimer) %>% 
    group_by(x_wgs84, y_wgs84, type_peche, ope_date, code_espece, code_station) %>% 
        summarise(effectif = sum(effectif, na.rm = TRUE)) %>% 
    ungroup() %>% 
    filter(TRUE)

# Repérage des bredouilles et interprétation de code_espece NA
# le pb est qu'il ne s'agit pas nécessairement de bredouilles car il peut y avoir dans une même pêche des codes espèce
# valides et des NA => signification ?
# on fait l'hypothèse que les seules bredouilles sont les cas où il n'y a que du NA en code espèce pour une pêche 
# en considérant une pêche comme une combinaison (X, Y, date)
# les pêches avec NA comme seul code espèce - on les mets au format de fish_aspe pour pouvoir ensuite les empiler

# bredouilles <- fish_aspe %>% 
#   filter(is.na(code_espece)) %>% 
#   select(x_wgs84, y_wgs84, ope_date, code_station) %>% 
#   left_join(x = fish_aspe, by = c("x_wgs84", "y_wgs84", "ope_date", "code_station")) %>% 
#   group_by(ope_date, type_peche, x_wgs84, y_wgs84, code_station) %>% 
#       tally() %>% 
#   ungroup() %>% 
#   filter(n == 1) %>% 
#   mutate(code_espece = as.character(NA), effectif = as.integer(NA)) %>% 
#   select(x_wgs84, y_wgs84, type_peche, ope_date, code_espece, code_station, effectif)

# traitement des autres codes espèces NA : on supprime les lignes

# fish_aspe <- fish_aspe %>% 
#   filter(!is.na(code_espece))

#  aspe <- rbind(fish_aspe, bredouilles) %>% 
#  filter(TRUE) %>% 
aspe <- fish_aspe %>% 
  mutate(code_exutoire = NA,
         localisation = NA,
         organisme = "ASPE") %>% 
  select(code_exutoire, code_station, localisation, x_wgs84, y_wgs84, date_peche = ope_date, organisme,
         type_peche, code_espece, effectif) %>% 
  mutate_at(vars(code_station, localisation, date_peche), as.character) %>% 
  filter(TRUE)

save(aspe, file = 'processed_data/aspe.RData')

rm(fish_aspe, bredouilles, fish_ref)

####################################################
# ASPE pour Josselin
# On pivote le tableau pour obtenir une colonne par espèce
# fish_aspe_bzh_wide <- aspe %>%
#   pivot_wider(id_cols = code_exutoire:type_peche, names_from = code_espece, values_from = effectif) %>% 
#   filter(TRUE)
# 
# # On ordonne les colonnes espèces par ordre alphabétique, remplace les NA par des zéros
# fish_aspe_bzh_wide <- fish_aspe_bzh_wide %>%
#   select(sort(names(.))) %>% 
#   select(x_wgs84, y_wgs84, type_peche, date_peche, everything()) %>% 
#   mutate_at(vars(ABH:VAX), replace_na, 0L) %>% 
#   select(-`NA`)
# 
# # passage du dataframe en classe sf pour pouvoir l'exporter en shp en Lambert 93
# fish_aspe_bzh_wide_geo <- fish_aspe_bzh_wide %>% 
#   st_as_sf(coords = c("x_wgs84", "y_wgs84"), crs = 4326) %>% 
#   st_transform(crs = 27572)
# 
# st_crs(fish_aspe_bzh_wide_geo)
#   
# st_write(obj = fish_aspe_bzh_wide_geo,
#          dsn = 'processed_data/aspe_bretagne.shp',
#          delete_dsn = TRUE)
# 
# ggplot(fish_aspe_bzh_wide_geo) + geom_sf()
# 
# rm(fish_aspe_bzh_wide, fish_aspe_bzh_wide_geo, fish_aspe, bredouilles, fish_ref)


########################### Données agence eau Loire Bretagne
base_repo <- "raw_data"
file <- "Export_wama_env_poiss_AELB_BZH_2016_2018.xls"
path <- paste(base_repo, file, sep = "/")
agence_base <- readxl::read_xls(path, sheet = "TempTable") 

fish_agence <- agence_base %>% 
  mutate(code_exutoire = NA,
         organisme = "EALB") %>% 
  select(code_exutoire, code_station = CdStationMesureEauxSurface, localisation = NomEntiteHydrographique,
         date_peche = Op_Dtd, organisme, type_peche = L1_Li_Nom, ABH:VAR) %>% 
  mutate(date_peche = as.character(date_peche)) %>%
  mutate_at(vars(ABH:VAR), replace_na, 0L) %>% 
  pivot_longer(cols = ABH:VAR, names_to = "code_espece", values_to = "effectif")

stations_agence <- agence_base %>% 
  select(code_station = CdStationMesureEauxSurface, x_l93 = CoordXPointEauxSurf,
         y_l93 = CoordYPointEauxSurf) %>%
  group_by(code_station) %>% 
      summarise(x_l93 = mean(x_l93, na.rm = TRUE), y_l93 = mean(y_l93, na.rm = TRUE)) %>% 
  ungroup() %>% 
  sf::st_as_sf(coords = c("x_l93", "y_l93"), crs = 2154) %>% 
  st_transform(crs = 4326) 

coords <- stations_agence %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  magrittr::set_colnames(c("x_wgs84", "y_wgs84"))

stations_agence <- cbind(stations_agence, coords) %>% 
  st_drop_geometry()

agence <- left_join(x = fish_agence, y = stations_agence) %>% 
  select(names(sd))

rm(agence_base, coords, fish_agence, stations_agence, base_repo, file, path)


############ empilement des fichiers

# poissons
data <- bind_rows(wama, sd, fede, aspe, agence) %>% 
  # mutate(date_peche = ifelse(str_length(date_peche) == 19, lubridate::ymd_hms(date_peche), date_peche)) %>% 
  # mutate(date_peche = ifelse(str_length(date_peche) == 10, lubridate::ymd(date_peche), date_peche)) %>% 
  mutate_if(is.character, as.factor) %>% 
  filter(TRUE) %>% 
  st_as_sf(coords = c("x_wgs84", "y_wgs84"))

rm(fish_wama, fish_sd, fish_fede)

# stations (après vérification que les CRS sont identiques) ; suppression des stations hors périmètre

class(data)

# Périmètre de l'étude : on exclut les points aberrants.
# On définit comme périmètre les 4 départements + les parties de BV qui "dépassent" vers les régions adjacentes
# Fond de carte téléchargé sur https://www.data.gouv.fr/fr/datasets/r/3096e551-c68d-40ce-8972-a228c94c0ad1
region <- rgdal::readOGR('raw_data/fond_de_carte/departements-20140306-100m.shp') %>% 
  st_as_sf() %>% 
  filter(code_insee %in% c(22, 29, 35, 56)) %>% 
  sf::st_union()

perimetre <- sf::st_union(bassins) %>% 
  c(region) %>% 
  sf::st_union()
  
mapview::mapview(perimetre)

rm(region)


# list(stations_wama, stations_fede, stations_sd) %>% map(st_crs)
#   
# stations <- rbind(stations_wama, stations_sd, stations_fede, stations_aspe) %>% 
#   st_crop(xmin = -6, xmax = 0, ymin = 47, ymax = 49)
# 
# rm(stations_wama, stations_sd, stations_fede, stations_aspe)
# 
# 
# # ggplot(stations)+geom_sf()
# 
# 
# # vérification que les codes stations sont exactement les mêmes dans stations et dans fish
# # normalement on a les mêmes stations dans les deux tables
# setdiff(unique(stations$code_station), unique(fish$code_station))
# intersect(unique(stations$code_station), unique(fish$code_station))



########################################################################
# données hydrographiques
# source "Z:\dr35_projets\PROJETS\ATLAS_POISSONS\ATLAS_SIG\atlas_piscicole_bretagne_20200220\layers"

# les bassins versants
# à partir de la couche de Josselin
base_repo <- "Z:/dr35_projets/PROJETS/ATLAS_POISSONS/ATLAS_SIG/atlas_piscicole_bretagne_20200220/layers"
bv_file <- "bv_20200132_indicateurs.shp"
bv_path <- paste(base_repo, bv_file, sep = "/")

# comme il y avait pb d'encodage UTF-8 avec st_read(), utilisation de rgdal::readOGR() puis st_as_sf()

bassins <- st_read(bv_path) %>% 
  `st_crs<-`(2154) %>% 
  st_transform(crs = 4326) %>% 
  rename(code_exutoire = IDD) %>% 
  filter(TRUE)

# ggplot(data=bassins) + geom_sf() + geom_sf(data = stations)

liste_bassins_tot <- bassins %>% 
  pull(code_exutoire)

# s'il y a des stations en dehors de la couche des bv, pb par la suite => besoin de les exclure
perimeter <- st_union(bassins %>% lwgeom::st_make_valid())

selected_stations <- st_intersects(x = stations, y = perimeter, sparse = FALSE) %>% 
  unlist()

stations <- stations[selected_stations,]

rm(perimeter, bv_file, bv_path, selected_stations)

# pour vérifier : ggplot(data=bassins) + geom_sf() + geom_sf(data = stations)

# liste des bassins avec au moins une pêche
 liste_bassins_mini_une_peche <- stations %>% 
  pull(code_exutoire) %>% 
  unique()

# bassins sans pêches
liste_bassins_sans_peches <- setdiff(liste_bassins_tot, liste_bassins_mini_une_peche)
m <- mapview::mapview(bassins %>% filter(code_exutoire %in% liste_bassins_sans_peches))

save(m, file = '../processed_data/carte_bv_non_prospectes.RData')

# stations totales
liste_stations_tot <- stations %>% 
  pull(code_station) %>% 
  unique()

# stations mini une pêche
liste_stations_mini_une_peche <- fish %>% 
  filter(code_station %in% liste_stations_tot) %>% # pour exclure les stations hors périmètre
  group_by(code_station, date_peche) %>% 
      tally() %>% 
  ungroup() %>% 
  pull(code_station) %>% 
  unique()

####################################################
#### Référentiel des espèces piscicoles fourni par mail par Thibault

fish_ref <- readxl::read_xls(path = "../raw_data/Codes espèces cemagref.xls")

# on le restreint aux espèces présentes sur le périmètre de l'étude
prov <- fish %>%
  filter(effectif > 0) %>%
  pull(code_espece) %>%
  unique() %>%
  droplevels()

fish_ref <- fish_ref %>% 
  filter(espoi %in% prov) %>% 
  rename(nom_espece_FR = esnom, code_espece = espoi, nom_espece_LA = eslat) %>% 
  mutate_all(as.factor)
  

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

n_indiv_par_bassin <- fish %>% 
  group_by(code_exutoire) %>% 
      summarise(n_tot_indiv_captures = sum(effectif, na.rm = T)) %>% 
  ungroup()

n_stations_par_bassin <- fish %>% 
  group_by(code_exutoire) %>% 
      summarise(n_stations_bassin = n_distinct(code_station)) %>% 
  ungroup()

bassins_simp <- bassins %>% 
  select(code_exutoire, toponyme, geometry)

n_peches_par_station <- fish %>% 
  group_by(code_station) %>% 
  summarise(n_peches = n_distinct(date_peche)) %>% 
  ungroup()

rm(bassins, prov, base_repo, stations_fede, stations_sd, stations_wama)

save.image(file = "../processed_data/fish_and_geographical_data.RData")
getwd()


