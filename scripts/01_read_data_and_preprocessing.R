##########################################################################
# inventaires piscicoles & données géo
# il faut homogénéiser les jeux de données pour pouvoir les empiler

library(tidyverse)
library(lubridate)
library(sf)
library(mapview)
library(aspe)
library(atlaspoissons)
library(devtools)

rm(list = ls())

#-------------------------------------------------------------------
# données hydrographiques
#-------------------------------------------------------------------

# les bassins versants
# à partir de la couche de Josselin. Si pas à la DR, monter le VPN pour accéder ; sinon 
# base_repo <- "Z:/dr35_projets/PROJETS/ATLAS_POISSONS/ATLAS_SIG/atlas_piscicole_bretagne_20200220/layers"
base_repo <- "raw_data/atlas_piscicole_bretagne_20200220/layers"
bv_file <- "bv_20200132_indicateurs.shp"
bv_path <- paste(base_repo, bv_file, sep = "/")

# comme il y avait pb d'encodage UTF-8 avec st_read(), utilisation de rgdal::readOGR() puis st_as_sf()
# c'est sans doute améliorable
bassins <- rgdal::readOGR(bv_path,
                          use_iconv = TRUE,
                          encoding = "UTF-8")

bassins@data <-  bassins@data %>%
  dplyr::mutate_if(is.character, iconv, 'UTF-8')

bassins <- bassins %>% 
  st_as_sf() %>% 
  `st_crs<-`(2154) %>%
  st_transform(crs = 4326) %>%
  rename(code_exutoire = IDD) %>%
  filter(TRUE) %>% 
  st_make_valid()

save(bassins, file = "processed_data/bassins.RData")
load("processed_data/bassins.RData")

#-------------------------------------------------------------------
# SD
#-------------------------------------------------------------------

# sd_file <- "peche_georect_sd_2015_2019_20210818.shp"
# sd_path <- paste(base_repo, sd_file, sep = "/")
# sd_base <- st_read(dsn = sd_path)
# save(sd_base, file = "raw_data/sd.RData")

load(file = "raw_data/sd.RData")

sd <- sd_base %>%
  atlaspoissons::clean_sd()

save(sd, file = 'processed_data/sd.RData')

rm(sd_base, wama_base)

#-------------------------------------------------------------------
# fédé 56
#-------------------------------------------------------------------

fede56_file <- "peche_fede_56_20200215.shp"
fede56_path <- paste(base_repo, fede56_file, sep = "/")
fede56_base <- st_read(dsn = fede56_path)
save(fede56_base, file = "raw_data/fede56.RData")
load(file = "raw_data/fede56.RData")

fede56 <- fede56_base %>% 
  atlaspoissons::clean_fede()

save(fede56, file = 'processed_data/fede56.RData')

rm(fede56_base)

#-------------------------------------------------------------------
# fédé 35
#-------------------------------------------------------------------
fede35_base <- lire_xlsx_fede35(repertoire = "raw_data/donnees_fd35", 
                                fichier_reference = "CR op pêche elec FD35 2020-VF.xlsx")

fede35 <- fede35_base %>% 
  clean_fede35() 

save(fede35, file = 'processed_data/fede35.RData')

rm(fede35_base)

#-------------------------------------------------------------------
# agence eau Loire Bretagne
#-------------------------------------------------------------------

base_repo <- "raw_data"
file <- "Export_wama_env_poiss_AELB_BZH_2016_2018.xls"
path <- paste(base_repo, file, sep = "/")

agence <- readxl::read_xls(path,
                           sheet = "TempTable") %>% 
  atlaspoissons::clean_agence()

#-------------------------------------------------------------------
# ASPE
#-------------------------------------------------------------------

load(file = "../../../ASPE/raw_data/tables_sauf_mei_2022_03_08_17_55_06.RData")

# ajout du code EPSG aux pop
mes_pops <- point_prelevement %>%
  left_join(y = ref_type_projection,
            by = c("pop_typ_id" = "typ_id"))

# homogénéisation des CRS et passage en sf
coords <- aspe::geo_convertir_coords_df(df = mes_pops,
                                          var_x = pop_coordonnees_x,
                                          var_y = pop_coordonnees_y,
                                          var_crs_initial = typ_code_epsg,
                                          crs_sortie = 4326)



mes_pops <- mes_pops %>% 
  cbind(coords) %>% 
    sf::st_as_sf(coords = c("X", "Y"),
               crs = 4326)

# attribution des bassins aux points pour sélectionner ceux qui sont dans les BV considérés

# vérification !
# mes_pops <- mes_pops %>% 
#   sf::st_join(bassins) %>%
#   filter(!is.na(code_exutoire))
# 
# mapview(mes_pops) 

mes_pops <- mes_pops %>% 
  sf::st_join(bassins) %>%
  filter(!is.na(code_exutoire)) %>%
  pull(pop_id)


# exclusion des points qui ne sont pas dans nos bassins + nettoyage
aspe <- mef_creer_passerelle() %>% 
  filter(pop_id %in% mes_pops) %>% 
  clean_aspe()


# Liste des codes espèces à supprimer
especes_a_supprimer <- c("PCC", "ASL", "OCI", "ECR", "MAH", "PCF", "OCV", "ASA",
                         "APP", "APT", "OCL", "GOX", "VAL", "POB", "CRE", "CRC", "GRV",
                         "GRT", "GRI", "LOU", "MUP", "PLI", "ALF", "BRX")

aspe <- aspe %>% 
  atlaspoissons::recode_and_filter_species(sp_to_remove = especes_a_supprimer)

save(aspe, file = 'processed_data/aspe.RData')



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

#-------------------------------------------------------------------
# WAMA - NB pas de date de pêche ; codes stations sont codes sandre à "padifier"
#-------------------------------------------------------------------

# base_repo <- "//dr35stoc/partages_$/dr35_projets/PROJETS/ATLAS_POISSONS/donnees_geographiques_reference"
# base_repo <- "raw_data/donnees_geographiques_reference"
# wama_file <- "peches_WAMA_BZH_1978_2019_20200215.shp"
# wama_path <- paste(base_repo, wama_file, sep = "/")
# wama_base <- st_read(wama_path)
# save(wama_base, file = "raw_data/wama.RData")

load(file = "raw_data/wama.RData")

wama <- wama_base %>% 
  atlaspoissons::clean_wama()

# on complète les codes sandre station qui ont perdu leurs zéros de tête
# et récupère les libellés des stations depuis la table "station" de aspe
wama <- wama %>% 
  mutate(code_station = str_pad(code_station, width = 8, pad = "0", side = "left")) %>% 
  left_join(station %>% select(code_station = sta_code_sandre,
                               sta_libelle_sandre)) %>% 
  mutate(localisation = sta_libelle_sandre) %>% 
  select(-sta_libelle_sandre)



save(wama, file = 'processed_data/wama.RData')


# ---------------------------------------------------------------------
############ empilement des fichiers + passage en sf

gdata::keep(wama,
            sd,
            fede56,
            fede35,
            aspe,
            agence,
            bassins,
            sure = T)


data <- bind_rows(wama,
                  sd,
                  fede56,
                  fede35,
                  aspe,
                  agence) %>%
  mutate(code_station = ifelse(
    is.na(code_station),
    paste(round(x_wgs84, 6), round(y_wgs84, 6), sep = "_"),
    code_station
  )) %>%
  mutate(date_peche = as.Date(date_peche)) %>%
  mutate(ope_id = paste0(code_station,date_peche)) %>%
  mutate_if(is.character, as.factor) %>%
  group_by_at(vars(-effectif)) %>% 
  summarise(effectif = sum(effectif, na.rm = TRUE)) %>% 
  ungroup()

st_as_sf(coords = c("x_wgs84", "y_wgs84"),
           crs = 4326) %>%
  filter(annee > 2010 |
           is.na(annee)) # suppression des données anciennes de aspe / wama

# attribution sur l'ensemble du jdd des bassins
data <- data %>%
  select(-code_exutoire) %>% 
  sf::st_join(bassins %>% 
                select(code_exutoire, geometry)) %>%
  filter(!is.na(code_exutoire)) # au cas où il resterait des stations hors des bassins

# objet géo des stations
mes_stations <- data %>%
  select(code_station, geometry) %>% 
  group_by(code_station) %>% 
  slice(1) %>% 
  ungroup()

gdata::keep(data,
            mes_stations,
            bassins,
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
liste_stations_tot <- mes_stations %>% 
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

data("passerelle_taxo")

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

bassins_simp <- bassins %>% 
  select(code_exutoire, toponyme, geometry)

n_annees_par_station <- data %>% 
  st_drop_geometry() %>% 
  group_by(code_station) %>% 
    summarise(n_peches = n_distinct(annee)) %>% 
  ungroup()



save.image(file = "processed_data/fish_and_geographical_data.RData")


