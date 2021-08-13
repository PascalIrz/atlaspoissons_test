##########################################################################
# inventaires piscicoles & données géo
# il faut homogénéiser les jeux de données pour pouvoir les empiler

library(tidyverse)
library(sf)
library(mapview)
library(aspe)
library(atlas)

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
  filter(TRUE)

save(bassins, file = "processed_data/bassins.RData")
load("processed_data/bassins.RData")

#-------------------------------------------------------------------
# WAMA - NB pas de date de pêche
#-------------------------------------------------------------------

# base_repo <- "//dr35stoc/partages_$/dr35_projets/PROJETS/ATLAS_POISSONS/donnees_geographiques_reference"
# base_repo <- "raw_data/donnees_geographiques_reference"
# wama_file <- "peches_WAMA_BZH_1978_2019_20200215.shp"
# wama_path <- paste(base_repo, wama_file, sep = "/")
# wama_base <- st_read(wama_path)
# save(wama_base, file = "raw_data/wama.RData")

load(file = "raw_data/wama.RData")

wama <- wama_base %>% 
  atlas::clean_wama()

save(wama, file = 'processed_data/wama.RData')

#-------------------------------------------------------------------
# SD
#-------------------------------------------------------------------

# sd_file <- "peche_georect_sd_2015_2019_20200215.shp"
# sd_path <- paste(base_repo, sd_file, sep = "/")
# sd_base <- st_read(dsn = sd_path)
# save(sd_base, file = "raw_data/sd.RData")

load(file = "raw_data/sd.RData")

sd <- sd_base %>%
  atlas::clean_sd()

save(sd, file = 'processed_data/sd.RData')

rm(sd_base, wama_base)

#-------------------------------------------------------------------
# fédé 56
#-------------------------------------------------------------------

# fede_file <- "peche_fede_56_20200215.shp"
# fede_path <- paste(base_repo, fede_file, sep = "/")
# fede_base <- st_read(dsn = fede_path)
# save(fede_base, file = "raw_data/fede.RData")
load(file = "raw_data/fede.RData")

fede <- fede_base %>% 
  atlas::clean_fede()

save(fede, file = 'processed_data/fede.RData')

rm(fede_base)

#-------------------------------------------------------------------
# agence eau Loire Bretagne
#-------------------------------------------------------------------

base_repo <- "raw_data"
file <- "Export_wama_env_poiss_AELB_BZH_2016_2018.xls"
path <- paste(base_repo, file, sep = "/")

agence <- readxl::read_xls(path,
                           sheet = "TempTable") %>% 
  clean_agence()

#-------------------------------------------------------------------
# ASPE
#-------------------------------------------------------------------

load(file = "../../../ASPE/package/aspe_test/processed_data/toutes_tables_aspe_sauf_mei.RData")

# On complète de référentiel des CRS
ref_type_projection <- ref_type_projection %>%
  mutate(typ_code_epsg = ifelse((is.na(typ_code_epsg) & typ_libelle_sandre == "Lambert II Etendu"),
                                yes = 27572,
                                no = typ_code_epsg))

# ajout du code EPSG aux pop
mes_pops <- point_prelevement %>%
  left_join(y = ref_type_projection,
            by = c("pop_typ_id" = "typ_id"))

# homogénéisation des CRS et passage en sf
mes_pops <- geo_convertir_coords_df(df = mes_pops,
                                    var_x = "pop_coordonnees_x",
                                    var_y = "pop_coordonnees_y",
                                    var_crs_initial = "typ_code_epsg",
                                    crs_sortie = 4326) %>%
  sf::st_as_sf(coords = c("X", "Y"),
               crs = 4326)

# attribution des bassins aux points
mes_pops <- mes_pops %>% 
  sf::st_join(bassins) %>%
  filter(!is.na(code_exutoire)) %>% 
  pull(pop_id)

# Exclusion des points qui ne sont pas dans nos bassins + nettoyage
aspe <- mef_creer_passerelle() %>% 
  filter(pop_id %in% mes_pops) %>% 
  atlas::clean_aspe()


# Liste des codes espèces à supprimer
especes_a_supprimer <- c("PCC", "ASL", "OCI", "ECR", "MAH", "PCF", "OCV", "ASA",
                         "APP", "APT", "OCL", "GOX", "VAL", "POB", "CRE", "CRC", "GRV",
                         "GRT", "GRI", "LOU", "MUP", "PLI", "ALF", "BRX")

aspe <- aspe %>% 
  recode_and_filter_species (sp_to_remove = especes_a_supprimer)

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
# aspe <- fish_aspe %>% 
#   mutate(code_exutoire = NA,
#          localisation = NA,
#          organisme = "ASPE") %>% 
#   select(code_exutoire, code_station, localisation, x_wgs84, y_wgs84, date_peche = ope_date, organisme,
#          type_peche, code_espece, effectif) %>% 
#   mutate_at(vars(code_station, localisation, date_peche), as.character) %>% 
#   filter(TRUE)

save(aspe, file = 'processed_data/aspe.RData')

# rm(fish_aspe, bredouilles, fish_ref)
# 
# aspe <- aspe %>% 
#   mutate_at(vars(code_station, localisation, date_peche), as.character)

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





############ empilement des fichiers + passage en sf

data <- bind_rows(wama, sd, fede, aspe, agence) %>% 
  mutate(code_station = ifelse(is.na(code_station),
                               paste(round(x_wgs84, 6), round(y_wgs84, 6), sep = "_"),
                               code_station)) %>% 
  mutate_if(is.character, as.factor) %>%
  filter(TRUE)

# objet géo des stations plus rapide à créer à ce stade qu'une fois de data passé en géo
mes_stations <- data %>%
  select(code_station, x_wgs84, y_wgs84) %>% 
  distinct() %>% 
  st_as_sf(coords = c("x_wgs84", "y_wgs84"),
           crs = 4326)
# passage de data en objet sf
data <- data %>%
  st_as_sf(coords = c("x_wgs84", "y_wgs84"),
           crs = 4326)

gdata::keep(data,
            mes_stations,
            bassins,
            sure = T)

# data %>% sample_n(1000) %>% mapview

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

# attribution sur l'ensemble du jdd des bassins
data <- data %>%
  select(-code_exutoire) %>% 
  sf::st_join(bassins %>% 
                select(code_exutoire, geometry)) %>%
  filter(!is.na(code_exutoire))

# pour vérifier : ggplot(data=bassins) + geom_sf() + geom_sf(data = stations, col = "#2892c4")# liste des bassins avec au moins une pêche
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


