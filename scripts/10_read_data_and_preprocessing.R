# _________________
# inventaires piscicoles & données géo -----
# il faut homogénéiser les jeux de données pour pouvoir les empiler

library(tidyverse)
library(lubridate)
library(sf)
library(mapview)
library(aspe)
library(atlaspoissons)

rm(list = ls())

# _________________
# données hydrographiques ----
# _________________

# les bassins versants
# à partir de la couche de Josselin. Si pas à la DR, monter le VPN pour accéder ; sinon 
# base_repo <- "Z:/dr35_projets/PROJETS/ATLAS_POISSONS/ATLAS_SIG/atlas_piscicole_bretagne_20200220/layers"
base_repo <- "raw_data/atlas_piscicole_bretagne_20200220/layers"
bv_file <- "bv_20200132_indicateurs.shp"
bv_dbf <- "bv_20200132_indicateurs.dbf"

bv_path <- paste(base_repo, bv_file, sep = "/")
bv_dbf <- paste(base_repo, bv_dbf, sep = "/")

# test <- foreign::read.dbf(bv_dbf, as.is = TRUE) %>% 
#   mutate(toponyme = ifelse(toponyme == "NR", NA, toponyme))
# 
# Encoding(test$toponyme) <- c("UTF-8")

#test2 <- shp::read_dbf(bv_path, col_spec = "?")





# comme il y avait pb d'encodage UTF-8 avec st_read(), utilisation de rgdal::readOGR() puis st_as_sf()
# c'est sans doute améliorable
bassins <- rgdal::readOGR(bv_path,
                         use_iconv = TRUE,
                         encoding = "UTF-8"
                         )


#stringi::stri_enc_toutf8(bassins@data$toponyme, is_unknown_8bit  = TRUE)[1:50]

bassins@data <-  bassins@data %>%
  dplyr::mutate_if(is.character, iconv, 'UTF-8')

bassins <- bassins %>% 
  st_as_sf() %>% 
  `st_crs<-`(2154) %>%
  st_transform(crs = 4326) %>%
  rename(code_exutoire = IDD) %>%
  filter(TRUE) %>% 
  st_make_valid() %>% 
  mutate_if(is.character, as.factor)




# bassins2 <- sf::st_read(bv_path) %>% 
#   as.data.frame() %>% 
#   dplyr::mutate_if(is.character, iconv, 'UTF-8') %>% 
#   mutate(pb = iconv(pb, "UTF-8"))
#   
# mutate_if(is.character, stringi::stri_encode, from = "UTF-8", to = "UTF-8")
# 
# bassins2 %>% 
#   filter(IDD == "exut_11") %>% 
#   pull(pb) %>% 
#   iconv(to = "UTF-8")
# 
# bassins3 <- terra::vect(bv_path) %>%
#   st_as_sf() %>%
#   dplyr::mutate_if(is.character, iconv, 'UTF-8')
# 
# bassins4 <- sf::read_sf(bv_path, options="ENCODING=UTF-8") 

# save(bassins, file = "processed_data/bassins.RData")
# load("processed_data/bassins.RData")

# Liste des espèces à supprimer ----
especes_a_supprimer <- c("PCC", "ASL", "OCI", "ECR", "MAH", "PCF", "OCV", "ASA",
                         "APP", "APT", "OCL", "GOX", "VAL", "POB", "CRE", "CRC",
                         "GRV", "GRT", "GRI", "LOU", "MUP", "PLI", "ALF", "BRX",
                         "CYP", "GAX", "HBG", "HYC", "LPX", "PFL")



# _________________
# SD ----
# _________________
base_repo <- "raw_data/donnees_geographiques_reference"
sd_file <- "peche_georect_sd_2015_2019_20210818.shp"
sd_path <- paste(base_repo, sd_file, sep = "/")
sd_base <- rgdal::readOGR(dsn = sd_path)

sd_base@data <-  sd_base@data %>%
  mutate_if(is.character, iconv, 'UTF-8') 

sd_base <- sd_base %>% 
  sf::st_as_sf() 

sd_carto_data <- sd_base %>%
  atlaspoissons::clean_sd() %>% 
  mutate(effectif = as.numeric(effectif))

# save(sd_carto_data, file = 'processed_data/sd.RData')

# rm(sd_base, wama_base)

# _________________
# agence eau Loire Bretagne ----
# _________________

base_repo <- "raw_data"
file <- "Export_wama_env_poiss_AELB_BZH_2016_2018.xls"
path <- paste(base_repo, file, sep = "/")

agence_carto_data <- readxl::read_xls(path,
                           sheet = "TempTable") %>% 
  atlaspoissons::clean_agence()

# _________________
# ASPE ----
# _________________
fichier_aspe <- misc_nom_dernier_fichier(repertoire = "../../../../../ASPE/raw_data/rdata",
                                         pattern = "^tables")

load(file = fichier_aspe)

# ajout du code EPSG aux pop
aspe_pops <- point_prelevement %>%
  left_join(y = ref_type_projection,
            by = c("pop_typ_id" = "typ_id"))

# homogénéisation des CRS et passage en sf
coords <- aspe::geo_convertir_coords_df(df = aspe_pops,
                                        var_x = pop_coordonnees_x,
                                        var_y = pop_coordonnees_y,
                                        var_id = pop_id,
                                        var_crs_initial = typ_code_epsg,
                                        crs_sortie = 4326)



aspe_pops_geo <- aspe_pops %>% 
  left_join(coords) %>% 
#  filter(Y > 47, Y < 49) %>% # qq points qui posent problème
  sf::st_as_sf(coords = c("X", "Y"),
               crs = 4326) %>% 
# attribution des bassins aux points pour sélectionner ceux qui sont dans les BV considérés
  sf::st_join(bassins) %>% 
  filter(!is.na(code_exutoire)) %>% 
  select(pop_id,
         code_exutoire,
         toponyme)

mapview(sample_n(aspe_pops_geo, 1000))

aspe_pops <- aspe_pops_geo %>%  
  pull(pop_id)


aspe_pops_coords <- cbind(sf::st_drop_geometry(aspe_pops_geo),
                          sf::st_coordinates(aspe_pops_geo)) %>%
  rename(x_wgs84 = X,
         y_wgs84 = Y)

# exclusion des points qui ne sont pas dans nos bassins + nettoyage
aspe_passerelle <- mef_creer_passerelle() %>% 
  filter(pop_id %in% aspe_pops) %>% 
  mef_ajouter_ope_date()

# données environnementales au point
aspe_env <- aspe_passerelle %>% 
  mef_ajouter_ope_desc_peche() %>% 
  mef_ajouter_ope_env() %>% 
  mef_ajouter_ipr() %>% 
 # mef_ajouter_ope_date() %>% 
  select(sta_id:ope_id,
         ope_date,
         annee,
         odp_longueur,
         odp_largeur_lame_eau,
         distance_mer:temp_janvier) %>% 
  distinct() %>% 
  mef_ajouter_type_protocole() %>% 
  filter(str_detect(pro_libelle, "Pêche")) # seulement les inventaires

# les valeurs nulles dans certaines variables sont en fait des NA
# comme certaines années il y a des valeurs manquantes on agrège au point en moyennant
aspe_env <- aspe_env %>%
  select(pop_id,
         odp_longueur:temp_janvier) %>%
  pivot_longer(cols = odp_longueur:temp_janvier) %>%
  filter(!(
    name %in% c(
      "odp_longueur",
      "odp_largeur_lame_eau",
      "surface_bv",
      "distance_source",
      "largeur",
      "pente",
      "profondeur",
      "temp_juillet",
      "temp_janvier"
    ) & value == 0
  )) %>%
  group_by(pop_id, name) %>%
  summarise_all(mean, na.rm = T) %>%
  ungroup() %>%
  pivot_wider(#id_cols = pop_id,
    names_from = name,
    values_from = value,
    values_fill = NA)

aspe_ope_captures <- aspe_passerelle %>% 
  mef_ajouter_lots() %>% 
  mef_ajouter_esp() %>% 
  mef_ajouter_type_protocole() %>% 
  rename(code_espece = esp_code_alternatif) %>% 
  select(-starts_with("esp")) %>% 
  left_join(aspe_pops_coords) %>% 
  atlaspoissons::recode_and_filter_species(sp_to_remove = especes_a_supprimer) %>% 
  group_by(ope_id, code_espece, pro_libelle) %>% 
    summarise(effectif = sum(lop_effectif, na.rm = TRUE)) %>% 
  ungroup()

aspe_carto_data <- aspe_passerelle %>% 
  select(-ope_date#,
         #-lop_id, -pre_id
         ) %>%
  distinct() %>% 
  clean_aspe()


save(aspe_carto_data,
     aspe_pops,
     aspe_pops_geo,
     aspe_pops_coords,
     aspe_env,
     aspe_passerelle,
     aspe_ope_captures,
     ref_protocole,
     fichier_aspe,
     file = 'processed_data/aspe.rda')


# WAMA ----
# NB pas de date de pêche ; codes stations sont codes sandre à "padifier"

# base_repo <- "//dr35stoc/partages_$/dr35_projets/PROJETS/ATLAS_POISSONS/donnees_geographiques_reference"
# base_repo <- "raw_data/donnees_geographiques_reference"
# wama_file <- "peches_WAMA_BZH_1978_2019_20200215.shp"
# wama_path <- paste(base_repo, wama_file, sep = "/")
# wama_base <- st_read(wama_path)
# save(wama_base, file = "raw_data/wama.RData")

load(file = "raw_data/wama.RData")

wama_carto_data <- wama_base %>% 
  atlaspoissons::clean_wama()

# on complète les codes sandre station qui ont perdu leurs zéros de tête
# et récupère les libellés des stations depuis la table "station" de aspe
wama_carto_data <- wama_carto_data %>% 
  mutate(code_station = str_pad(code_station,
                                width = 8,
                                pad = "0",
                                side = "left")) %>% 
  left_join(station %>% select(code_station = sta_code_sandre,
                               sta_libelle_sandre)) %>% 
  mutate(localisation = sta_libelle_sandre) %>% 
  select(-sta_libelle_sandre)



# save(wama_carto_data, file = 'processed_data/wama.RData')


# Fédés départementales ----
fedes_carto_data <- lire_fichier_fedes(chemin = "raw_data/fedes_departementales_peche.xlsx") %>% 
  clean_fede()


############ Empilement des fichiers et sauvegarde -----

gdata::keep(wama_carto_data,
            sd_carto_data,
            fedes_carto_data,
            aspe_carto_data,
            aspe_pops_geo,
            agence_carto_data,
            bassins,
            ref_espece,
            especes_a_supprimer,
            sure = T)


carto_data <- bind_rows(wama_carto_data,
                  sd_carto_data,
                  fedes_carto_data,
                  aspe_carto_data,
                  agence_carto_data)

save(carto_data,
     ref_espece,
     especes_a_supprimer,
     bassins,
     file = "processed_data/carto_data.rda")

save(especes_a_supprimer,
     file = "processed_data/especes_a_supprimer.rda")
