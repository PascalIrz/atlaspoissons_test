##########################################################################
# inventaires piscicoles & données géo
# il faut homogénéiser les jeux de données pour pouvoir les empiler

library(tidyverse)
library(lubridate)
library(sf)
library(mapview)
library(aspe)
library(atlaspoissons)

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
  st_make_valid() %>% 
  mutate_if(is.character, as.factor)

# save(bassins, file = "processed_data/bassins.RData")
# load("processed_data/bassins.RData")

#-------------------------------------------------------------------
# SD
#-------------------------------------------------------------------

sd_file <- "peche_georect_sd_2015_2019_20210818.shp"
sd_path <- paste(base_repo, sd_file, sep = "/")
sd_base <- rgdal::readOGR(dsn = sd_path)

sd_base@data <-  sd_base@data %>%
  mutate_if(is.character, iconv, 'UTF-8') 

sd_base <- sd_base %>% 
  sf::st_as_sf() 

sd <- sd_base %>%
  atlaspoissons::clean_sd() %>% 
  mutate(effectif = as.numeric(effectif))

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
# attention pas mal de pbs / coordonnées et libellés
fede35_base <- lire_xlsx_fede35(repertoire = "raw_data/donnees_fd35", 
                                fichier_reference = "CR op pêche elec FD35 2020-VF.xlsx")

fede35 <- fede35_base %>% 
  clean_fede35() 

save(fede35, file = 'processed_data/fede35.RData')

rm(fede35_base)

#-------------------------------------------------------------------
# fédé 22
#-------------------------------------------------------------------
fede22_base <- lire_xls_fede22("raw_data/donnees_fede22/FDPPMA 22_baseexcelpêches_scientifiques_OFB 2021.xls")

fede22 <- fede22_base %>% 
  clean_fede22() 

save(fede22, file = 'processed_data/fede22.RData')

rm(fede22_base)

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
                                        var_id = pop_id,
                                        var_crs_initial = typ_code_epsg,
                                        crs_sortie = 4326)



mes_pops <- mes_pops %>% 
  left_join(coords) %>% 
  sf::st_as_sf(coords = c("X", "Y"),
               crs = 4326)

# attribution des bassins aux points pour sélectionner ceux qui sont dans les BV considérés
mes_pops <- mes_pops %>% 
  sf::st_join(bassins) %>% 
  filter(!is.na(code_exutoire)) %>% 
  pull(pop_id)


# exclusion des points qui ne sont pas dans nos bassins + nettoyage
aspe <- mef_creer_passerelle() %>% 
  filter(pop_id %in% mes_pops) %>% 
  clean_aspe()

save(aspe, file = 'processed_data/aspe.RData')

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
  mutate(code_station = str_pad(code_station,
                                width = 8,
                                pad = "0",
                                side = "left")) %>% 
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
            fede22,
            aspe,
            agence,
            bassins,
            ref_espece,
            sure = T)



data <- bind_rows(wama,
                  sd,
                  fede56,
              #    fede35, # tout est à vérifier pour ce jeu de données
                  fede22,
                  aspe,
                  agence)

save(data,
     ref_espece,
     bassins,
     file = "processed_data/data.RData")
