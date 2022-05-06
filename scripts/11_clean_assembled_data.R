library(tidyverse)
library(sf)
library(mapview)
library(aspe)
library(atlaspoissons)
library(readxl)

rm(list = ls())

load(file = "processed_data/data.RData")

# Liste des espèces à supprimer
especes_a_supprimer <- c("PCC", "ASL", "OCI", "ECR", "MAH", "PCF", "OCV", "ASA",
                         "APP", "APT", "OCL", "GOX", "VAL", "POB", "CRE", "CRC",
                         "GRV", "GRT", "GRI", "LOU", "MUP", "PLI", "ALF", "BRX",
                         "CYP", "GAX", "HBG", "HYC", "LPX")

ref_espece <- ref_espece %>% 
  rename(code_espece = esp_code_alternatif)

# simplification du découpage en bassins
bv_simp_geo <- bassins %>% 
  select(code_exutoire, toponyme, geometry) %>% 
  st_simplify(dTolerance = 20,
              preserveTopology = T)


# ------------------------------
# mise en forme du jeu de données au point
# ------------------------------

# Test: enlever les points à l'extérieur des bassins (sans succès)
# data <- data %>% 
#   filter(!geometry == bassins$geometry)

noms_communs <- read_xls("raw_data/Codes espèces cemagref.xls") %>% 
  select(espoi, esnom) %>%
  rename(code_espece = espoi,
         esp_nom_commun = esnom) 

# data <- data %>% 
#   left_join(noms_communs)

data <- data %>%
  atlaspoissons::recode_and_filter_species(sp_to_remove = especes_a_supprimer) %>% 
  mutate(code_coords = paste(round(x_wgs84, 6), round(y_wgs84, 6), sep = "_")) %>%
  mutate(date_peche = as.Date(date_peche),
         ope_id = paste0(code_coords, code_station, date_peche),
         annee = as.integer(annee)) %>%
  mutate_if(is.character, as.factor) %>%
  group_by_at(vars(-effectif)) %>% 
    summarise(effectif = sum(effectif, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(annee > 2010 | # suppression des données anciennes de aspe / wama
         is.na(annee)) %>% 
  droplevels()

# ------------------------------
# présences au point
# ------------------------------
pt_presence <- data %>% 
  select(-code_exutoire) %>% 
  distinct() %>% 
  mutate(code_espece = as.factor(code_espece)) %>% 
  droplevels()

# suppression des espèces jamais présentes
especes_a_garder <- pt_presence %>%
  group_by(code_espece) %>% 
    summarise(effectif = sum(effectif)) %>% 
  filter(effectif > 0) %>% 
  pull(code_espece) %>% 
  droplevels()

pt_presence <- pt_presence %>% 
  filter(code_espece %in% especes_a_garder,
         effectif > 0) %>% 
  mutate(statut = "Présent")

# ------------------------------
# absences au point
# ------------------------------
# caractérisation des pêches selon le protocole en inventaire ou non
peches_inventaires <- data %>% 
  group_by(type_peche) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(inventaire = case_when(
    str_detect(type_peche, "ndice") ~ FALSE,
    str_detect(type_peche, "truite") ~ FALSE,
    str_detect(type_peche, "IA ") ~ FALSE,
    str_detect(type_peche, "partiel") ~ FALSE,
    TRUE ~ TRUE
  ))

pt_absence <- data %>% 
  filter(code_espece %in% especes_a_garder) %>% 
  atlaspoissons::ajouter_absence() %>% 
  filter(effectif == 0) %>% 
  left_join(peches_inventaires %>% select(-n)) %>% 
  mutate(statut = case_when(
    inventaire ~ "Absent",
    TRUE ~ "Non détecté"
  )) %>%
  select(names(pt_presence))

# ------------------------------
# assemblage des données au point
# ------------------------------
pt_data <- rbind(pt_presence, pt_absence) %>% 
  mutate(statut = as.factor(statut)) %>% 
  droplevels() %>% 
  select(-date_peche, -ope_id)

pt_data <- pt_data %>% 
  left_join(noms_communs)

rm(pt_presence, pt_absence)


# attribution sur l'ensemble du jdd des bassins (code_exutoire)
coords <- pt_data %>% 
  select(code_coords,
         x_wgs84,
         y_wgs84) %>% 
  distinct()

pt_data <- pt_data %>%
  st_as_sf(coords = c("x_wgs84", "y_wgs84"),
           crs = 4326) %>% 
  sf::st_join(bassins %>% 
                select(code_exutoire)) %>% # au cas où il resterait des stations hors des bassins
  filter(!is.na(code_exutoire)) %>% 
  sf::st_drop_geometry() %>% 
  left_join(coords)

# création de l'objet sf des points
pt_geo <- coords %>% 
  st_as_sf(coords = c("x_wgs84", "y_wgs84"),
           crs = 4326)

gdata::keep(pt_data,
            pt_geo,
            bv_simp_geo,
            sure = TRUE)


# pt_data_geo <- pt_data %>% 
#   group_by(code_coords, code_exutoire, esp_nom_commun, code_espece, code_station, annee) %>% 
#   # summarise(n_an_abs = sum(statut == "Absent"),
#   #           n_an_pres = sum(statut == "Présent"),
#   #           n_an_n_d = sum(statut == "Non détecté")) %>%
#   # ungroup() %>%
#   # mutate(statut = case_when(
#   #   n_an_pres > 0 ~ "Présent",
#   #   n_an_pres == 0 & n_an_abs > 0 ~ "Absent",
#   #   TRUE ~ "Non détecté"
#   # )) %>% 
#   left_join(pt_geo) %>% 
#   st_sf

# ---------------------------------------------
# Donnée au bassin
# ---------------------------------------------

# détermination du statut par bassin x espèce chaque année
bv_data <- pt_data %>% 
  group_by(code_exutoire, code_espece, annee, esp_nom_commun) 
# %>% 
#     summarise(n_abs = sum(statut == "Absent"),
#               n_pres = sum(statut == "Présent"),
#               n_n_d = sum(statut == "Non détecté")) %>% 
#   ungroup() %>% 
#   mutate(statut = case_when(
#     n_pres > 0 ~ "Présent",
#     n_pres == 0 & n_abs > 0 ~ "Absent",
#     TRUE ~ "Non détecté"
#   ))



bv_map_data <- bv_simp_geo %>% 
  st_drop_geometry() %>%
  left_join(bv_data) %>%
  group_by(code_exutoire, toponyme, code_espece, esp_nom_commun, annee) 
# %>%
#   summarise(n_abs = sum(statut == "Absent"),
#             n_pres = sum(statut == "Présent"),
#             n_n_d = sum(statut == "Non détecté")) %>%
#   ungroup() %>%
#   mutate(statut = case_when(
#     n_pres > 0 ~ "Présent",
#     n_pres == 0 & n_abs > 0 ~ "Absent",
#     TRUE ~ "Non détecté"
#   ))

bv_map_data_geo <- bv_simp_geo %>%
  left_join(bv_map_data)
# %>%
#   mutate(statut = ifelse(is.na(statut), "Non prospecté", statut),
#          statut = as.factor(statut),
#          statut = fct_relevel(statut, c("Présent", "Absent", "Non détecté", "Non prospecté" )))

bv_map_geo <- bv_map_data_geo %>% 
  select(code_exutoire, geometry) %>% 
  unique()

pt_geo <- pt_geo %>% 
  select(code_coords, geometry) %>% 
  distinct()

pt_data_geo_esp <- pt_data_geo %>%
  st_sf
















# save.image(file = "processed_data/fish_and_geographical_data.RData")

save(pt_data,
     pt_geo,
     bv_map_data,
     bv_map_data_geo,
     file = "../../atlas_poissons_app/atlas/donnees_appli.RData")
