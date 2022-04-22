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
                         "GRV", "GRT", "GRI", "LOU", "MUP", "PLI", "ALF", "BRX",
                         "CYP", "GAX", "HBG", "HYC", "LPX")

ref_espece <- ref_espece %>% 
  rename(code_espece = esp_code_alternatif)
# ------------------------------
# mise en forme du jeu de données au point
# ------------------------------
data <- data %>%
  atlaspoissons::recode_and_filter_species(sp_to_remove = especes_a_supprimer) %>% 
  mutate(code_station = ifelse(
    is.na(code_station),
    paste(round(x_wgs84, 6), round(y_wgs84, 6), sep = "_"),
    code_station
  )) %>%
  mutate(date_peche = as.Date(date_peche),
         ope_id = paste0(code_station, date_peche),
         annee = as.integer(annee)) %>%
  mutate_if(is.character, as.factor) %>%
  group_by_at(vars(-effectif)) %>% 
    summarise(effectif = sum(effectif, na.rm = TRUE)) %>% 
  ungroup()%>% 
  filter(annee > 2010 | # suppression des données anciennes de aspe / wama
         is.na(annee)) %>% 
  droplevels()

# ------------------------------
# présences au point
# ------------------------------
pt_presence <- data %>% 
  select(-code_exutoire,
         -localisation) %>% 
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

rm(pt_presence, pt_absence)


  
  

# bv_simp_geo <- bassins %>% 
#   select(code_exutoire, toponyme, geometry)


# création de l'objet sf des points
pt_geo <- data %>% 
  select(code_station,
         code_exutoire,
         localisation,
         x_wgs84,
         y_wgs84) %>% 
  distinct() %>% 
  st_as_sf(coords = c("x_wgs84", "y_wgs84"),
           crs = 4326)


# ---------------------------------------------
# Donnée au bassin

# attribution sur l'ensemble du jdd des bassins (code_exutoire)
bv_data <- pt_data %>%
  st_as_sf(coords = c("x_wgs84", "y_wgs84"),
           crs = 4326) %>% 
  sf::st_join(bassins %>% 
              select(code_exutoire)) %>%
  filter(!is.na(code_exutoire)) %>%  # au cas où il resterait des stations hors des bassins
  sf::st_drop_geometry()

# détermination du statut par bassin x espèce chaque année
bv_data <- bv_data %>% 
  group_by(code_exutoire, code_espece, annee) %>% 
    summarise(n_abs = sum(statut == "Absent"),
              n_pres = sum(statut == "Présent"),
              n_n_d = sum(statut == "Non détecté")) %>% 
  ungroup() %>% 
  mutate(statut = case_when(
    n_pres > 0 ~ "Présent",
    n_pres == 0 & n_abs > 0 ~ "Absent",
    TRUE ~ "Non détecté"
  ))

# simplification du découpage en bassins
bv_simp_geo <- bassins %>% 
  select(code_exutoire, toponyme, geometry) %>% 
  st_simplify(dTolerance = 20,
              preserveTopology = T)

mon_espece <- "GAR"

bv_map_data <- bv_simp_geo %>% 
 st_drop_geometry() %>%
  left_join(bv_data) %>%
  filter(code_espece == mon_espece) %>%
  group_by(code_exutoire, toponyme, code_espece) %>%
  summarise(n_abs = sum(statut == "Absent"),
            n_pres = sum(statut == "Présent"),
            n_n_d = sum(statut == "Non détecté")) %>%
  ungroup() %>%
  mutate(statut = case_when(
    n_pres > 0 ~ "Présent",
    n_pres == 0 & n_abs > 0 ~ "Absent",
    TRUE ~ "Non détecté"
  ))

prov <- bv_simp_geo %>% 
  left_join(bv_map_data) %>% 
  mutate(statut = ifelse(is.na(statut), "Non prospecté", statut),
         statut = as.factor(statut),
         statut = fct_relevel(statut, c("Présent", "Absent", "Non détecté", "Non prospecté" )))
  

mapview(prov,
        zcol = "statut",
        layer.name = mon_espece,
        map.types = c("OpenStreetMap", "Esri.WorldImagery"),
        col.regions = c("green", "red", "pink", "grey50"))
  






















save.image(file = "processed_data/fish_and_geographical_data.RData")

save(data_pt,
     data_bv,
     pts_geo,
     bv_simp_geo,
     file = "../../atlas_poissons_app/atlas/donnees_appli.RData")
