library(tidyverse)
library(sf)
library(mapview)
library(aspe)
library(atlaspoissons)
library(readxl)
library(aspe)
data(liste_rouge)
library(readODS)

rm(list = ls())

load(file = "processed_data/data.RData")

# Liste des espèces à supprimer
especes_a_supprimer <- c("PCC", "ASL", "OCI", "ECR", "MAH", "PCF", "OCV", "ASA",
                         "APP", "APT", "OCL", "GOX", "VAL", "POB", "CRE", "CRC",
                         "GRV", "GRT", "GRI", "LOU", "MUP", "PLI", "ALF", "BRX",
                         "CYP", "GAX", "HBG", "HYC", "LPX", "PFL")

ref_espece <- ref_espece %>% 
  rename(code_espece = esp_code_alternatif)

# simplification du découpage en bassins
bv_simp_geo <- bassins %>% 
  select(code_exutoire, toponyme, geometry) %>% 
  st_simplify(dTolerance = 50,
              preserveTopology = T)


# ------------------------------
# mise en forme du jeu de données au point
# ------------------------------


# noms_communs <- read_xls("raw_data/Codes espèces cemagref.xls") %>% 
#   select(espoi, esnom) %>%
#   rename(code_espece = espoi,
#          esp_nom_commun = esnom) 

# data <- data %>% 
#   left_join(noms_communs)

data <- data %>%
  atlaspoissons::recode_and_filter_species(sp_to_remove = especes_a_supprimer) %>% 
  mutate(code_coords = paste(round(x_wgs84, 6),
                             round(y_wgs84, 6),
                             sep = "_"),
         date_peche = as.Date(date_peche),
         ope_id = paste0(code_coords,
                         code_station,
                         date_peche),
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
  mutate(
    statut = as.factor(statut),
    statut = fct_relevel(statut, c("Non détecté", "Absent", "Présent")),
    statut = factor(statut, ordered = T)) %>% 
  droplevels() %>% 
  select(-date_peche, -ope_id)

pt_data <- pt_data %>% 
  left_join(y = ref_espece %>% 
              select(code_espece,
                     esp_nom_commun)) %>% 
  mutate(statut = factor(statut, ordered = T)) # nécessaire plus loin pour summarise (.. = max(statut)) 


# ------------------------------
# listes rouges
# ------------------------------

lr_nationale <- liste_rouge %>% 
  select(code_espece = esp_code_alternatif,
         lr_nationale = statut_lr_fr)

lr_regionale <- read_ods("raw_data/LRR_RBR_08_avril_2015.ods") %>% 
  filter(LISTE == "PoisEauDou") %>% 
  select(code_espece,
         lr_regionale = LRR)

truite_mer = data.frame(code_espece = "TRM",
                        lr_regionale = "LC") 

lr_regionale <- lr_regionale %>% 
  rbind(truite_mer)

# ------------------------------
# passerelle taxonomique + liens fiches INPN
# ------------------------------

data("passerelle_taxo")

passerelle_taxo <- passerelle_taxo %>% 
  rename(code_espece = esp_code_alternatif) %>% 
  left_join(y = ref_espece %>% 
              select(code_espece,
                     esp_nom_commun)) %>% 
  filter(!is.na(esp_nom_commun),
         code_espece %in% unique(pt_data$code_espece)) %>% 
  mutate(fiche_inpn = paste0("<a href='https://inpn.mnhn.fr/espece/cd_nom/",
                             esp_code_taxref,
                             "' target='_blank'>",
                             esp_nom_commun,
                             "</a>"))

passerelle_taxo <- passerelle_taxo %>% 
  left_join(lr_nationale) %>%  # Ajout statut liste rouge (France)
  left_join(lr_regionale) %>%  # Ajout statut liste rouge (Bretagne)
  mutate(lr_nationale = case_when(
    lr_nationale == "EX" ~ "Eteint",
    lr_nationale == "EW" ~ "Eteint à l'état sauvage",
    lr_nationale == "CR" ~ "En danger critique d'extinction",
    lr_nationale == "EN" ~ "En danger",
    lr_nationale == "VU" ~ "Vulnérable",
    lr_nationale == "NT" ~ "Quasi menacé",
    lr_nationale == "LC" ~ "Préoccupation mineure",
    lr_nationale == "DD" ~ "Données insuffisantes",
    (lr_nationale == "NE" | is.na(lr_nationale)) ~ "Non évalué"),
    lr_regionale = case_when(
      lr_regionale == "EX" ~ "Eteint",
      lr_regionale == "EW" ~ "Eteint à l'état sauvage",
      lr_regionale == "CR" ~ "En danger critique d'extinction",
      lr_regionale == "EN" ~ "En danger",
      lr_regionale == "VU" ~ "Vulnérable",
      lr_regionale == "NT" ~ "Quasi menacé",
      lr_regionale == "LC" ~ "Préoccupation mineure",
      lr_regionale == "DD" ~ "Données insuffisantes",
      (lr_regionale == "NE" | is.na(lr_regionale)) ~ "Non évalué")
    )
  
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
           crs = 4326) %>% 
  select(code_coords, geometry) %>% 
  distinct()

gdata::keep(pt_data,
            pt_geo,
            bv_simp_geo,
            passerelle_taxo,
            sure = TRUE)

# ---------------------------------------------
# Donnée au bassin
# ---------------------------------------------

# bv_effectif <- pt_data %>% 
#   group_by(code_exutoire,
#            esp_nom_commun) %>% 
#   summarise(effectif = sum(effectif))

# détermination du statut par bassin x espèce chaque année + ajout effectif
bv_data <- pt_data %>% 
  group_by(code_exutoire, code_espece, annee, esp_nom_commun) %>% 
    summarise(statut = max(statut)) %>% 
  ungroup()




save(pt_data,
     pt_geo,
     bv_data,
     bv_simp_geo,
     passerelle_taxo,
     file = "../../atlas_poissons_app/atlas/donnees_appli.RData")

