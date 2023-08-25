library(tidyverse)
library(sf)
library(mapview)
library(aspe)
library(atlaspoissons)
library(readxl)
library(aspe)
library(readODS)

rm(list = ls())

load(file = "processed_data/carto_data.rda")
load(file = "processed_data/especes_a_supprimer.rda")

# Délimitation de la période temporelle ----
premiere_annee <- 2014

# renommage du référentiel taxo + corrections sur les noms communs ----
ref_espece <- ref_espece %>% 
  rename(code_espece = esp_code_alternatif) %>% 
  mutate(esp_nom_commun = case_when(
    code_espece == "GRE" ~ "Grémille",
    code_espece == "CAG" ~ "Carassin argenté",
    code_espece == "LPP" ~ "Lamproie de Planer",
    code_espece == "TRF" ~ "Truite de rivière",
    code_espece == "PCH" ~ "Poisson-chat",
    code_espece == "PES" ~ "Perche-soleil",
    TRUE ~ esp_nom_commun
  ))

# simplification du découpage en bassins et corrections sur les toponymes ----
bv_simp_geo <- bassins %>% 
  mutate(toponyme = as.character(toponyme), # indispensable pour éviter des bugs dans l'appli
         toponyme = str_to_title(toponyme),
         toponyme = str_replace_all(toponyme,
                                    pattern = " De",
                                    replacement = " de"),
         toponyme = str_replace_all(toponyme,
                                    pattern = " Du",
                                    replacement = " du"),
         toponyme = str_replace_all(toponyme,
                                    pattern = " La ",
                                    replacement = " la "),
         toponyme = str_replace_all(toponyme,
                                    pattern = " Le",
                                    replacement = " le"),
         toponyme = str_replace_all(toponyme,
                                    pattern = " L'",
                                    replacement = " l'"),
         toponyme = str_replace_all(toponyme,
                                    pattern = " D'",
                                    replacement = " d'"),
         toponyme = str_replace_all(toponyme,
                                    pattern = " l'a",
                                    replacement = " l'A"),
         toponyme = str_replace_all(toponyme,
                                    pattern = " l'a",
                                    replacement = " l'A"),
         toponyme = str_replace_all(toponyme,
                                    pattern = " l'e",
                                    replacement = " l'E"),
         toponyme = str_replace_all(toponyme,
                                    pattern = " l'é",
                                    replacement = " l'E"),
         toponyme = str_replace_all(toponyme,
                                    pattern = " l'o",
                                    replacement = " l'O"),
         toponyme = str_replace_all(toponyme,
                                    pattern = " l'u",
                                    replacement = " l'U"),
         toponyme = str_replace_all(toponyme,
                                    pattern = " d'a",
                                    replacement = " D'A"),
         toponyme = str_replace_all(toponyme,
                                    pattern = "L'aber",
                                    replacement = "L'Aber")
         
         ) %>% 
  mutate(toponyme = ifelse(toponyme == "Nr",
                    "Bassin non nommé",
                    toponyme))

bv_env <- bv_simp_geo %>% 
  sf::st_drop_geometry()

bv_simp_geo <- bv_simp_geo %>% 
  select(code_exutoire,
       toponyme,
       geometry) %>% 
  st_simplify(dTolerance = 50,
              preserveTopology = T)

# mise en forme du jeu de données au point ----
# _____________________________________
carto_data <- carto_data %>%
  atlaspoissons::recode_and_filter_species(sp_to_remove = especes_a_supprimer) %>% 
  mutate(code_coords = paste(round(x_wgs84, 6),
                             round(y_wgs84, 6),
                             sep = "_"),
         date_peche = as.Date(date_peche),
         ope_id = paste0(code_coords,
                         code_station,
                         date_peche,
                         annee),
         annee = as.integer(annee)) %>%
  mutate_if(is.character, as.factor) %>%
  group_by_at(vars(-effectif)) %>% 
    summarise(effectif = sum(effectif, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(annee > premiere_annee | # suppression des données anciennes de aspe / wama
         is.na(annee)) %>% 
  droplevels()


# présences au point ---------
# _________________________________
# pour les présences on ne conserve l'ensemble des protocoles
pt_carto_presence <- carto_data %>% 
  select(-code_exutoire) %>% 
  distinct() %>% 
  mutate(code_espece = as.factor(code_espece)) %>% 
  droplevels()

# suppression des espèces jamais présentes
especes_a_garder <- pt_carto_presence %>%
  group_by(code_espece) %>% 
    summarise(effectif = sum(effectif)) %>% 
  filter(effectif > 0) %>% 
  pull(code_espece) %>% 
  droplevels()

pt_carto_presence <- pt_carto_presence %>% 
  filter(code_espece %in% especes_a_garder,
         effectif > 0) %>% 
  mutate(statut = "Présent")


# absences au point --------------
# ____________________________
# caractérisation des pêches selon le protocole en inventaire ou non
peches_inventaires <- carto_data %>% 
  group_by(type_peche) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(inventaire = ifelse(
    str_detect(type_peche, "WAMA|Suivi|Stratifiée|partielle|ambiances|complète|Inventaire|Complète|Atlas"),
    yes = TRUE,
    no = FALSE
  ))

pt_carto_absence <- carto_data %>% 
  filter(code_espece %in% especes_a_garder) %>% 
  atlaspoissons::ajouter_absence() %>% 
  filter(effectif == 0) %>% 
  left_join(peches_inventaires %>% select(-n)) %>% 
  mutate(statut = case_when(
    inventaire ~ "Absent",
    TRUE ~ "Non détecté"
  )) %>%
  select(names(pt_carto_presence))


# assemblage des données au point ----
# __________________________________
pt_carto_data <- rbind(pt_carto_presence, pt_carto_absence) %>% 
  mutate(
    statut = as.factor(statut),
    statut = fct_relevel(statut, c("Non détecté", "Absent", "Présent")),
    statut = factor(statut, ordered = T)) %>% 
  droplevels() %>% 
  select(-ope_id)

pt_carto_data <- pt_carto_data %>% 
  left_join(y = ref_espece %>% 
              select(code_espece,
                     esp_nom_commun)) %>% 
  mutate(statut = factor(statut, ordered = T)) # nécessaire plus loin pour summarise (.. = max(statut)) 



# listes rouges -----
# _________________________

lr_nationale <- data_liste_rouge %>% 
  select(code_espece = esp_code_alternatif,
         lr_nationale = statut_lr_fr)

lr_regionale <- read_ods("raw_data/LRR_RBR_08_avril_2015.ods") %>% 
  filter(LISTE == "PoisEauDou") %>% 
  select(code_espece,
         lr_regionale = LRR)

truite_mer <- data.frame(code_espece = "TRM",
                         lr_regionale = "LC") 

lr_regionale <- lr_regionale %>% 
  rbind(truite_mer)


# passerelle taxonomique + liens fiches INPN -----
# __________________________________________________

# data("data_passerelle_taxo")

passerelle_taxo <- data_passerelle_taxo %>% 
  rename(code_espece = esp_code_alternatif) %>% 
  left_join(y = ref_espece %>% 
              select(code_espece,
                     esp_nom_commun)) %>% 
  filter(!is.na(esp_nom_commun),
         code_espece %in% unique(pt_carto_data$code_espece)) %>% 
  mutate(fiche_inpn1 = paste0("<a href='https://inpn.mnhn.fr/espece/cd_nom/",
                             esp_code_taxref,
                             "' target='_blank'>",
                             "Fiche espèce INPN",
                             "</a>"),
         fiche_inpn2 = paste0("<a href='https://inpn.mnhn.fr/espece/cd_nom/",
                              esp_code_taxref,
                              "' target='_blank'>",
                              esp_nom_commun,
                              "</a>"))

passerelle_taxo <- passerelle_taxo %>% 
  left_join(lr_nationale) %>%  # Ajout statut liste rouge (France)
  left_join(lr_regionale) %>%  # Ajout statut liste rouge (Bretagne)
  expliciter_statut_lr(var = lr_nationale) %>%  
  expliciter_statut_lr(var = lr_regionale)
  
rm(pt_carto_presence, pt_carto_absence)


# spatialisation des points et attribution d'un code_exutoire ----
# _______________________________________________________________
coords <- pt_carto_data %>% 
  select(code_coords,
         localisation,
         x_wgs84,
         y_wgs84) %>% 
  distinct()

pt_carto_data <- pt_carto_data %>%
  st_as_sf(coords = c("x_wgs84", "y_wgs84"),
           crs = 4326,
           remove = FALSE) %>% 
  sf::st_join(bassins %>% 
                select(code_exutoire)) %>% # au cas où il resterait des stations hors des bassins
  filter(!is.na(code_exutoire),
         annee >= premiere_annee) %>% 
  sf::st_drop_geometry()

# création de l'objet sf des points ----
# _________________________________________
pt_carto_geo <- coords %>% 
  st_as_sf(coords = c("x_wgs84", "y_wgs84"),
           crs = 4326) %>% 
  select(code_coords,
         localisation,
         geometry) %>% 
  distinct()

# Donnée au bassin ----
# détermination du statut par bassin x espèce chaque année + ajout effectif
# _______________________________________________________________________
bv_faune <- pt_carto_data %>% 
  group_by(code_exutoire, code_espece, annee) %>% 
    summarise(statut = max(statut)) %>% 
  ungroup() %>% 
  mutate(layerId = code_exutoire)

save(pt_carto_data,
     pt_carto_geo,
     bv_faune,
     bv_env,
     bv_simp_geo,
     passerelle_taxo,
     #file = "../../atlas_poissons_app/atlas/donnees_appli.RData"
     file = "processed_data/carto_data2.rda")

