library(vegan) # à conserver avant tidyverse / conflit de fonction select
library(aspe)
library(tidyverse)


rm(list=ls())

load(file = 'processed_data/aspe.rda')
#load(file = "processed_data/especes_a_supprimer.rda")
load(file = fichier_aspe)

# Calcul des indicateurs de diversité par opération ----

## Sélection des données ----
# Seuls les inventaires de la base Aspe sont pris en compte. 
# effectif par espèce par operation
# la truite de mer est recodée en truite fario
ope_effectif <- aspe_ope_captures %>%
  filter(str_detect(pro_libelle, pattern = "partielle|ambiances|complète"),
         effectif > 0) %>%
  mutate(code_espece = ifelse(code_espece == "TRM", "TRF", code_espece)) %>% 
  group_by(ope_id,
           code_espece,
           pro_libelle) %>%
  group_by(across(-effectif)) %>%
  summarise(effectif = sum(effectif)) %>%
  ungroup()

# verification des types de pêche 
ope_effectif %>% 
  count(pro_libelle)

# passage en matrice avec ope_id en nom de ligne
ope_effectif_matrice <- ope_effectif %>% 
  pivot_wider(id_cols = ope_id,
              names_from = code_espece,
              values_from = effectif,
              values_fill = 0) %>% 
  column_to_rownames(var = "ope_id")

# Calcul des indices par opération
## Calcul des indices sur les effectifs
ope_indices <- ope_effectif_matrice %>%
  transmute(
    richesse = specnumber(.),
    shannon = diversity(.),
    simpson = diversity(., index = "simpson"),
    pielou = shannon / log(richesse)
  ) %>%
  rownames_to_column(var = "ope_id") %>% 
  mutate(ope_id = as.integer(ope_id))

# Indices au point pour la dernière opération
pop_indices <- ope_indices %>% 
  left_join(y = aspe_passerelle %>% 
              select(pop_id,
                     ope_id) %>% 
              distinct()) %>% 
  mef_ajouter_ope_date()

pop_indices <- pop_indices %>% 
  group_by(pop_id) %>% 
  filter(ope_date == max(ope_date)) %>% 
  ungroup()

# en vue du filtrage des opérations par point
ope_id_der_peche_par_pop_id <- pop_indices %>% 
  pull(ope_id)

# passage du tableau en format long pour les graphiques
pop_indices_long <- pop_indices %>% 
  pivot_longer(richesse:pielou)

# assemblage du tableau pour la modélisation
pop_indices <- pop_indices %>% 
  mutate(code_point = as.integer(pop_id)) %>% 
  left_join(y = aspe_env,
            by = c("code_point" = "pop_id")) %>% 
  left_join(y = aspe_pops_coords,
            by = c("code_point" = "pop_id")) %>% 
  left_join(y = aspe_pops_geo %>% 
              select(pop_id, code_exutoire) %>% 
              sf::st_drop_geometry() %>% 
              distinct()) %>%
  select(pop_id:altitude,
         dist_source = distance_source,
         surf_bv = surface_bv,
         largeur = odp_largeur_lame_eau,
         pente,
         x_wgs84,
         y_wgs84,
         temp_01 = temp_janvier,
         temp_07 = temp_juillet,
         code_exutoire,
         richesse:pielou)

# recherche de variables avec trop de valeurs manquantes
colSums(is.na(pop_indices))

# suppression des stations avec des données incomplètes
pop_indices <- pop_indices %>%  
  drop_na(altitude:temp_07)

# vérification absence de valeurs manquantes
colSums(is.na(pop_indices))

# sauvegarde
save(pop_indices_long,
     pop_indices,
     ope_indices,
     ope_effectif,
     ope_id_der_peche_par_pop_id,
     aspe_pops_geo,
     aspe_passerelle,
     aspe_carto_data,
     file = "processed_data/aspe_macroecologie.rda")

