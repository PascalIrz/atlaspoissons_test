library(tidyverse)
library(sf)
library(vegan)

rm(list = ls())

load(file = "processed_data/data.RData")
load(file = "../../atlas_poissons_app/atlas/donnees_appli.RData")

rm(data, ref_espece)

test <- pt_data %>%
  select(code_exutoire, code_espece, effectif) %>%
  group_by(code_exutoire, code_espece) %>%
  summarise(eff = sum(effectif)) %>% 
  ungroup() %>% 
  mutate(eff = ifelse(eff > 0, 
                      yes = 1,
                      no =0))
  

test_pivot <- test %>%
  pivot_wider(names_from = "code_espece",
              values_from = "eff")

bassins_no_geom <- bassins %>% 
  st_drop_geometry()

rm(bassins, pt_data, bv_data, bv_simp_geo, pt_geo)

# =====================================

# Préparation du tableau de données

# Matrice des effectifs.

matrice <- test_pivot %>%
  column_to_rownames(var = "code_exutoire")



# Ici il faudra filtrer pour virer certains taxons.



# Calcul des indices :

indices <- matrice %>% 
  transmute(richesse = specnumber(.),
            shannon = diversity(.),
            simpson = diversity(., index = "simpson"),
            pielou = shannon/log(richesse)) %>% 
  rownames_to_column(var = "code_exutoire")
  
