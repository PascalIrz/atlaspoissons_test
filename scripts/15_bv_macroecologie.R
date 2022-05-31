library(tidyverse)
library(sf)
library(vegan)
library(factoextra)
library(FactoMineR)

rm(list = ls())

load(file = "processed_data/data.RData")
load(file = "../../atlas_poissons_app/atlas/donnees_appli.RData")

rm(data, ref_espece)

data <- pt_data %>%
  select(code_exutoire, code_espece, effectif) %>%
  group_by(code_exutoire, code_espece) %>%
  summarise(eff = sum(effectif)) %>% 
  ungroup() %>% 
  mutate(eff = ifelse(eff > 0, 
                      yes = 1,
                      no =0))


presence <- data %>%
  pivot_wider(names_from = "code_espece",
              values_from = "eff")

bassins_no_geom <- bassins %>% 
  st_drop_geometry()

rm(bassins, pt_data, bv_data, bv_simp_geo, pt_geo)

# =====================================

# Préparation du tableau de données

# Matrice des presences

matrice <- presence %>%
  column_to_rownames(var = "code_exutoire")

# Calcul des indices

indices <- matrice %>% 
  transmute(richesse = specnumber(.),
            shannon = diversity(.),
            simpson = diversity(., index = "simpson"),
            pielou = shannon/log(richesse)) %>% 
  rownames_to_column(var = "code_exutoire")

# verification

# verif <- data %>% 
#   select(-code_espece) %>%
#   group_by(code_exutoire) %>% 
#   summarise(abondance = sum(eff))

# jointure

data_me <- indices %>% 
  left_join(y = bassins_no_geom) %>% 
  mutate(log_richesse = log10(richesse+1))

# carte richesse / surface
ggplot(data = data_me %>% 
         filter(richesse > 0), 
       aes(x = surf_m2, y = log_richesse)) +
  geom_point() +
  scale_x_log10() +
  geom_smooth(method = "loess", se = FALSE)

# carte richesse / altitude
ggplot(data = data_me, aes(x = alt_moy, y = richesse)) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10() +
  geom_smooth(method = "loess", se = FALSE)