library(tidyverse)

rm(list = ls())

load(file = "processed_data/data.RData")
load(file = "../../atlas_poissons_app/atlas/donnees_appli.RData")

rm(data, ref_espece)

test <- pt_data %>%
  select(code_exutoire, code_espece, effectif) %>%
  group_by(code_exutoire, code_espece) %>%
  summarise(eff = sum(effectif))

test <- test %>% 
  group_by(code_exutoire, code_espece) %>% 
  filter(eff > 0) %>% 
  mutate(presence = "1") %>% 
  select(code_exutoire,code_espece,presence)
 # on enlève par contre tout le reste, on a juste les présences marquées d'un 1

