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
  summarise(presence = case_when(
    test$eff > 1 ~ 1,
    TRUE ~ 0
  )) %>% 
  unique()
 # affiche 0... si on ne met pas unique() on a 105 millions de résultats