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
  select(code_exutoire,code_espece,presence) %>% 
  ungroup()
 # on enlève par contre tout le reste, on a juste les présences marquées d'un 1

code_exutoire <- test %>%
  pull(code_exutoire) %>%
  unique()

code_espece <- test %>% 
  pull(code_espece) %>% 
  unique()

test2 <- test %>% 
  select(code_espece, presence) %>% 
  pivot_wider(names_from = "code_espece", 
              values_to = "presence", 
              values_fill = 0)
# Ne fonctionne pas, normal parce qu'il faut

