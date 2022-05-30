library(tidyverse)

rm(list = ls())

load(file = "processed_data/data.RData")
load(file = "../../atlas_poissons_app/atlas/donnees_appli.RData")

rm(data, ref_espece)

test <- pt_data %>%
  select(code_exutoire, code_espece, effectif) %>%
  group_by(code_exutoire, code_espece) %>%
  summarise(eff = sum(effectif))

test1 <- test %>% 
  group_by(code_exutoire, code_espece) %>% 
  filter(eff > 0) %>% 
  mutate(presence = "1") %>% 
  select(code_exutoire,code_espece,presence) %>% 
  ungroup()
 # on enlève par contre tout le reste, on a juste les présences marquées d'un 1

test2 <- test %>% 
  group_by(code_exutoire, code_espece) %>% 
  filter(eff == 0) %>% 
  rename(presence = eff)
# On récupère ici le reste des données (pas nécessaire je suppose qu'après on peut faire un truc pour fill avec des 0)

test_regroupe <- test1 %>% 
  rbind(test1,test2)
# On regroupe test1 et test2

rm(test1, test2, test)
  

test_pivot <- test_regroupe %>%
  pivot_wider(names_from = "code_espece",
              values_to = "presence",
              values_fill = 0)
# Ne fonctionne pas, normal parce qu'il faut garder les code_exutoire
