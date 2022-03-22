statut_station <- donner_statut_sp_point(data)

statut_bv <- donner_statut_sp_bv(data)

data %>%
  pull(code_espece) %>% 
  unique %>% 
  length

data %>% 
  sf::st_drop_geometry() %>% 
  filter(effectif == 0) %>% 
  nrow

#######################################################################
code_valide <- passerelle_taxo %>%
  filter(nchar(esp_code_alternatif) == 3) %>% 
  pull(esp_code_alternatif)

data2 <- data %>% 
  mutate(ope_id = paste0(code_station,date_peche,annee,localisation,type_peche)) %>% 
  filter(code_espece %in% code_valide) %>% 
  droplevels()

ope_id <- data2 %>% 
  pull(ope_id) %>% 
  unique

code_espece <- data2 %>% 
  pull(code_espece) %>% 
  unique


df_ope <- expand.grid(ope_id=ope_id,code_espece=code_espece)

test <- df_ope %>% 
  left_join(data2)

#######################################################################

ope <- data2 %>% 
  select(-code_espece,-effectif) %>% 
  distinct()

ope %>% 
  count(ope_id) %>% 
  arrange(-n)

test2 <- test %>% 
  left_join(data2)
