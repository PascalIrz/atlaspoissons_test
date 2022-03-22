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

#######################################################################

# Fonction annotée à compléter

ajout_absence <- function(df) {
  
  # Permet de sélectionner les codes espèces dans le fichier "passerelle_taxo"
  # disponible dans le package aspe. On ne sélectionne que les codes "valides", 
  # c'est à dire les codes de 3 lettres.
  code_valide <- passerelle_taxo %>%
    filter(nchar(esp_code_alternatif) == 3) %>% 
    pull(esp_code_alternatif)
  
  # On crée un code pour chaque opération
  data <- df %>% 
    mutate(ope_id = paste0(code_station,date_peche,annee,localisation,type_peche))) %>% 
  filter(code_espece %in% code_valide) %>% 
  droplevels()

# On récupère le code de chaque opération sous forme de vecteur
ope_id <- data %>% 
  pull(ope_id) %>% 
  unique

# On récupère le code espèce sous forme de vecteur
code_espece <- data %>% 
  pull(code_espece) %>% 
  unique

# On crée un nouveau dataframe en joignant les deux vecteurs, on renomme les 
# variables
df_ope <- expand.grid(ope_id=ope_id,code_espece=code_espece)

# On rejoint ce nouveau dataframe à l'ancien
data <- df_ope %>% 
  left_join(data)

}