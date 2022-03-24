# Fonction annotée à compléter

ajout_absence <- function(df) {
  
  # On commence par ajouter toutes les espèces pour chaque opération 
  # Une opération = une pêche
  
  # Permet de sélectionner les codes espèces dans le fichier "passerelle_taxo"
  # disponible dans le package aspe. On ne sélectionne que les codes "valides", 
  # c'est à dire les codes de 3 lettres.
  code_valide <- passerelle_taxo %>%
    filter(nchar(esp_code_alternatif) == 3) %>% 
    pull(esp_code_alternatif)
  
  # On crée un code pour chaque opération
  data <- df %>% 
    mutate(ope_id = paste0(code_station,date_peche,annee,localisation,type_peche)) %>% 
  filter(code_espece %in% code_valide)

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
  # Remplacer NA par 0
  data <- df_ope %>% 
    left_join(data) %>% 
    mutate(effectif = if_else(is.na(effectif), 0, effectif))

}

ajout_absence(data)
