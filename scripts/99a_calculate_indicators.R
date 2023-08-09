load(file = "../../atlas_poissons_app/atlas/donnees_appli.RData")

library(tidyverse)
library(sf)

# liste des bassins avec au moins une pêche
liste_bassins_mini_une_peche <- pt_data %>% 
  pull(code_exutoire) %>% 
  as.character() %>% 
  unique()

# bassins non prospectés
liste_bassins_sans_peches <- bassins %>% 
  pull(code_exutoire) %>% 
  setdiff(liste_bassins_mini_une_peche)

# carte des bv non prospectés
carte_bv_non_prospectes <- bassins %>%
  filter(code_exutoire %in% liste_bassins_sans_peches) %>%
  mapview()

save(carte_bv_non_prospectes,
     file = 'processed_data/carte_bv_non_prospectes.RData')

# stations totales
liste_stations_tot <- pt_geo %>% 
  pull(code_coords) %>% 
  as.character

# nb années de pêche par station
liste_stations_mini_une_peche <- pt_data %>%
  group_by(code_station) %>% 
  summarise(n_annees = n_distinct(annee)) %>% 
  ungroup()

# nb stations par bassin
n_stations_par_bassin <- pt_data %>% 
  group_by(code_exutoire) %>% 
  summarise(n_stations_bassin = n_distinct(code_station)) %>% 
  ungroup()

# nb annes par station
n_annees_par_station <- pt_data %>% 
  st_drop_geometry() %>% 
  group_by(code_station) %>% 
  summarise(n_peches = n_distinct(annee)) %>% 
  ungroup()
