rm(list=ls())

# lecture des fichiers de sortie des modèles effectifs
# issu slt de aspe => pratique pour joindre aux autres données
pt_evol <- data.table::fread("raw_data/sorties_modeles_cefe/Taux_croissance_Effectifs.txt") %>% 
  rename(code_espece = TA_CODE,
         pop_id = ope_pop_id)

# points avec des tendances
pops <- pt_evol %>% 
  pull(pop_id) %>% 
  unique()


# données aspe
load(file = "../../../ASPE/raw_data/tables_sauf_mei_2022_05_30_12_49_01.RData")

# correspondance entre pop_id et sta_id
passerelle <- mef_creer_passerelle() %>% 
  filter(pop_id %in% pops) %>% 
  select(pop_id,
         sta_id) %>% 
  distinct() %>% 
  rename(code_station = sta_id) %>% 
  mutate(source_donnee = "Aspe",
         code_station = as.character(code_station)) %>% 
  filter(!is.na(code_station)) # qq cas isolés en dehors de la région bzh

gdata::keep(pt_evol,
            pops,
            passerelle,
            sure = TRUE)

# données préparées pour l'appli
load(file = "../../atlas_poissons_app/atlas/donnees_appli.RData")

passerelle2 <- pt_data %>% 
  mutate(code_station = as.character(code_station)) %>% 
  left_join(y = passerelle) %>% 
  select(
    #code_station,
         code_coords,
         pop_id) %>% 
  distinct() %>% 
  filter(pop_id %in% pops)




pt_evol2 <- pt_evol %>% 
  left_join(y = passerelle2) %>% 
  filter(!is.na(code_coords)) %>% 
  mutate(Tendance = case_when(
    Tendance == "H" ~ "Hausse",
    Tendance == "Sh" ~ "Stable / hausse",
    Tendance == "S" ~ "Stable",
    Tendance == "Sb" ~ "Stable / baisse",
    Tendance == "B" ~ "Baisse"
  ),
  Tendance = as.factor(Tendance),
  Tendance = ordered(Tendance, levels = c("Hausse",
                                          "Stable / hausse",
                                          "Stable",
                                          "Stable / baisse",
                                          "Baisse")),
  R = round(R, digits = 3)) %>% 
  select(code_coords,
         code_espece,
         pop_id,
         `Nombre de taux` = nb_R,
         `Taux moyen` = R,
         Tendance)



pt_geo %>% 
  left_join(pt_evol2) %>% 
  filter(code_espece == "ANG") %>% 
  select(-code_coords,
         -code_espece,
         -pop_id) %>% 
  rename(Localisation = localisation) %>% 
  mapview::mapview(zcol = "Tendance",
                   col.regions = c("darkblue", "lightblue", "white", "pink", "red"),
                   layer.name = "ANG",
                   feature.id = FALSE)




save(pt_evol,
     file = "../../atlas_poissons_app/atlas/sorties_modeles_cefe.RData")

evol_geo <- pt_evol %>% 
  rename(pop_id = ope_pop_id) %>% 
  left_join(y = point_prelevement %>% 
              select(pop_id, ))
