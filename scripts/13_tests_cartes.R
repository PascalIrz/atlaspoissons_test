library(tidyverse)
library(sf)
library(mapview)

rm(list = ls())

load(file = "../atlas_poissons_app/atlas/donnees_appli.RData")

# ==============================================================================
# Assemblage des données

#######################
#Pour les bv

bv <- bv_map_data %>% 
  group_by(code_exutoire, code_espece, annee, esp_nom_commun) %>%
  summarise(n_abs = sum(statut == "Absent"),
            n_pres = sum(statut == "Présent"),
            n_n_d = sum(statut == "Non détecté"),
            n_np = sum(statut == "Non prospecté")) %>% 
  ungroup() %>% 
  mutate(statut = case_when(
    n_pres > 0 ~ "Présent",
    n_pres == 0 & n_abs > 0 ~ "Absent",
    TRUE ~ "Non détecté"
  )) 

# Le mutate ne fonctionne donc pas, les bv non prospectés n'apparaissent pas
bv %>% 
  mutate(statut = ifelse(is.na(statut), "Non prospecté", statut),
         statut = as.factor(statut),
         statut = fct_relevel(statut, c("Présent", "Absent", "Non détecté", "Non prospecté" )))

bv_geo <- bv %>% 
  left_join(bv_map_geo) %>% 
  st_sf

#######################
# Pour les pts

pt <- pt_data%>%
  group_by(code_coords, code_exutoire, esp_nom_commun, code_espece, code_station, annee) %>%
  summarise(n_an_abs = sum(statut == "Absent"),
            n_an_pres = sum(statut == "Présent"),
            n_an_n_d = sum(statut == "Non détecté")) %>%
  ungroup() %>%
  mutate(statut = case_when(
    n_an_pres > 0 ~ "Présent",
    n_an_pres == 0 & n_an_abs > 0 ~ "Absent",
    TRUE ~ "Non détecté"
  ))

pt_g <- pt_geo %>%
  left_join(pt)   %>% 
  st_sf


# ==============================================================================
# Filtration des données pour choisir espèces et années

mon_espece <- "Lamproie marine"
premiere_annee <- 2018
derniere_annee <- 2018

pt_g <- pt_g %>% 
  filter(esp_nom_commun == mon_espece,
       annee >= premiere_annee,
       annee <= derniere_annee)

bv_geo <- bv_geo %>%
  filter(esp_nom_commun == mon_espece,
         annee >= premiere_annee,
         annee <= derniere_annee)


# ==============================================================================
# Visualisation de la carte

#La carte fonctionne, cependant les bv non prospectés n'apparaissent pas
mapview(bv_geo,
        zcol = "statut",
        layer.name = mon_espece,
        map.types = c("OpenStreetMap", "Esri.WorldImagery"),
        col.regions = c("red", "pink", "green", "grey40"),
        alpha.regions = 0.5) + 
  mapview(pt_g,
          zcol = "statut",
          col.regions = c("red", "pink", "green"),
          cex = 4,
          legend = FALSE)
