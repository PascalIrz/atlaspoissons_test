# library(leaflet)
library(mapview)
library(htmlwidgets)
library(htmltools)
library(tidyverse)

rm(list=ls())

load(file = "../processed_data/fish_and_geographical_data.RData")

###### Fonction de carte présente / absence d'une espèce
# renvoie un message texte qui l'indique si l'espèce n'est présente nulle part sur la zone d'étude 

species_mapping <- function (code_sp) {
  
  # pour les bassins
  
  liste_bassins_presence <- fish_data %>% 
      filter(code_espece == code_sp) %>% 
      group_by(code_espece, code_bassin) %>% 
          summarise(n = sum(effectif, na.rm = T)) %>% 
      ungroup() %>% 
      filter(n > 0) %>%
      pull(code_bassin)
  
  
  if(length(liste_bassins_presence) == 0)
    
    { print(paste0(code_sp, " : Espèce absente du périmètre de l'étude")) }
  
  else 
    
         {
           
           n_stations_du_bassin_ou_sp_presente <- fish_non_spatial %>% 
             filter(code_espece == code_sp & effectif > 0) %>% 
             group_by(code_bassin) %>% 
                summarise(n_stations_presence = n_distinct(code_station)) %>% 
             ungroup()
           
           n_indiv_sp_dans_bassin <- fish_non_spatial %>% 
             filter(code_espece == code_sp) %>% 
             group_by(code_bassin) %>% 
                summarise(n_indiv_sp = sum(effectif, na.rm = TRUE)) %>% 
             ungroup()
           
           bassins_simp2 <- bassins_simp %>%    
             left_join(y = n_indiv_par_bassin) %>% 
             left_join(y = n_indiv_sp_dans_bassin)  %>% 
             left_join(y = n_stations_par_bassin) %>% 
             left_join(y = n_stations_du_bassin_ou_sp_presente) %>% 
             mutate(pourcent_stations_presence_mini_une_fois = n_stations_presence / n_stations_bassin,
                    pourcent_indiv_captures = n_indiv_sp / n_tot_indiv_captures,
                    n_stations_presence = ifelse(n_tot_indiv_captures > 0 & is.na(n_stations_presence),
                                                 0, n_stations_presence),
                    pourcent_stations_presence_mini_une_fois = ifelse(n_tot_indiv_captures > 0 &
                                                                    is.na(pourcent_stations_presence_mini_une_fois),
                                                 0, pourcent_stations_presence_mini_une_fois),
                     n_indiv_sp = ifelse(n_tot_indiv_captures > 0 & is.na(n_indiv_sp),
                                                 0, n_indiv_sp),
                    presence = ifelse(n_indiv_sp > 0, "Présence", ifelse(n_indiv_sp == 0, "Absence", NA))) %>% 
             mutate_at(vars(starts_with("pourcent")), list(scales::percent)) %>% 
             select(1:8, 10:12, 9)
             
  
    # pour les stations
           
    liste_stations_presence <- fish_data %>% 
      filter(code_espece == code_sp & effectif > 0) %>% 
      group_by(code_espece, code_station) %>% 
          summarise(n = sum(effectif, na.rm = T)) %>% 
      ungroup() %>% 
      pull(code_station)

  
    liste_stations_absence <- setdiff(liste_stations_mini_une_peche, liste_stations_presence)
    

    
    n_peches_sp_presente <- fish_non_spatial %>% 
      filter(code_espece == code_sp) %>% 
      group_by(code_station) %>% 
          summarise(n_peches_sp_presente = n_distinct(date_peche)) %>% 
      ungroup()
    
    n_indiv_captures <- fish_non_spatial %>% 
      group_by(code_station) %>% 
          summarise(n_indiv_captures = sum(effectif, na.rm = TRUE)) %>% 
      ungroup()
    
    n_indiv_captures_sp <- fish_non_spatial %>% 
      filter(code_espece == code_sp & effectif > 0) %>% 
      group_by(code_station) %>% 
          summarise(n_indiv_captures_sp = sum(effectif, na.rm = TRUE)) %>% 
      ungroup()
    
    stations2 <- stations %>% 
      select(-code_bassin, -libelle_bassin) %>% 
      left_join(y = n_peches_par_station) %>% 
      left_join(y = n_peches_sp_presente) %>% 
      left_join(y = n_indiv_captures) %>% 
      left_join(y = n_indiv_captures_sp) %>% 
      mutate(pourcent_indiv_sp = n_indiv_captures_sp / n_indiv_captures,
             pourcent_indiv_sp = ifelse(n_peches > 0 & is.na(pourcent_indiv_sp), 0, pourcent_indiv_sp),
             pourcent_peches_sp_presente = n_peches_sp_presente  / n_peches,
             pourcent_peches_sp_presente = ifelse(n_peches > 0 & is.na(pourcent_peches_sp_presente), 0, pourcent_peches_sp_presente),
             n_peches_sp_presente = ifelse(n_peches > 0 & is.na(n_peches_sp_presente), 0, n_peches_sp_presente),
             n_indiv_captures_sp = ifelse(n_peches > 0 & is.na(n_indiv_captures_sp), 0, n_indiv_captures_sp),
             presence = ifelse(n_indiv_captures_sp > 0, "Présence", ifelse(n_indiv_captures_sp == 0, "Absence", NA))) %>% 
      mutate_at(vars(starts_with("pourcent")), list(scales::percent))
      
    
#    stations_presence <- stations2 %>%
#      filter(code_station %in% liste_stations_presence)
    
#    stations_absence <- stations2 %>%
#      filter(code_station %in% liste_stations_absence)

#    mapview(bassins_presence, col.regions = "green", alpha.regions = 0.2, layer.name = c("Présence")) +
#      mapview(bassins_absence, col.regions = "red", alpha.regions = 0.2, layer.name = c("Absence")) +
#      mapview(bassins_sans_peches, col.regions = "grey", alpha.regions = 0.2, layer.name = c("Pas de données")) +
#      mapview(stations_presence, col.regions = "green", layer.name = c("Présence (station)"),
#              legend = FALSE) +
#      mapview(stations_absence, col.regions = "red", layer.name = c("Absence (station)"),
#              legend = FALSE) 
    
    # cartographie
    
    species_fr <- fish_ref %>% 
      filter(espoi == code_sp) %>% 
      pull(esnom)
    
    popup_sta <- leafpop::popupTable(stations2, zcol = c("localisation", "n_peches", "n_peches_sp_presente",
                                                         "n_indiv_captures", "n_indiv_captures_sp", "pourcent_indiv_sp",          
                                                         "pourcent_peches_sp_presente", "presence"))
    
    mapview(bassins_simp2, zcol="presence", alpha.regions = 0.2, layer.name = species_fr,
            col.regions = c("red", "grey", "green"), popup = popup_sta) +
      mapview(stations2, zcol="presence", col.regions = c("red", "grey", "green"), legend = FALSE)
       
        } 
}

species_mapping(code_sp = "TRF")

save(species_mapping, file = '../processed_data/species_mapping.RData')

unique(fish_data$code_espece)

###############
## table des stats sur les opérations

species_sampling_table <- function(code_sp) {
  
  n_peches_presence <- fish_non_spatial %>% 
    filter(code_espece == code_sp) %>% 
    group_by(code_station, date_peche) %>% 
      tally() %>% 
    ungroup() %>% 
    nrow()
  
  n_peche <- fish_non_spatial %>% 
    group_by(code_station, date_peche) %>% 
    tally() %>% 
    ungroup() %>% 
    nrow()
  
  pc_peches_presente <- signif(100 * n_peches_presence / n_peche, digits = 2) %>% 
    paste0("%")
    
  sampling_table <- c(n_peches_presence, n_peche, pc_peches_presente) %>% 
    as.data.frame() %>% 
    t()
    
  colnames(sampling_table) <-    c("Nombre de pêches où l'espèce est présente", "Nombre total de pêches réalisées",
                             "Pourcentage des pêches où l'espèce est présente")
  
  assign("sampling_table", sampling_table, envir = .GlobalEnv)
  
}

###############
## table des stats sur les stations

species_stations_table <- function(code_sp) {
  
  n_stations_presence <- fish_non_spatial %>% 
    filter(code_espece == code_sp) %>% 
    group_by(code_station) %>% 
    tally() %>% 
    ungroup() %>% 
    nrow()
  
  n_stations <- fish_non_spatial %>% 
    group_by(code_station) %>% 
    tally() %>% 
    ungroup() %>% 
    nrow()
  
  pc_stations_presente <- signif(100 * n_stations_presence / n_stations, digits = 2) %>% 
    paste0("%")
  
  stations_table <- c(n_stations_presence, n_stations, pc_stations_presente) %>% 
    as.data.frame() %>% 
    t()
  
  colnames(stations_table) <-    c("Nombre de stations où l'espèce est présente",
                                   "Nombre total de stations prospectées",
                                   "Pourcentage des stations où l'espèce est présente")
  
  assign("stations_table", stations_table, envir = .GlobalEnv)
  
}

species_stations_table("GAR")
View(stations_table)



###############
## table des stats sur les bassins

species_bassins_table <- function(code_sp) {
  
  n_bassins_presence <- fish_non_spatial %>% 
    filter(code_espece == code_sp) %>% 
    group_by(code_bassin) %>% 
    tally() %>% 
    ungroup() %>% 
    nrow()
  
  n_bassins <- fish_non_spatial %>% 
    group_by(code_bassin) %>% 
    tally() %>% 
    ungroup() %>% 
    nrow()
  
  pc_bassins_presente <- signif(100 * n_bassins_presence / n_bassins, digits = 2) %>% 
    paste0("%")
  
  bassins_table <- c(n_bassins_presence, n_bassins, pc_bassins_presente) %>% 
    as.data.frame() %>% 
    t()
  
  colnames(bassins_table) <-    c("Nombre de bassins où l'espèce est présente",
                                   "Nombre total de bassins prospectés",
                                   "Pourcentage des bassins où l'espèce est présente")
  
  assign("bassins_table", bassins_table, envir = .GlobalEnv)
  
}

species_bassins_table("GAR")
View(bassins_table)

###############
## table des stats sur les individus

species_indiv_table <- function(code_sp) {
  
  n_indiv_captures_sp <- fish_non_spatial %>% 
    filter(code_espece == code_sp) %>% 
    summarise(n_sp = sum(effectif, na.rm = TRUE))
  
  n_indiv_captures_tot <- fish_non_spatial %>% 
    summarise(n_sp = sum(effectif, na.rm = TRUE))
  
  pc_des_indiv_appt_a_sp <- signif(100 * n_indiv_captures_sp / n_indiv_captures_tot, digits = 2) %>% 
    paste0("%")
  
  indiv_table <- c(n_indiv_captures_sp, n_indiv_captures_tot, pc_des_indiv_appt_a_sp) %>% 
    as.data.frame() 
  
  colnames(indiv_table) <-    c("Nombre d'individus de l'espèce capturés",
                                  "Nombre total d'individus capturés",
                                  "Pourcentage des individus appartenant à cette espèce")
  
  assign("indiv_table", indiv_table, envir = .GlobalEnv)
  
}


species_indiv_table("TRF")
View(indiv_table)

###############
## graphique comparaison présence / absence

species_indiv_table <- function(code_sp, variable_env) {
  
  occur_sp_par_bassin <- fish_non_spatial %>% 
    filter(code_espece == code_sp) %>% 
    group_by(code_bassin) %>% 
      tally() %>% 
    ungroup() %>% 
    mutate(presence = ifelse(n > 0, "Présence", "Absence"))
  
  occurence_graph <- ggplot(data = occur_sp_par_bassin, aes(x = presence, y = get(variable_env))) +
    geom_boxplot() +
    coord_trans(y = "log10")
  
  assign("occurence_graph", occurence_graph, envir = .GlobalEnv)
  
}



