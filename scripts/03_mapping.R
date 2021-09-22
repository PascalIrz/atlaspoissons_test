# library(leaflet)
library(mapview)
library(htmlwidgets)
library(htmltools)
library(tidyverse)

rm(list=ls())

load(file = "processed_data/fish_and_geographical_data.RData")

###### Fonction de carte présente / absence d'une espèce
# renvoie un message texte qui l'indique si l'espèce n'est présente nulle part sur la zone d'étude 



save(species_mapping, file = '../processed_data/species_mapping.RData')

unique(data$code_espece)

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



