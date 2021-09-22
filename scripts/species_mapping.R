species_mapping <- function (data_sf, code_sp) {
  
  data_df <- data_sf %>% 
    sf::st_drop_geometry()
  
  # pour les bassins
  
  liste_bassins_presence <- data_df %>% 
    filter(code_espece == code_sp) %>% 
    group_by(code_exutoire) %>% 
      summarise(n = sum(effectif, na.rm = T)) %>% 
    ungroup() %>% 
    filter(n > 0) %>%
    pull(code_exutoire)
  

  
  if(length(liste_bassins_presence) == 0)
    
  { print(paste0(code_sp, " : Espèce absente du périmètre de l'étude")) }
  
  else 
    
  {
    
    n_indiv_par_bassin <- data_df %>% 
      group_by(code_exutoire) %>% 
      summarise(n_indiv = sum(effectif, na.rm = TRUE)) %>% 
      ungroup()
    
    n_indiv_sp_par_bassin <- data_df %>% 
      filter(code_espece == code_sp) %>% 
      group_by(code_exutoire) %>% 
      summarise(n_indiv_sp = sum(effectif, na.rm = TRUE)) %>% 
      ungroup()
    
    n_stations_par_bassin <- data_df %>% 
      group_by(code_exutoire) %>% 
        summarise(n_stations = n_distinct(code_station))
    
    n_stations_par_bassin_ou_sp_presente <- data_df %>% 
      filter(code_espece == code_sp & effectif > 0) %>% 
      group_by(code_exutoire) %>% 
      summarise(n_stations_presence = n_distinct(code_station)) %>% 
      ungroup()
    
    bassins_simp2 <- bassins_simp %>%    
      left_join(y = n_indiv_par_bassin) %>% 
      left_join(y = n_indiv_sp_par_bassin)  %>% 
      left_join(y = n_stations_par_bassin) %>% 
      left_join(y = n_stations_par_bassin_ou_sp_presente) %>% 
      mutate(pourcent_stations_presence_mini_une_fois = n_stations_presence / n_stations,
             pourcent_indiv_captures = n_indiv_sp / n_indiv,
             n_stations_presence = ifelse(n_indiv > 0 & is.na(n_stations_presence),
                                          0, n_stations_presence),
             pourcent_stations_presence_mini_une_fois = ifelse(n_indiv > 0 &
                                                                 is.na(pourcent_stations_presence_mini_une_fois),
                                                               0, pourcent_stations_presence_mini_une_fois),
             n_indiv_sp = ifelse(n_indiv > 0 & is.na(n_indiv_sp),
                                 0, n_indiv_sp),
             presence = ifelse(n_indiv_sp > 0, "Présence", ifelse(n_indiv_sp == 0, "Absence", NA))) %>% 
      mutate_at(vars(starts_with("pourcent")), list(scales::percent)) # %>% 
 #     select(1:8, 10:12, 9)
    
    
    # pour les stations
    
    liste_stations_presence <- data_df %>%
      filter(code_espece == code_sp & effectif > 0) %>%
      group_by(code_espece, code_station) %>%
      summarise(n = sum(effectif, na.rm = T)) %>%
      ungroup() %>%
      pull(code_station) %>% 
      as.character()
    
    liste_stations_mini_une_peche <- data_df %>% 
      pull(code_station) %>% 
      unique() %>% 
      as.character()
    # 
    # 
    liste_stations_absence <- setdiff(liste_stations_mini_une_peche, liste_stations_presence)
    
    n_peches_sp_presente <- data_df %>%
      filter(code_espece == code_sp) %>%
      group_by(code_station) %>%
      summarise(n_peches_sp_presente = n_distinct(date_peche)) %>%
      ungroup()

    n_indiv_captures <- data_df %>%
      group_by(code_station) %>%
      summarise(n_indiv_captures = sum(effectif, na.rm = TRUE)) %>%
      ungroup()

    n_indiv_captures_sp <- data_df %>%
      filter(code_espece == code_sp & effectif > 0) %>%
      group_by(code_station) %>%
      summarise(n_indiv_captures_sp = sum(effectif, na.rm = TRUE)) %>%
      ungroup()
    
    stations2 <- stations %>%
      select(-code_exutoire, -libelle_bassin) %>%
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
    
    # species_fr <- fish_ref %>% 
    #   filter(espoi == code_sp) %>% 
    #   pull(esnom)
    # 
    # popup_sta <- leafpop::popupTable(stations2, zcol = c("localisation",
    #                                                      "n_peches",
    #                                                      "n_peches_sp_presente",
    #                                                      "n_indiv_captures",
    #                                                      "n_indiv_captures_sp",
    #                                                      "pourcent_indiv_sp",          
    #                                                      "pourcent_peches_sp_presente",
    #                                                      "presence"))
    
    # mapview(bassins_simp2,
    #         zcol = "presence",
    #         alpha.regions = 0.2,
    #         layer.name = species_fr,
    #         col.regions = c("red", "grey", "green"),
    #         popup = popup_sta) +
    #   mapview(stations2,
    #           zcol = "presence",
    #           col.regions = c("red", "grey", "green"),
    #           legend = FALSE)
    
  } 
}

species_mapping(data_sf = data,
                code_sp = "TRF")
