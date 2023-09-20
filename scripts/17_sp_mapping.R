load(file = "processed_data/fish_and_geographical_data.RData")

# =============================================================================
#                         CARTE POUR LES POINTS                                
# =============================================================================
datapt <- donner_statut_sp_point(data)

datapt <- datapt %>% 
  left_join(data) %>% 
  st_sf

cartept <- datapt %>% 
  sample_n(500) %>% 
  mapview(zcol = "presence",
          cex = "effectif",
          col.regions = list("#ff0055", "#b3e93e"))

cartept

# =============================================================================
#                         CARTE POUR LES BV                                    
# =============================================================================

databv <- donner_statut_sp_bv(data)

databv <- databv %>% 
  left_join(bassins_simp) %>%
  st_sf

cartebv <- databv %>% 
  filter(code_espece == "ABL") %>% 
  mapview(zcol = "statut",
          alpha.regions = 0.3)

cartebv

# =============================================================================
#                         CARTE POUR LES DEUX                                  
# =============================================================================

mapview(bassins_simp, col.regions = "#8fd175") +
  mapview(datapt, zcol="presence",
          cex = "effectif",
          col.regions = list("#ff0055", "#b3e93e"))

map <- mapview(databv, zcol="statut", alpha.region = 0.3) +
  mapview(datapt, zcol="presence", cex = "effectif")

