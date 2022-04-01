# =============================================================================
#                         CARTE POUR LES POINTS                                
# =============================================================================
datapt <- donner_statut_sp_point(data)

datapt <- datapt %>% 
  left_join(data) %>% 
  st_sf

cartept <- datapt %>% 
  mapview(zcol="presence", cex = "effectif")

cartept

# =============================================================================
#                         CARTE POUR LES BV                                    
# =============================================================================

databv <- donner_statut_sp_bv(data)

databv <- databv %>% 
  left_join(bassins_simp) %>%
  st_sf

cartebv <- databv %>% 
  mapview(zcol="statut")

cartebv

# =============================================================================
#                         CARTE POUR LES DEUX                                  
# =============================================================================

map <- mapview(databv, zcol="statut", alpha.region = 0.3) +
  mapview(datapt, zcol="presence", cex = "effectif")
