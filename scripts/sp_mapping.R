# =============================================================================
#                         CARTE POUR LES POINTS                                
# =============================================================================
datapt <- donner_statut_sp_point(data)

datapt <- datapt %>% 
  left_join(data) %>% 
  st_sf

cartept <- datapt %>% 
  sample_n(500) %>% 
  mapview(zcol="presence", cex = "effectif", col.regions = list("#ff0055", "#b3e93e"))

cartept

# =============================================================================
#                         CARTE POUR LES BV                                    
# =============================================================================

databv <- donner_statut_sp_bv(data)

databv <- databv %>% 
  left_join(bassins_simp) %>% 
  st_sf

cartebv <- databv %>% 
  sample_n(500, replace = TRUE) %>% 
  mapview(zcol="statut", col.regions = list("#d18975", "#d1ab75", "#8fd175"), alpha.regions = 0.3)

cartebv
mapview(bassins_simp)
# ça n'affiche rien, juste la légende

# =============================================================================
#                         CARTE POUR LES DEUX                                  
# =============================================================================

mapview(bassins_simp, col.regions = "#8fd175") +
  mapview(datapt, zcol="presence", cex = "effectif", col.regions = list("#ff0055", "#b3e93e"))
