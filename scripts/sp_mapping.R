# =============================================================================
#                         CARTE POUR LES POINTS                                
# =============================================================================
datapt <- donner_statut_sp_point(data)

datapt <- datapt %>% 
  left_join(data) %>% 
  st_sf

cartept <- datapt %>% 
  sample_n(500) %>% 
  mapview(zcol="presence", cex = "effectif", col.regions = list("green","red"))

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
  mapview(zcol="statut", col.regions = list("red","orange", "green","grey"), alpha.regions = 0.3)

cartebv
# ça n'affiche rien, juste la légende

# =============================================================================
#                         CARTE POUR LES DEUX                                  
# =============================================================================

data3_sample <- data3 %>% 
  sample_n(500, )

mapview(datapt, zcol="presence", cex = "effectif", col.regions = list("green", "red")) +
  mapview(databv, ...)