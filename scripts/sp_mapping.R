# =============================================================================
#                         CARTE POUR LES POINTS                                
# =============================================================================
datapt <- donner_statut_sp_point(data)

datapt_ABH <- datapt %>% 
  left_join(data) %>% 
  filter(code_espece == "ABH") %>%
  st_sf

cartept_ABH <- datapt_ABH %>% 
  mapview(zcol="presence", cex = "effectif")

cartept_ABH

# =============================================================================
#                         CARTE POUR LES BV                                    
# =============================================================================

databv <- donner_statut_sp_bv(data)

databv_ABH <- databv %>% 
  left_join(bassins_simp) %>%
  filter(code_espece == "ABH") %>% 
  st_sf

cartebv_ABH <- databv_ABH %>% 
  mapview(zcol="statut")

cartebv

# =============================================================================
#                         CARTE POUR LES DEUX                                  
# =============================================================================

mapview(databv_ABH, zcol="statut", alpha.region = 0.3) +
  mapview(datapt_ABH, zcol="presence", cex = "effectif")
