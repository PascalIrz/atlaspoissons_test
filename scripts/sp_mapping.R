# =============================================================================
#                         CARTE POUR LES POINTS                                
# =============================================================================
datapt <- donner_statut_sp_point(data)

datapt <- datapt %>% 
  left_join(data) %>% 
  st_sf

cartept <- datapt %>% 
<<<<<<< HEAD
  sample_n(500) %>% 
  mapview(zcol = "presence",
          cex = "effectif",
          col.regions = list("#ff0055", "#b3e93e"))
=======
  mapview(zcol="presence", cex = "effectif")
>>>>>>> 1190f33872c6146a2d6951bf69c4d9f9f8ec2306

cartept

# =============================================================================
#                         CARTE POUR LES BV                                    
# =============================================================================

databv <- donner_statut_sp_bv(data)

databv <- databv %>% 
  left_join(bassins_simp) %>%
  st_sf

cartebv <- databv %>% 
<<<<<<< HEAD
  filter(code_espece == "ABL") %>% 
  mapview(zcol = "statut",
          alpha.regions = 0.3)
=======
  mapview(zcol="statut")
>>>>>>> 1190f33872c6146a2d6951bf69c4d9f9f8ec2306

cartebv

# =============================================================================
#                         CARTE POUR LES DEUX                                  
# =============================================================================

<<<<<<< HEAD
mapview(bassins_simp, col.regions = "#8fd175") +
  mapview(datapt, zcol="presence",
          cex = "effectif",
          col.regions = list("#ff0055", "#b3e93e"))
=======
map <- mapview(databv, zcol="statut", alpha.region = 0.3) +
  mapview(datapt, zcol="presence", cex = "effectif")
>>>>>>> 1190f33872c6146a2d6951bf69c4d9f9f8ec2306
