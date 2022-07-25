library(sf)
library(mapview)
library(aspe)
library(dplyr)
library(RColorBrewer)
library(ggplot2)

rm(list=ls())

load(file = "../../../ASPE/raw_data/tables_sauf_mei_2022_05_30_12_49_01.RData")
load("processed_data/aspe.RData")

densite <- mef_creer_passerelle() %>% 
  mef_ajouter_libelle() %>% 
  mef_ajouter_ope_date() %>% 
  mef_ajouter_surf_calc() 

densite2 <- densite %>% 
  group_by(pop_id) %>% 
  filter(pop_id %in% mes_pops,
         annee == max(annee, na.rm = TRUE)) %>% 
  mef_ajouter_lots() %>% 
  group_by(pop_id, ope_id, pop_libelle, ope_surface_calculee, annee) %>% 
  summarise(effectif = sum(lop_effectif, na.rm = T)) %>% 
  ungroup() %>% 
  filter(ope_surface_calculee > 0) %>%  # beaucoup de surfaces calculées manquantes (près de la moitié)
  mutate(densite = effectif / ope_surface_calculee,
         log_densite = log(densite))

coords <- point_prelevement %>% 
  filter(pop_id %in% mes_pops) %>% 
  left_join(ref_type_projection,
            by = c("pop_typ_id" = "typ_id")) %>% 
  aspe::geo_convertir_coords_df(var_x = pop_coordonnees_x,
                                var_y = pop_coordonnees_y,
                                var_id = pop_id,
                                var_crs_initial = typ_code_epsg,
                              crs_sortie = 4326)

# Densité au point
densite_pt <- densite2 %>% 
  left_join(coords) %>%  #pour la carte ajouter ce qui suit
  st_as_sf(coords = c("X", "Y"),
      crs = 4326)

ggplot(data = densite_pt,
       aes(x = X,
           y = log_densite)) + 
  geom_point() +
  geom_smooth(method = lm) +
  labs(x = "Latitude", y = "Densité (log)")

modele_densite <- lm(log_densite ~ X + Y, densite_pt)
summary(modele_densite)

# Densité au bassin

load("processed_data/bassins.RData")

# bassins <- bassins %>%
#    st_drop_geometry()

densite_bv <- densite_pt %>%
  sf::st_join(bassins %>%
                select(code_exutoire, X_centroid)) %>% 
  st_drop_geometry %>% 
  left_join(bassins %>% 
              select(code_exutoire, geometry,surf_m2)) %>% 
  group_by(code_exutoire) %>% 
  mutate(effectif = sum(effectif),
         densite_bassin = effectif/surf_m2,
         log_densite_bassin = log(densite_bassin),
         log_surf = log(surf_m2)) %>% 
  filter(annee == max(annee, na.rm = TRUE)) %>% 
  select(-geometry)
# %>% # Pour carte ajouter ce qui suit
  # st_as_sf() 

ggplot(data = densite_bv,
       aes(x = log_surf,
           y = log_densite_bassin)) + 
  geom_point() +
  geom_smooth(method = lm) +
  labs(x = "Log de la surface du bassin versant (m²)", y = "Densité (log)")

modele_densite_bv <- lm(log_densite_bassin ~ log_surf, 
                        data = densite_bv)
summary(modele_densite_bv)

# Carte pour visualiser
mapview(densite_pt,
        zcol = "log_densite",
        cex = 4,
        col.regions = brewer.pal(1117, "PuOr")) +
  mapview(densite_bv,
          zcol = "log_densite_bassin",
          col.regions = brewer.pal(1117, "PuOr"))

