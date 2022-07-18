library(ggplot2)
library(sf)
library(dplyr)

rm(list=ls())

load(file = "processed_data/data.RData")
load(file = "../../atlas_poissons_app/atlas/donnees_appli.RData")

bassins_no_geom <- bassins %>% 
  st_drop_geometry() %>% 
  filter(!is.na(strahler_m)) %>% 
  select(code_exutoire,surf_m2, X_exutoire, strahler_m, alt_max)

ggplot(data = bassins_no_geom,
       aes(x = X_centroid,
           y = log(surf_m2))) +
  geom_point()

ggplot(data = bassins_no_geom,
       aes(x = strahler_m,
           y = alt_max)) +
  geom_point()
# Pas pertinent, strahler max par bassin (petits bassins seulement du 1)

chisq.test(bassins$X_centroid, bassins$surf_m2) # H0 pas rejetée
chisq.test(bassins$strahler_m, bassins$alt_max) # H0 pas rejetée
