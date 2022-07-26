library(tidyverse)
library(sf)
library(vegan)
library(factoextra)
library(FactoMineR)
library(mapview)

rm(list = ls())

load(file = "processed_data/data.RData")
load(file = "../../atlas_poissons_app/atlas/donnees_appli.RData")

# ==============================================================================

rm(data, ref_espece)

bassins_geom <- bassins %>%
  select(code_exutoire, geometry)

data <- pt_data %>%
  select(code_exutoire, code_espece, effectif) %>%
  group_by(code_exutoire, code_espece) %>%
  summarise(eff = sum(effectif)) %>%
  ungroup() %>%
  mutate(eff = ifelse(eff > 0,
                      yes = 1,
                      no = 0))

presence <- data %>%
  pivot_wider(names_from = "code_espece",
              values_from = "eff")



# rm(pt_data, bv_data, bv_simp_geo, pt_geo)

# ==============================================================================
# Préparation du tableau de données
# ==============================================================================

# Matrice des presences

matrice_presence <- presence %>%
  column_to_rownames(var = "code_exutoire")

# Calcul des indices

indices <- matrice_presence %>%
  transmute(
    richesse = specnumber(.),
    shannon = diversity(.),
    simpson = diversity(., index = "simpson"),
    pielou = shannon / log(richesse)
  ) %>%
  rownames_to_column(var = "code_exutoire")

# verification

# verif <- data %>%
#   select(-code_espece) %>%
#   group_by(code_exutoire) %>%
#   summarise(abondance = sum(eff))

# création objet bassins
centroid <- st_centroid(bassins) %>%
  st_coordinates() %>%
  as.data.frame %>%
  set_names(c("x_centroid", "y_centroid"))

bassins_no_geom <- bassins %>%
  st_drop_geometry() %>%
  cbind(centroid) %>%
  select(-X_exutoire,-X_centroid) %>%
  filter(code_exutoire %in% data$code_exutoire)

# jointure
data_me <- indices %>%
  left_join(y = bassins_no_geom) %>%
  mutate(log_richesse = log10(richesse + 1)) %>% 
  filter(shannon > 0,
         simpson > 0)

# Etude indices
# richesse
model_richesse <- lm(richesse ~ x_centroid + y_centroid + log(surf_m2) +
                       alt_max +
                       pente_medi + canal_conn,
                     data = data_me)

cook <- cooks.distance(model_richesse) %>% 
  as.data.frame() %>% 
  set_names("cook") %>% 
  mutate(filtre = (cook < 4 / nrow(data_me)))

data_me <- data_me %>% 
  cbind(cook)

data_me_filtre <- data_me %>% 
  filter(filtre)

model_richesse <- lm(richesse ~ x_centroid + y_centroid + log(surf_m2) +
                       alt_max +
                       pente_medi# + canal_conn
                     ,
                     data = data_me_filtre)
summary(model_richesse)

res <- model_richesse$residuals
fitted <- model_richesse$fitted.values


