library(tidyverse)
library(sf)
library(vegan)
library(factoextra)
library(FactoMineR)

rm(list = ls())

load(file = "processed_data/data.RData")
load(file = "../../atlas_poissons_app/atlas/donnees_appli.RData")

rm(data, ref_espece)

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



rm(pt_data, bv_data, bv_simp_geo, pt_geo)

# =====================================

# Préparation du tableau de données

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
  mutate(log_richesse = log10(richesse + 1))

# carte richesse / surface
ggplot(data = data_me %>%
         filter(richesse > 0),
       aes(x = surf_m2, y = log_richesse)) +
  geom_point() +
  scale_x_log10() +
  geom_smooth(method = "loess", se = FALSE)

# carte richesse / altitude
ggplot(data = data_me, aes(x = alt_moy, y = richesse)) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10() +
  geom_smooth(method = "loess", se = FALSE)

# =============================


bassins_verif <- bassins_no_geom %>%
  select(
    -toponyme,
    -canal_conn,
    -pb,
    -prospecte,
    -x_centroid,
    -y_centroid,
    -strahler_m,
    -pte_tp_moy,
    -starts_with("parc"),
    -prct_rpg
  ) %>%
  mutate(
    long_tp_m = log10(long_tp_m + 1),
    # on log pour que ce soit gaussien
    surf_m2 = log10(surf_m2),
    # on log pour que ce soit gaussien
    prct_PE_CE = asin(sqrt(prct_PE_CE / 100)),
    # on fait un arcsin pour les pourcentages
    prct_PE_CE = ifelse(is.na(prct_PE_CE),
                        yes = 0,
                        no = prct_PE_CE)
  ) %>%
  pivot_longer(cols = surf_m2:prct_PE_CE,
               names_to = "variable",
               values_to = "valeur")

# On vérifie que les variables suivent une distribution gaussienne
ggplot(data = bassins_verif, aes(x = valeur)) +
  geom_histogram() +
  facet_wrap( ~ variable, scales = "free")

# On construit l'ACP

bassins_acp <- bassins_verif %>%
  pivot_wider(names_from = "variable",
              values_from = "valeur",
              values_fill = 0) %>%
  column_to_rownames(var = "code_exutoire")

res <- PCA(bassins_acp)

# ====================================

data_modele <- bassins_acp %>%
  rownames_to_column(var = "code_exutoire") %>%
  left_join(indices)

m <-
  lm(formula = richesse ~ pente_medi + alt_median + surf_m2 + prct_PE_CE,
     data = data_modele)
summary(m)
plot(m)
