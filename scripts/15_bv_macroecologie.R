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
  mutate(log_richesse = log10(richesse + 1))

# graphique richesse / surface
ggplot(data = data_me %>%
         filter(richesse > 0),
       aes(x = surf_m2, y = log_richesse)) +
  geom_point() +
  scale_x_log10() +
  geom_smooth(method = "loess", se = FALSE)

# graphique richesse / altitude
ggplot(data = data_me, aes(x = alt_moy, y = richesse)) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10() +
  geom_smooth(method = "loess", se = FALSE)

# ==============================================================================

# On vérifie que les variables env suivent un modèle gaussien
bassins_verif <- bassins_no_geom %>%
  select(
    -toponyme,
    -canal_conn,
    -pb,
    -prospecte,
    -strahler_m,
    -pte_tp_moy,
    -starts_with("parc"),
    -prct_rpg
  ) %>%
  filter(!code_exutoire %in% c("exut_303", "exut_212", "exut_665", "exut_3627")) %>%
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

# ==============================================================================
# regression linéaire
# ==============================================================================

data_modele <- bassins_acp %>%
  rownames_to_column(var = "code_exutoire") %>% 
  left_join(indices) %>%
  mutate(code_exut = code_exutoire) %>% 
  column_to_rownames(var = "code_exut")

m <-
  lm(formula = richesse ~ pente_medi + alt_median + surf_m2 + prct_PE_CE + x_centroid + y_centroid,
     data = data_modele)
summary(m)
plot(m)

rm(bassins, bassins_verif, centroid, bassins_acp, matrice_presence, data, res)
# Il nous reste maintenant: 
# bassins_geom = codes exut + géométrie
# bassins_non_geom = informations sur les bassins 
# data_me = indices + informations
# data_modele = data pour le lm
# m = modèle lm
# presence = liste de présence/absence des espèces

plot(m$residuals)
summary(m$residuals)

carte_residuals <- bassins_geom %>% 
  filter(!code_exutoire %in% c("exut_303", "exut_212", "exut_665", "exut_3627")) %>% 
  filter(code_exutoire %in% data_me$code_exutoire) %>% 
  cbind(m$residuals)

mapview(carte_residuals, 
        zcol = "m.residuals",
        col.regions = RColorBrewer::brewer.pal(10, "PRGn"))

# ==============================================================================
# Richesse spécifique régionale et locale
# ==============================================================================

inventaire <- c("Atlas", 
                "WAMA", 
                "Complète", 
                "Stratifiée par Points (grand milieu)",
                "Pêche partielle par points (grand milieu)",
                "Pêche complète à un ou plusieurs passages", 
                " Pêche par ambiances",
                "partiel",
                "complet")

nb_inv <- pt_data %>% 
  filter(type_peche %in% inventaire) %>% 
  group_by(code_exutoire) %>% 
  summarise(n_inventaire = n_distinct(code_coords)) %>% 
  filter(n_inventaire > 4)

bv_test_macro <- nb_inv %>% 
  left_join(bv_simp_geo) %>% 
  st_sf()

mapview(bv_test_macro)

richesse <- pt_data %>% 
  filter(code_exutoire %in% nb_inv$code_exutoire,
         type_peche %in% inventaire,
         statut == "Présent") %>%
  group_by(code_coords, code_exutoire) %>% 
  filter(annee == max(annee, na.rm = TRUE)) %>% # On prend seulement la dernière annee pour les stations échantillonnées plusieurs fois
  ungroup()

richesse_loc <- richesse %>%  
  group_by(code_coords, code_exutoire) %>% 
  summarise(richesse_locale = n_distinct(code_espece)) %>% 
  left_join(pt_data %>% 
              select("code_coords", "x_wgs84", "y_wgs84"))%>% 
  group_by(code_exutoire) %>% 
  summarise(richesse_loc_moy = mean(richesse_locale, na.rm = TRUE))

# Test distribution richesse locale
ggplot(data = richesse_loc,
       aes(x = y_wgs84,
           y = richesse_locale)) +
  geom_point() +
  geom_smooth(method = lm)


richesse_reg <- richesse %>% 
  group_by(code_exutoire) %>% 
  summarise(richesse_regionale = n_distinct(code_espece))

richesse_macro <- richesse_reg %>% 
  left_join(richesse_loc)

ggplot(data = richesse_macro %>% 
         filter(code_exutoire != "exut_303"), 
       aes(x = richesse_regionale, y = richesse_loc_moy)) +
  geom_point() +
  # geom_smooth() +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), # 0 +  pour enlever intercept, ne change rien à la courbe
              size = 1, se = FALSE, colour = "blue") +
  # geom_abline(slope = 1, intercept = 0, col = "red") +
  coord_cartesian(xlim = c(0,NA), ylim = c(0,NA))

# Methode 1 pour enlever intercept
mod <- lm(richesse_loc_moy ~ 0 + richesse_regionale + I(richesse_regionale^2), 
            data = richesse_macro)

# Méthode 2 pour enlever intercept
# mod2 <- lm(richesse_loc_moy ~ richesse_regionale + I(richesse_regionale^2) -1, 
#            data = richesse_macro)

# Quelque soit la méthode utilisée, le résultat est le même avec le summary
summary(mod)

# ==============================================================================
# Prédiction
# ==============================================================================

prediction <- mod$fitted.values %>% 
  as.data.frame()

richesse_prediction <- cbind(richesse_macro, prediction) %>% 
  rename("fitted_values" = ".")

ggplot(richesse_prediction, aes(x = richesse_regionale,
                       y = fitted_values)) +
  geom_point()

