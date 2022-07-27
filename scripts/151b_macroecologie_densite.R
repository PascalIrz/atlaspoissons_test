library(sf)
library(mapview)
library(aspe)
library(dplyr)
library(RColorBrewer)
library(ggplot2)

rm(list = ls())

load(file = "../../../ASPE/raw_data/tables_sauf_mei_2022_05_30_12_49_01.RData")
load("processed_data/aspe.RData")

rsl <- mef_creer_passerelle() %>% 
  filter(pop_id %in% mes_pops) %>% 
  mef_ajouter_libelle() %>% 
  mef_ajouter_ope_date() %>% 
  mef_ajouter_lots() %>% 
  select(-lop_id,
         -tyl_id,
         -tlo_id,
         -lop_effectif,
         -pre_id) %>% 
  group_by_at(vars(-esp_code_alternatif)) %>% 
    summarise(rsl = n_distinct(esp_code_alternatif)) %>% 
  ungroup()

# pour éviter des biais entre stations selon le nb de prospections, on conserve la dernière pêche

rsl <- rsl %>% 
  group_by(pop_id, pop_libelle) %>%
    filter(ope_date == max(ope_date, na.rm = TRUE)) %>% # dernière date de pêche
    sample_n(1) %>% # qq cas où 2 pêches à la même date sur le même point
  ungroup() %>% 
  mef_ajouter_ope_env() %>% 
  filter(profondeur < 1.5)

# vérification du nb d'opérations par point
# rsl_prov <- rsl %>% 
#   count(pop_id) %>% 
#   arrange(-n)

# transformation des variables
# certaines sont log-transformées
# les zéros sont en fait des valeurs manquantes
# qq valeurs quasi nulles de températures => NA
rsl <- rsl %>% 
  mutate_at(vars(distance_mer:profondeur),
            function(x) {log(1 + x, base = 10)}) %>% 
  mutate_at(vars(distance_mer:profondeur),
            function(x) {ifelse(x == 0, NA, x)}) %>% 
  mutate_at(vars(starts_with("temp_")),
            function(x) {ifelse(x < 1, NA, x)})

rsl %>% 
  pivot_longer(cols = distance_mer:temp_janvier,
               names_to = "variable",
               values_to = "valeur") %>% 
  ggplot(aes(x = valeur)) +
    geom_histogram() +
  facet_wrap(~variable,
             scales = "free") +
  labs(x = "",
       y = "")


rsl %>% 
  select(distance_mer:temp_janvier) %>% 
  cor(use = "pairwise.complete.obs") %>% # pour pb des valeurs manquantes
  corrplot::corrplot.mixed(order = "hclust",
                     upper = 'ellipse',
                     tl.pos = "lt")



mod <- lm(formula = rsl ~
            distance_mer +
            altitude +
            surface_bv +
            distance_source +
            largeur +
            pente +
            profondeur +
            temp_juillet +
            temp_janvier,
          data = rsl)

summary(mod)
mod_simp <- MASS::stepAIC(mod)
summary(mod_simp)



M1 <- rsl %>% 
  select(distance_mer:temp_janvier) %>% 
  cor()

