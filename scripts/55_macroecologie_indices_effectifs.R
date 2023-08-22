library(tidyverse)
library(sf)
library(ggplot2)
library(vegan)

rm(list = ls())

load(file = "processed_data/data.RData")
load(file = "../../atlas_poissons_app/atlas/donnees_appli.RData")

# ==============================================================================

bassins_geom <- bassins %>%
  select(code_exutoire, geometry)

test <- pt_data %>% 
  filter(type_peche != c("WAMA", "Atlas")) %>% 
  select(code_exutoire, code_espece, effectif) %>% 
  group_by(code_exutoire, code_espece) %>% 
  summarise(effectif = sum(effectif)) %>%
  ungroup() %>% 
  pivot_wider(names_from = "code_espece",
              values_from = "effectif")

mat_eff <- test %>% 
  column_to_rownames(var = "code_exutoire")

test_indice <- mat_eff %>% 
  transmute(richesse = specnumber(.),
            shannon = diversity(.),
            simpson = diversity(., index = "simpson"),
            pielou = shannon / log(richesse)) %>%
  rownames_to_column(var = "code_exutoire")

centroid <- st_centroid(bassins) %>%
  st_coordinates() %>%
  as.data.frame %>%
  set_names(c("x_centroid", "y_centroid"))

bassins_no_geom <- bassins %>%
  st_drop_geometry() %>%
  cbind(centroid) %>%
  select(-X_exutoire,-X_centroid) %>%
  filter(code_exutoire %in% test$code_exutoire)

# jointure
data_me <- test_indice %>%
  left_join(y = bassins_no_geom) %>%
  mutate(log_richesse = log10(richesse + 1)) %>% 
  filter(shannon > 0,
         simpson > 0,
         pielou >0)

# Shannon
ggplot(data = data_me,
       aes(x = x_centroid,
           y = shannon)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(x = "Longitude", y = "Indice de Shannon")

ggplot(data = data_me,
       aes(x = y_centroid,
           y = shannon)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(x = "Latitude", y = "Indice de Shannon")

model_shannon <- lm(shannon ~ x_centroid + y_centroid, data_me)
summary(model_shannon)

# Simpson
ggplot(data = data_me,
       aes(x = x_centroid,
           y = simpson)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(x = "Longitude", y = "Indice de Simpson")

ggplot(data = data_me,
       aes(x = y_centroid,
           y = simpson)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(x = "Latitude", y = "Indice de Simpson")

model_simpson <- lm(simpson ~ x_centroid + y_centroid, data_me)
summary(model_simpson)

# Piélou
ggplot(data = data_me,
       aes(x = x_centroid,
           y = pielou)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(x = "Longitude", y = "Indice de Piélou")

ggplot(data = data_me,
       aes(x = y_centroid,
           y = pielou)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(x = "Latitude", y = "Indice de Piélou")

model_pielou <- lm(pielou ~ x_centroid + y_centroid, data_me)
summary(model_pielou)
