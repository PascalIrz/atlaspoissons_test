library(tidyverse)
library(sf)

rm(list=ls())

load(file = "../atlas_poissons_app/atlas/donnees_appli.RData")

mon_espece = "Vairon"

# =======================================================
# Calculs

data_etude <- pt_data %>%
  filter(esp_nom_commun == mon_espece,
         statut == "Présent") %>% 
  group_by(annee, statut) %>%
  summarise(abondance = sum(effectif),
            n_presences = n())


# =======================================================
# Graphs

g <- ggplot(data_etude, aes(annee, abondance)) +
  geom_point() +
  geom_line() +
  coord_cartesian(ylim = c(0,NA))



plot <- gg_temp(data = data_etude, var_x = annee, var_y = abondance)
# Fonction ne fonctionne pas
# Erreur: "Quosures can only be unquoted within a quasiquotation context"

