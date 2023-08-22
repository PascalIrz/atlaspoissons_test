library(tidyverse)
library(sf)
library(atlaspoissonapp)

rm(list=ls())

load(file = "../../atlas_poissons_app/atlas/donnees_appli.RData")

mon_espece = "Gambusie"


# =======================================================
# Calculs

data_etude <- pt_data %>%
  filter(esp_nom_commun == mon_espece,
         statut == "Présent") %>% 
  group_by(annee, statut) %>%
  summarise(abondance = sum(effectif),
            n_presences = n())

# =================================================

gg_temp2 <- function(data, var_x, var_y) {

  var_x <- enquo(var_x)
  var_y <- enquo(var_y)
  
  g <- ggplot(data, aes(!!var_x, !!var_y)) +
    geom_point() +
    geom_line() +
    coord_cartesian(ylim = c(0,NA))

  # Regression linéaire
  y <- data %>% 
    pull(!!var_y)
  
  x <- data %>% 
    pull(!!var_x)
  
  regression <- lm(y ~ x) %>% 
    summary()
  
  pvalue <- regression$coefficients[2, 4]

  # Test
  if(pvalue < 0.05) {
    g <- g +
      geom_smooth(se = FALSE, method = "lm")
  }

  g
  
}

gg_temp2(data = data_etude,
                 var_x = annee,
                 var_y = abondance)


# =======================================================
# Graphs

# g <- ggplot(data_etude, aes(annee, abondance)) +
#   geom_point() +
#   geom_line() +
#   coord_cartesian(ylim = c(0,NA))




# Fonction ne fonctionne pas
# Erreur: "Quosures can only be unquoted within a quasiquotation context"

class(plot)
