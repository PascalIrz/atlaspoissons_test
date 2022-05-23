library(tidyverse)
library(sf)
library(atlaspoissonapp)

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

gg_temp2 <- function(data, var_x, var_y) {
  
  ma_formule <- as.formula(quote(var_y) ~ quote(var_x))
  var_x <- enquo(var_x)
  var_y <- enquo(var_y)
  
  g <- ggplot(data, aes(!!var_x, !!var_y)) +
    geom_point() +
    geom_line() +
    coord_cartesian(ylim = c(0,NA))
  
  # Regression linéaire
  
  regression <- lm(formula = ma_formule, data = data)
  
  # summary()
  
  # pvalue <- regression$coefficients[,4] %>%
  #   as.data.frame() %>%
  #   
  #   pvalue <- pvalue[-1,]
  # 
  # # Test
  # if(pvalue > 0.5) {
  #   g <- g +
  #     geom_smooth(se = FALSE, method = "lm")
  # }
  # 
  # g
  
}


# =======================================================
# Graphs

# g <- ggplot(data_etude, aes(annee, abondance)) +
#   geom_point() +
#   geom_line() +
#   coord_cartesian(ylim = c(0,NA))



plot <- gg_temp2(data = data_etude, var_x = annee, var_y = abondance)
# Fonction ne fonctionne pas
# Erreur: "Quosures can only be unquoted within a quasiquotation context"

class(plot)
