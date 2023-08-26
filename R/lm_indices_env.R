#' Modélisation des indices de bioviversité en fonction des variables d'environnement
#'
#' @param df Dataframe contenant les données.
#' @param variables_dependantes Vecteur caractères contenant les noms des variables à modéliser.
#'
#' @return Un dataframe contenant les coefficients, leur significativité et le r2 ajusté pour chaque modèle.
#' @export
#'
#' @examples
lm_indices_env <- function(df, variables_dependantes) {
  
  #### fonction pour une variable dépendante
  my_lm <- function(df, variable_dependante = "richesse") {
    # formulation du modele
    my_formula <- formula(
      paste(
        variable_dependante,
        "~",
        "taille +",
        "pente +",
        "temp_01 +",
        "temp_07 +",
        "x_wgs84 +",
        "y_wgs84"
        
      )
    )
    
    # exécution
    mod <- lm(formula = my_formula,
              data = df)
    
    # collecte des coefficients et de leur significativité, mise en forme
    coefs <- summary(mod)$coefficients[, 1]
    
    etoiles <- summary(mod)$coefficients[, 4]
    
    variables <- names(etoiles)
    
    etoiles <- case_when(
      etoiles < 0.1 & etoiles > 0.05 ~ ".",
      etoiles < 0.05 & etoiles > 0.01 ~ "*",
      etoiles < 0.01 & etoiles > 0.001 ~ "**",
      etoiles < 0.001 ~ "***",
      TRUE ~ ""
    )
    
    resultat <- paste0(round(coefs, 3),
                       etoiles)
    
    names(resultat) <- variables
    
    # ajout du r2
    r2 <- summary(mod)$adj.r.squared %>% round(3)
    names(r2) <- "adj.r.squared"
    
    resultat2 <- c(resultat, r2) %>%
      as.data.frame() %>%
      set_names(variable_dependante)
    
  }
  
  # application sur un ensemble de variables dépendantes et assemblage du tableau de résultats
  map(.x = variables_dependantes,
      .f = my_lm,
      df = df) %>%
    reduce(cbind)
  
}
