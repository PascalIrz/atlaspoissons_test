#' Calcule les courbes d'accumulation par groupe d'échantillons
#' 
#' Par exemple par point de prélèvement.
#'
#' @param df Dataframe avec une colonne par espèce, une colonne qui donne l'appartenance
#'     au groupe, et une ligne par observation
#' @param var_site_id Variable qui indique à quel groupe appartient l'observation
#'
#' @return Le dataframe avec pour chaque combinaison groupe x nb_obs, la richesse
#' @export
#'
#' @examples
mon_accum <- function(df, var_site_id)
  
{
  
  mon_accum_1_site <- function(df, var_site_id, site_id) {
    
    df <- df %>% filter( {{var_site_id}} == site_id) %>% 
      select(-{{var_site_id}})
    
    accum <- specaccum(df)
    
    data.frame(pop_id = site_id,
               n_opes = accum$sites,
               pt_richesse = accum$richness)
  }
  
  out <- map_df(.x = unique(df %>% pull( {{ var_site_id }} )),
                .f = mon_accum_1_site,
                df = df,
                var_site_id = {{var_site_id}})
  
  out
  
}