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
