library(tidyverse)

rm(list = ls())

###############################################################################
###################### Niveau des océans
# données issues de la publi 
# Rachel M. Spratt and Lorraine E. Lisiecki. 2016. A Late Pleistocene sea level stack.
# Climate of the Past, 12, 1079-1092. doi: 10.5194/cp-12-1-2016
# hébergées sur le serveur de la NOAA ou dans le package gsloid

# devtools::install_github("benmarwick/gsloid")
library(gsloid)

#ocean_level <- fread ("https://www1.ncdc.noaa.gov/pub/data/paleo/contributions_by_author/spratt2016/spratt2016.txt", skip=95)
ocean_level <- spratt2016 %>% 
  select(time_my = age_calkaBP, level_m = SeaLev_shortPC1) %>% 
  filter(time_my < 30)

detach("package:gsloid", unload=TRUE)

########### représentation graphique

offset <- ocean_level %>% 
  slice(1) %>% 
  pull(level_m)

ocean_level <- ocean_level %>% 
  mutate(level_m = level_m - offset)

min <- min(ocean_level$level_m)

min_year <- ocean_level %>% 
  filter(level_m == min) %>% 
  pull(time_my)


ggplot(data = ocean_level, aes(x = time_my, y = level_m)) +
  geom_line() +
  geom_hline(yintercept = min, col = 'red', linetype = 2) +
  geom_vline(xintercept = min_year, col = 'red', linetype = 2) + 
  annotate(geom="text", x=10, y=min + 3, label = paste0("Minimum = ", min, " m"), color="red") +
  annotate(geom="text", x=min_year-0.6, y = -75, label = paste0(min_year, " 000 years ", " before present"), color="red",
           angle = 90) +
  labs(x = "Thousend years BP", y = "Sea level in meters, difference with present")


################### fonction réciproque du graphique
# le but est de réussir, pour une hauteur de la mer donnée, de renvoyer la date en interpolant depuis la courbe
# pour la valeur de hauteur, on va l'encadrer par celle juste au-dessous et celle juste au-dessus,
# puis extrapoller linéairement.
# La fonction prend en entrée le niveau en mètres sous le niveau actuel et renvoie, en milliers d'années, la dernière date où
# l'océan était à ce niveau

ocean_level <- ocean_level %>% 
  slice(1:min_year)

level_to_years <- function (level = -100) {
  
  x_inf <- ocean_level %>% 
    filter(level_m < level) %>% 
    slice(1) %>% 
    pull(time_my)
  
  x_sup <- x_inf - 1
  
  y_inf <- ocean_level %>% 
    filter(time_my == x_inf) %>% 
    pull(level_m)
  
  y_sup <- ocean_level %>% 
    filter(time_my == x_sup) %>% 
    pull(level_m)
  
  a <- (y_inf - y_sup) / (x_inf - x_sup)
  b <- y_sup - a * x_sup
  
  year <- (level - b) / a
  
  print(year)
  
}

# test
# map_dbl(.x = 0:-100, .f = level_to_years)

rm(offset, min_year, min)

save(ocean_level, level_to_years, file = "processed_data/ocean_level.RData")

