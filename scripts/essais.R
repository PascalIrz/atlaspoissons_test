Couches pour diags territoriaux sur "Z:\dr35_IG_metier\CARTOTHEQUE\Enjeux_Eau_Biodiversite\SD"
library(tidyverse)
library(sf)

x <- 304660
y <- 2353600

dr_ofb <- sf::st_sfc(sf::st_point(c(x,y)))
sf::st_crs(dr_ofb) <- 27572
dr_ofb

get_coords(dr_ofb, crs_init = 27572)

get_coords <- function(sf_obj, crs_init, crs_fin = 4326, col_names = c("x_wgs84", "y_wgs84")) {
  sf_obj %>%
    `st_crs<-`(crs_init) %>%
    st_transform(crs = crs_fin) %>%
    st_coordinates() %>%
    as.data.frame() %>%
    magrittr::set_colnames(col_names)
}

get_coords(dr_ofb, crs_init = 27572)
