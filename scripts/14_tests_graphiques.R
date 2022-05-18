rm(list=ls())

load(file = "../atlas_poissons_app/atlas/donnees_appli.RData")

mon_espece = "Truite de rivière"

occurence <- pt_data %>% 
  filter(esp_nom_commun == mon_espece) %>% 
  select(statut, annee)

occ1 <- occurence %>% 
  filter(annee == 2011)
occ1 <- sum(occ1 == "Présent")

occ2 <- occurence %>% 
  filter(annee == 2012)
occ2 <- sum(occ2 == "Présent")

occ3 <- occurence %>% 
  filter(annee == 2013)
occ3 <- sum(occ3 == "Présent")

occ4 <- occurence %>% 
  filter(annee == 2014)
occ4 <- sum(occ4 == "Présent")

occ5 <- occurence %>% 
  filter(annee == 2015)
occ5 <- sum(occ5 == "Présent")

occ6 <- occurence %>% 
  filter(annee == 2016)
occ6 <- sum(occ6 == "Présent")

occ7 <- occurence %>% 
  filter(annee == 2017)
occ7 <- sum(occ7 == "Présent")

occ8 <- occurence %>% 
  filter(annee == 2018)
occ8 <- sum(occ8 == "Présent")

occ9 <- occurence %>% 
  filter(annee == 2019)
occ9 <- sum(occ9 == "Présent")

occ10 <- occurence %>% 
  filter(annee == 2020)
occ10 <- sum(occ10 == "Présent")

occ11 <- occurence %>% 
  filter(annee == 2021)
occ11 <- sum(occ11 == "Présent")

annees <- pt_data %>%
  group_by(annee) %>%
  pull(annee) %>%
  unique() %>%
  sort()

occ <- c(occ1, occ2, occ3, occ4, occ5, occ6, occ7, occ8, occ9, occ10, occ11)

occurences_sp <- data.frame(annees,occ)

ggplot(occurences_sp, aes(annees, occ)) +
  geom_point() +
  geom_line()
