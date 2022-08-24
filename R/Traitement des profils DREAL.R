# Chargement des packages
library(ade4)
library(vegan)
library(factoextra)
library(corrplot)
library(RVAideMemoire)
require(tidyverse)
require(InraeThemes)
require(readxl)
require(cooccur)
library(lubridate)
library(ggrepel)
library(devtools)
library(patchwork)
library(sf)
library(leaflet)
library(packcircles)
library(viridis)
library(htmlwidgets)
library(oceanis)
library(png)
library(leafpop)
library(RMySQL)
library(RPostgreSQL)
library(RODBC)
library(tmap)

# Chargement des données 
load("data/Donnees_utilisables/Donnees_completes.RData")

# Leaflet maps method 1 (Not the one we will keep) --------------------------------------------------

# Taxons à étudier
New_taxons <- as.tibble(read_excel("data/Donnees_utilisables/Fichier_DREAL_1.xlsx", 
                                   sheet = "Non_contributifs")) %>% 
  select(abre)

# On réalise un plot avec l'évolution du nombre de comptage pour chaque taxons
# entre 2007 et 2020

# Récupérer l'occurence de chaque taxon par année

# Fonction qui permettra de plotter plus facilement tout les resultats
Evol_tax <- function(esp,dt){
  
  options(repr.plot.width = 5, repr.plot.height =2)
  
  ggplot(dt, aes(x = as.factor(DATE), y = count, group = 1)) + 
    geom_point(size = 0.5) +
    geom_line(size = 0.5)+
    labs(title = esp,
         x = "Année",
         y = "Occurence")+
    theme_inrae()
  
  ggsave(file=paste("img/Occur_taxons/plot",esp, ".jpeg"))
}



Occur_tax_year <- Flore_checked %>% distinct() %>% 
  group_by(DATE = year(DATE),code_taxon) %>% summarize(count = n()) %>% ungroup() %>% 
  filter(code_taxon %in% unique(New_taxons %>% pull(abre)))


Evol_plots_tax <- Occur_tax_year %>%
  group_nest(code_taxon) %>%
  mutate(plot = map2(code_taxon, data, Evol_tax))

# # Sauvegarde du plot sous R
# png(paste("img/Occur_taxons/Global", ".jpeg", sep=""),width = 10000 ,
#     height = 15000,  res = 400)
# 
# 
# (Evol_plots_tax$plot[[1]] + Evol_plots_tax$plot[[2]] + Evol_plots_tax$plot[[3]]) / (Evol_plots_tax$plot[[4]] + 
#                                                                                       Evol_plots_tax$plot[[5]] + Evol_plots_tax$plot[[6]]) / (Evol_plots_tax$plot[[7]] +  Evol_plots_tax$plot[[8]] + 
#                                                                                                                                                 Evol_plots_tax$plot[[9]]) / (Evol_plots_tax$plot[[10]] +  Evol_plots_tax$plot[[11]] +  Evol_plots_tax$plot[[12]]) / 
#   (Evol_plots_tax$plot[[13]] +  Evol_plots_tax$plot[[14]] +  Evol_plots_tax$plot[[15]]) /   (Evol_plots_tax$plot[[16]] + 
#                                                                                                Evol_plots_tax$plot[[17]] +  Evol_plots_tax$plot[[18]]) / ( Evol_plots_tax$plot[[19]] +  Evol_plots_tax$plot[[20]] + 
#                                                                                                                                                              Evol_plots_tax$plot[[21]]) /  (Evol_plots_tax$plot[[22]] +  Evol_plots_tax$plot[[23]] +  Evol_plots_tax$plot[[24]]) / 
#   (Evol_plots_tax$plot[[25]] +  Evol_plots_tax$plot[[26]] +  Evol_plots_tax$plot[[27]]) /  (Evol_plots_tax$plot[[28]] + 
#                                                                                               Evol_plots_tax$plot[[29]] +  Evol_plots_tax$plot[[30]]) /  (Evol_plots_tax$plot[[31]] +  Evol_plots_tax$plot[[32]] + 
#                                                                                                                                                             Evol_plots_tax$plot[[33]]) /  (Evol_plots_tax$plot[[34]] +  Evol_plots_tax$plot[[35]] +  Evol_plots_tax$plot[[36]]) /
#   (Evol_plots_tax$plot[[37]] +  Evol_plots_tax$plot[[38]] +  Evol_plots_tax$plot[[39]]) /  (Evol_plots_tax$plot[[40]] + 
#                                                                                               Evol_plots_tax$plot[[41]] +  Evol_plots_tax$plot[[42]]) /  (Evol_plots_tax$plot[[43]] +  Evol_plots_tax$plot[[44]] + 
#                                                                                                                                                             Evol_plots_tax$plot[[45]]) / (Evol_plots_tax$plot[[46]] +  Evol_plots_tax$plot[[47]] +  Evol_plots_tax$plot[[48]]) / 
#   (Evol_plots_tax$plot[[49]] +  Evol_plots_tax$plot[[50]] +  Evol_plots_tax$plot[[51]]) /  (Evol_plots_tax$plot[[52]] + 
#                                                                                               Evol_plots_tax$plot[[53]] +  Evol_plots_tax$plot[[54]]) /  (Evol_plots_tax$plot[[55]] +  Evol_plots_tax$plot[[56]] + 
#                                                                                                                                                             Evol_plots_tax$plot[[57]]) / ( Evol_plots_tax$plot[[58]] +  Evol_plots_tax$plot[[59]] +  Evol_plots_tax$plot[[60]]) / 
#   (Evol_plots_tax$plot[[61]] +  Evol_plots_tax$plot[[62]] +  Evol_plots_tax$plot[[63]]) /  (Evol_plots_tax$plot[[64]] + 
#                                                                                               Evol_plots_tax$plot[[65]] +  Evol_plots_tax$plot[[66]]) /  (Evol_plots_tax$plot[[67]] +  Evol_plots_tax$plot[[68]]) 
# 
# dev.off()


# Meme graphique mais pour la part moyenne du taxon sur l'année dans les echantillons

# Fonction qui permettra de plotter plus facilement tout les resultats
Frac_tax <- function(esp,dt){
  
  options(repr.plot.width = 5, repr.plot.height =2)
  
  ggplot(dt, aes(x = as.factor(DATE), y = mean_frac, group = 1)) + 
    geom_point(size = 0.5) +
    geom_line(size = 0.5)+
    labs(title = esp,
         x = "Année",
         y = "Pourcentage")+
    theme_inrae()
  
  ggsave(file=paste("img/Part_taxons/plot",esp, ".jpeg"), width = 8, height = 4)
}

# Part moyenne de chaque taxon sur une année


Percent_Tax_year <- Flore_checked %>% distinct() %>%
  group_by(DATE = year(DATE), CODE_STATION) %>% 
  mutate(Tot_ab = sum(RESULTAT)) %>% 
  ungroup() %>% 
  group_by(DATE, CODE_STATION, code_taxon) %>% 
  mutate(frac_stat = RESULTAT/Tot_ab) %>% 
  ungroup() %>% 
  group_by(DATE, code_taxon) %>% 
  mutate(mean_frac = round(mean(frac_stat)*100,2)) %>% 
  ungroup() %>% 
  filter(code_taxon %in% unique(New_taxons %>% pull(abre))) %>% 
  select(DATE, code_taxon, mean_frac) %>% 
  distinct(code_taxon, DATE, .keep_all = TRUE) 


Evol_plots_Percent_tax <- Percent_Tax_year %>%
  group_nest(code_taxon) %>% 
  mutate(plot = map2(code_taxon, data, Frac_tax))

# Sauvegarde du plot sous R

# png(paste("img/Part_taxons/Global", ".jpeg", sep=""),width = 10000 ,
#     height = 15000,  res = 400)
# 
# 
# (Evol_plots_Percent_tax$plot[[1]] + Evol_plots_Percent_tax$plot[[2]] + Evol_plots_Percent_tax$plot[[3]]) / (Evol_plots_Percent_tax$plot[[4]] + 
#                                                                                                               Evol_plots_Percent_tax$plot[[5]] + Evol_plots_Percent_tax$plot[[6]]) / (Evol_plots_Percent_tax$plot[[7]] +  Evol_plots_Percent_tax$plot[[8]] + 
#                                                                                                                                                                                         Evol_plots_Percent_tax$plot[[9]]) / (Evol_plots_Percent_tax$plot[[10]] +  Evol_plots_Percent_tax$plot[[11]] +  Evol_plots_Percent_tax$plot[[12]]) / 
#   (Evol_plots_Percent_tax$plot[[13]] +  Evol_plots_Percent_tax$plot[[14]] +  Evol_plots_Percent_tax$plot[[15]]) /   (Evol_plots_Percent_tax$plot[[16]] + 
#                                                                                                Evol_plots_Percent_tax$plot[[17]] +  Evol_plots_Percent_tax$plot[[18]]) / ( Evol_plots_Percent_tax$plot[[19]] +  Evol_plots_Percent_tax$plot[[20]] + 
#                                                                                                                                                              Evol_plots_Percent_tax$plot[[21]]) /  (Evol_plots_Percent_tax$plot[[22]] +  Evol_plots_Percent_tax$plot[[23]] +  Evol_plots_Percent_tax$plot[[24]]) / 
#   (Evol_plots_Percent_tax$plot[[25]] +  Evol_plots_Percent_tax$plot[[26]] +  Evol_plots_Percent_tax$plot[[27]]) /  (Evol_plots_Percent_tax$plot[[28]] + 
#                                                                                               Evol_plots_Percent_tax$plot[[29]] +  Evol_plots_Percent_tax$plot[[30]]) /  (Evol_plots_Percent_tax$plot[[31]] +  Evol_plots_Percent_tax$plot[[32]] + 
#                                                                                                                                                             Evol_plots_Percent_tax$plot[[33]]) /  (Evol_plots_Percent_tax$plot[[34]] +  Evol_plots_Percent_tax$plot[[35]] +  Evol_plots_Percent_tax$plot[[36]]) /
#   (Evol_plots_Percent_tax$plot[[37]] +  Evol_plots_Percent_tax$plot[[38]] +  Evol_plots_Percent_tax$plot[[39]]) /  (Evol_plots_Percent_tax$plot[[40]] + 
#                                                                                               Evol_plots_Percent_tax$plot[[41]] +  Evol_plots_Percent_tax$plot[[42]]) /  (Evol_plots_Percent_tax$plot[[43]] +  Evol_plots_Percent_tax$plot[[44]] + 
#                                                                                                                                                             Evol_plots_Percent_tax$plot[[45]]) / (Evol_plots_Percent_tax$plot[[46]] +  Evol_plots_Percent_tax$plot[[47]] +  Evol_plots_Percent_tax$plot[[48]]) / 
#   (Evol_plots_Percent_tax$plot[[49]] +  Evol_plots_Percent_tax$plot[[50]] +  Evol_plots_Percent_tax$plot[[51]]) /  (Evol_plots_Percent_tax$plot[[52]] + 
#                                                                                               Evol_plots_Percent_tax$plot[[53]] +  Evol_plots_Percent_tax$plot[[54]]) /  (Evol_plots_Percent_tax$plot[[55]] +  Evol_plots_Percent_tax$plot[[56]] + 
#                                                                                                                                                             Evol_plots_Percent_tax$plot[[57]]) / ( Evol_plots_Percent_tax$plot[[58]] +  Evol_plots_Percent_tax$plot[[59]] +  Evol_plots_Percent_tax$plot[[60]]) / 
#   (Evol_plots_Percent_tax$plot[[61]] +  Evol_plots_Percent_tax$plot[[62]] +  Evol_plots_Percent_tax$plot[[63]]) /  (Evol_plots_Percent_tax$plot[[64]] + 
#                                                                                               Evol_plots_Percent_tax$plot[[65]] +  Evol_plots_Percent_tax$plot[[66]]) /  (Evol_plots_Percent_tax$plot[[67]] +  Evol_plots_Percent_tax$plot[[68]]) 
# 
# dev.off()


# Carte_repartition_taxons ------------------------------------------------


# Recuperation du tableau diatomique pour fusion (Que les taxons des DREAL)
Tab_carto <- Flore_checked %>% distinct() %>%
  st_as_sf(coords = c("x", "y"), crs = 2154) %>% 
  st_transform(4326) %>% 
  filter(code_taxon %in% unique(New_taxons %>% pull(abre))) %>% 
  mutate(DATE = year(DATE)) %>%
  group_by(DATE) %>% 
  mutate(code_taxon = as.factor(code_taxon),
                             ID = cur_group_id()) %>% 
  left_join(Evol_plots_Percent_tax %>%
              select(code_taxon, plot), by = "code_taxon") %>%
  rename(plot_percent = plot) %>%
  left_join(Evol_plots_tax %>%
              select(code_taxon, plot), by = "code_taxon") %>% 
  rename(plot_occur = plot)

# Creation des fonds graphiques 

leaflet() %>%
  setView(lng = 3.4, lat = 47, zoom = 6) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas,
                   group = "Fond clair") %>% 
  addProviderTiles(providers$CartoDB.DarkMatter,
                   group = "Fond noir") %>% 
  addProviderTiles(providers$GeoportailFrance.orthos,
                   group = "Fond satellite") -> map_base

# Palette de couleurs pour les annees des grpahiques

pal <- colorNumeric(palette = c("red", "blue", "green", "yellow", 
                                "purple", "pink","orange", "brown",
                                "black","grey", "aquamarine", "tan",
                                "darkred", "darkgreen", "darkorange"), domain = unique(Tab_carto$ID))

#Fonction pour Plotter les annees que l'on veut

# Creation des point de coordonnee


plot_tax = function(Plot_data, date){
  
  y1 = 51.5
  x1 = seq(-8, -0.7, length.out = nrow(Plot_data %>% ungroup() %>%  filter(DATE == date) %>% distinct(code_taxon)))
  
  y2 = 52.5
  x2 = seq(-8, -0.7, length.out = nrow(Plot_data %>% ungroup() %>%  filter(DATE == date) %>% distinct(code_taxon)))
  
  pnt1 = st_as_sf(data.frame(x = x1, y = y1),coords = c("x", "y"), crs = 4326)
  pnt2 = st_as_sf(data.frame(x = x2, y = y2),coords = c("x", "y"), crs = 4326)
  pnt3 = st_as_sf(data.frame(x = c(-9,-9), y = c(51.4, 52.4)),coords = c("x", "y"), crs = 4326)
  

  img1 = as.character(Plot_data %>% ungroup() %>%  filter(DATE == date) %>% 
                       distinct(code_taxon, .keep_all = T) %>% 
                       arrange(code_taxon) %>% .$plot_occur)
  
  img2 = as.character(Plot_data %>% ungroup() %>%  filter(DATE == date) %>% 
                       distinct(code_taxon, .keep_all = T) %>% 
                       arrange(code_taxon) %>% .$plot_percent)
  
  
  map <- map_base %>% addCircles(data = Plot_data %>% filter(DATE == date), group = ~code_taxon,
             label = ~Commune, color = ~pal(ID),
             opacity = 1, radius = 1) %>% 
  
    addRectangles(
      lng1=-10, lat1=53,
      lng2= 1, lat2=51,
      fillColor = "grey",
      opacity = 0.5,
      color = "grey") %>% 
  
  addCircles(data = Plot_data %>% filter(DATE == date), color = "snow", fillColor = "red",
             opacity = 0) %>% 
    
  addMarkers(data = pnt3, group = "pnt3",
                   label = c("Occurence des taxons (ordre alphabétique)","Moyenne de présence en % (ordre alphabétique)")) %>% 
    
  addCircleMarkers(data = pnt1, group = "pnt1", radius = 1, opacity = 1, color = "black") %>% 
  addPopupImages(img1, group = "pnt1", width = 550, height = 350, tooltip = T) %>%
    
  addCircleMarkers(data = pnt2, group = "pnt2", radius = 1, opacity = 1, color = "red") %>% 
  addPopupImages(img2, group = "pnt2", width = 550, height = 350, tooltip = T) %>%
  addLayersControl(position = "topleft",
                   baseGroups = c("Fond clair",
                                  "Fond noir", 
                                  "Fond satellite"),
                   overlayGroups = Plot_data %>% filter(DATE == date) %>% arrange(code_taxon) %>% pull(code_taxon),
                   options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(group = Plot_data$code_taxon) %>% 
  add_titre(titre = "Carte repartition taxons", sousTitre = as.character(date)) 
  return(map)}

map_2007 <- plot_tax(Tab_carto, 2007)
map_2008 <- plot_tax(Tab_carto, 2008)
map_2009 <- plot_tax(Tab_carto, 2009)
map_2010 <- plot_tax(Tab_carto, 2010)
map_2011 <- plot_tax(Tab_carto, 2011)
map_2012 <- plot_tax(Tab_carto, 2012)
map_2013 <- plot_tax(Tab_carto, 2013)
map_2014 <- plot_tax(Tab_carto, 2014)
map_2015 <- plot_tax(Tab_carto, 2015)
map_2016 <- plot_tax(Tab_carto, 2016)
map_2017 <- plot_tax(Tab_carto, 2017)
map_2018 <- plot_tax(Tab_carto, 2018)
map_2019 <- plot_tax(Tab_carto, 2019)
map_2020 <- plot_tax(Tab_carto, 2020)
map_2021 <- plot_tax(Tab_carto, 2021)

saveWidget(map_2007, file = "img/map_2007.html")
saveWidget(map_2008, file = "img/map_2008.html")
saveWidget(map_2009, file = "img/map_2009.html")
saveWidget(map_2010, file = "img/map_2010.html")
saveWidget(map_2011, file = "img/map_2011.html")
saveWidget(map_2012, file = "img/map_2012.html")
saveWidget(map_2013, file = "img/map_2013.html")
saveWidget(map_2014, file = "img/map_2014.html")
saveWidget(map_2015, file = "img/map_2015.html")
saveWidget(map_2016, file = "img/map_2016.html")
saveWidget(map_2017, file = "img/map_2017.html")
saveWidget(map_2018, file = "img/map_2018.html")
saveWidget(map_2019, file = "img/map_2019.html")
saveWidget(map_2020, file = "img/map_2020.html")
saveWidget(map_2021, file = "img/map_2021.html")


Global_map <- map_base %>% addCircles(data = Tab_carto %>% select(-plot_percent, -plot_occur), group = ~code_taxon,
                        label = ~DATE, color = ~pal(ID),
                        opacity = 1, popup = ~Commune) %>%
  addLayersControl(position = "topleft",
                   baseGroups = c("Fond clair",
                                  "Fond noir", 
                                  "Fond satellite",
                                  "Fond topographie"),
                   overlayGroups = Tab_carto$code_taxon,
                   options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(group = Tab_carto$code_taxon) 


# NDMS --------------------------------------------------------------------

`%notin%` <- Negate(`%in%`)
Communes = c("Mana", "Roura", "Regina", "Maripasoula", "Papaichton", "Kourou",
             "Saint-Laurent-du-Maroni", "Iracoubo", "Montsinery-Tonnegrande", "Apatou",
             "Massangis")

Flore2 <- Flore %>% 
  distinct() %>% 
  filter(Commune != "Massangis") %>% 
  sf::st_as_sf(coords=c("x","y"),crs=2154) %>%
  sf::st_transform(crs=4326) %>% 
  extract(geometry, c('lon', 'lat'), '\\((.*), (.*)\\)', convert = TRUE) %>% 
  select(-geometry) %>% 
  bind_rows(Flore %>% distinct() %>% 
              as_tibble() %>%
              sf::st_as_sf(coords=c("x","y"),crs=2154) %>%
              sf::st_transform(crs=4326) %>%
              extract(geometry, c('lon', 'lat'), '\\((.*), (.*)\\)', convert = TRUE) %>% 
              select(-geometry) %>% filter(Commune == "Massangis") %>% 
              mutate(lon = 4.00, lat = 47.6)) %>% 
  filter(Commune %notin% Communes[-11]) %>% drop_na()

# Pour réaliser des analyses mutivariees on a deja besoin de standardise
# les abondances, à savoir qu'elles ne sont pas totues faites sur le meme 
# nombre d'indivividus, donc on va diviser chaque RESULTAT par le Tot_indiv de 
# la station a la date x. Cela nous permettra d'obtenir des abondance relative 
# et non pas absolues

tab <- Flore2 %>% select(CODE_STATION, DATE, code_taxon, RESULTAT) %>% 
  group_by(CODE_STATION, DATE) %>% 
  mutate(Presence = if_else(code_taxon %in% unique(New_taxons %>% pull(abre)) == TRUE, 1, 0)) %>% 
  filter(any(Presence == 1)) %>% 
  ungroup() %>% 
  select(-DATE, -Presence) %>% 
  group_by(CODE_STATION, code_taxon) %>% 
  mutate(RESULTAT = sum(RESULTAT)) %>% distinct() %>% 
  pivot_wider(names_from = code_taxon, values_from = RESULTAT) %>% 
  replace(is.na(.), 0) %>% column_to_rownames(., var = "CODE_STATION")

# # Methode NMDS (non dimensional scaling) 
# example_NMDS=metaMDS(tab_stand, # Our community-by-species matrix
#                      k=2, trymax = 100, distance = "bray") # The number of reduced dimensions
# 
# example_NMDS
# plot(example_NMDS)
# orditorp(example_NMDS,display="species",col="red",air=0.01)
# orditorp(example_NMDS,display="sites",cex=1.25,air=0.01)


# AFC ---------------------------------------------------------------------

# On essaie en transformant une abondance positive en présence (1 vs 0)

library(FactoMineR)
library(ade4)

tab_AFC <- tab[1:30,1:75] %>% decostand("hellinger")
tab_AFC <- tab_AFC[,colSums(tab_AFC) != 0]
cor(tab_AFC)
AFC <- CA(tab_AFC)

# Récupération des contributions absolues des espèces

fviz_contrib(AFC, choice ="col", axes = 1)
fviz_contrib(AFC, choice ="col", axes = 2)

# Asymetric biplot
fviz_ca_biplot(AFC, 
               map ="rowprincipal", arrow = c(TRUE, TRUE),
               repel = TRUE)

# Contribution
fviz_ca_biplot(AFC, map ="colgreen", arrow = c(TRUE, FALSE),
               repel = TRUE)

get_ca_col(AFC)$contrib

# Représentation graphique des colonnes et des lignes puis des deux
fviz_ca_row(AFC, repel = TRUE)
fviz_ca_col(AFC, repel = TRUE)
fviz_ca_biplot(AFC, repel = TRUE)

# Obtenir un graph pour savoir si un point est bien représenté 
#  If a row item is well represented by two dimensions, 
# the sum of the cos2 is closed to one
fviz_ca_col(AFC, col.col = "cos2",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
            repel = TRUE)

fviz_cos2(AFC, choice = "col", axes = 1:2)# Sous forme histogramme

# Obtenir un graph des importance de chaque taxon sur 1 dimension

corrplot(get_ca_col(AFC)$cos2, is.corr=FALSE)

# CAH
AFC$eig # Pour voir les dimensions qu'on veux garder 
dist.dudi(AFC,amongrow = T)

res <- HCPC(AFC)
names(res)
res$data.clust


# Co-occurence ------------------------------------------------------------


tab[tab > 0] <- 1

Test_coocur <- cooccur(t(tab),
                       type = "spp_site",
                       spp_names = TRUE,
                       thresh = FALSE)
summary(Test_coocur)
prob.table(Test_coocur)
effect.sizes(Test_coocur, standardized = TRUE, matrix = FALSE)
plot(Test_coocur)# Regarder les coocurrences positive pour affilier un profil
# existant ou pas au taxons qui le nécessite

















