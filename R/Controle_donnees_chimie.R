# Type d'algorithme : Evaluation de la qualité des données physico-chimiques de Naïades
# Auteur(s)         : Thibault Leboucher
# Date              : 11/03/22
# Interpreteur      : R version 4.1.2
# Pre-requis        : aucun
# Fichiers lies     : naiades_export *
# Commentaires      : aucun

rm(list=ls())

# 1 - SETUP ---------------------------------------------------------------
## Initialisation
set.seed(123)

## Packages
library(tidyverse)
library(vegan)
library(visdat)
library(lubridate)

## Déclaration des fonctions
`%not_in%` <- Negate(`%in%`)
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("slice", "dplyr")



## Data

chem_2007 <- as.tibble(fread("data/Donnes_chimiques/Naiades_Export_France_Entiere_PC_2022/analyses_2009.csv")) %>% mutate(IncertAna = as.numeric(IncertAna)) %>% mutate(RdtExtraction = as.numeric(RdtExtraction))

# 2 - SELECTION DES DONNEES -----------------------------------------------
phychi_tot <- chem_2007

## Transformation des résultats en variable numérique
phychi_tot <- phychi_tot %>% 
  mutate(RsAna = as.numeric(RsAna)) %>% 
  mutate(LdAna = as.numeric(LdAna)) %>% 
  mutate(LqAna = as.numeric(LqAna))

## Analyses dans l'eau
phychi_tot <- phychi_tot %>% 
  filter(CdSupport == 3)

phychi_tot %>% 
  filter(CdParametre == 1302 | CdParametre == 1304 | CdParametre == 1311 | 
           CdParametre == 1314 | CdParametre == 1335| 
           CdParametre == 1433 | CdParametre == 1340) %>% 
  group_by(LbLongParamètre, CdUniteMesure, SymUniteMesure) %>% 
  summarise(Moyenne = mean(RsAna),
            count = n())





## Choix de fractions : eau brute et phase aqueuse de l'eau (on garde toutes les mesures de CaCO3 pour la classification des micropolluants métaliques)
phychi_tot <- phychi_tot %>% 
  filter(CdFractionAnalysee %in% c(3,23))

## Limites de détection et de quantification pour tous les param (moyenne)
limites_param <- phychi_tot %>% 
  select(CdParametre,LbLongParamètre,LdAna,LqAna) %>% 
  group_by(CdParametre) %>% 
  mutate(LdParam = quantile(LdAna, 0.95, na.rm = TRUE)) %>% 
  mutate(LqParam = quantile(LqAna, 0.95, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(CdParametre,LdParam,LqParam) %>% 
  distinct() %>% 
  mutate(LdParam = if_else(condition = is.na(LdParam) | LdParam > LqParam, true = LqParam, false = LdParam)) # évite que la LD > LQ

## Gestion des codes remarques analyses 0 : donnée non mesurée // 8 & 9 -> dénombrement (aucun sens pour des données quantitatives) // 3 : seuil de saturation
phychi_tot <- phychi_tot %>%
  left_join(y = limites_param, by = "CdParametre") %>% 
  filter(CdRqAna %not_in% c(0,3,8,9)) %>%
  ## Seuil de détection
  mutate(RsAna = if_else(condition = RsAna > LdParam & CdRqAna == 2, true = LdParam, false = RsAna)) %>% 
  mutate(RsAna = if_else(condition = CdRqAna == 2, true = RsAna / 2, false = RsAna)) %>% 
  ## Trace (entre détection et quantification)
  mutate(RsAna = if_else(condition = RsAna > LqParam & CdRqAna == 7, true = LqParam, false = RsAna)) %>% 
  mutate(RsAna = if_else(condition = CdRqAna == 7, true = RsAna / 2, false = RsAna)) %>% 
  ## Seuil de quantification
  mutate(RsAna = if_else(condition = RsAna > LqParam & CdRqAna == 10, true = LqParam, false = RsAna)) %>% 
  mutate(RsAna = if_else(condition = CdRqAna == 10, true = RsAna / 2, false = RsAna)) %>% 
  filter(!is.na(RsAna))

# 3 - GESTION DES UNITES DE MESURE ----------------------------------------
## Azote Kjeldahl : retirer les mesures en mg par Kg
phychi_tot <- phychi_tot %>% 
  filter(!(CdParametre == 1319 & CdUniteMesure == 395))

## DBO5 : une mesure exprimée en m ???
phychi_tot <- phychi_tot %>% 
  filter(!(CdParametre == 1313 & CdUniteMesure == 111))

## Nitrates : N * 4.429 = NO3
phychi_tot <- phychi_tot %>% 
  mutate(RsAna = if_else(condition = CdParametre == 1340 & CdUniteMesure == 168, true = RsAna * 4.429, false = RsAna))

## Nitrites : N * 3.286 = NO2
phychi_tot <- phychi_tot %>% 
  mutate(RsAna = if_else(condition = CdParametre == 1339 & CdUniteMesure == 168, true = RsAna * 3.286, false = RsAna))

## OrthoP : P * 3.066 = PO4 3- 
phychi_tot <- phychi_tot %>% 
  mutate(RsAna = if_else(condition = CdParametre == 1433 & CdUniteMesure == 177, true = RsAna * 3.066, false = RsAna))

## Potassium : micro à milli
phychi_tot <- phychi_tot %>% 
  mutate(RsAna = if_else(condition = CdParametre == 1367 & CdUniteMesure == 133, true = RsAna / 1000, false = RsAna))

## Silicates : SiO2 * 1.266 = SiO3
phychi_tot <- phychi_tot %>% 
  mutate(RsAna = if_else(condition = CdParametre == 1342 & CdUniteMesure == 273, true = RsAna * 1.266, false = RsAna))

## Sulfates : S * 2.996 = SO4 // qq mesures exprimées en '‰ vs SMOW' ???
phychi_tot <- phychi_tot %>% 
  mutate(RsAna = if_else(condition = CdParametre == 1338 & CdUniteMesure == 180, true = RsAna * 2.996, false = RsAna)) %>% 
  filter(!(CdParametre == 1338 & CdUniteMesure == 32))

## Turbidité : 0 à 20 -> 1 NFU = 1 NTU // > 20 -> 1 NFU = 0.6 NTU
phychi_tot <- phychi_tot %>% 
  mutate(RsAna = if_else(condition = CdParametre == 1295 & CdUniteMesure == 233 & RsAna > 20, true = RsAna / 0.6, false = RsAna))
