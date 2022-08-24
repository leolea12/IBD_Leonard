# Chargement des packages 

require(tidyverse)
require(InraeThemes)
require(readxl)
require(fuzzyjoin)
library(readr)
library(leaflet)
library(sf)
require(tictoc)
library(lubridate)
library(purrr)
library(data.table)
library(progress)
library(rlist)
library(openxlsx)
library(purrr)
library(cooccur)
library(magrittr)
library(RODBC)
library(RMySQL)
library(RPostgreSQL)


# Recuperation des codes 4 lettres des taxons 
code <- bind_rows(as.tibble(read.csv2("data/Donnees_utilisables/Codes 4 lettres taxons.csv", stringsAsFactors = FALSE) %>% 
                              mutate(SANDRE = as.numeric(SANDRE))) %>% 
                    select(AFNOR = "cd_apa", "SANDRE"),as.tibble(read.csv2("data/Code_4L_taxons/Base_Omnidia.csv", stringsAsFactors = FALSE) %>% 
                                                                   select(AFNOR = CODE, "SANDRE") %>% 
                                                                   filter(SANDRE!=0)  %>% 
                                                                   mutate(SANDRE = as.numeric(SANDRE))), as.tibble(read.csv2("data/Code_4L_taxons/Complement_code4L.csv", stringsAsFactors = FALSE)) %>% 
                    select(AFNOR = "TAXON", "SANDRE") %>% 
                    mutate(SANDRE = as.numeric(SANDRE)) %>% drop_na()) %>% 
  distinct(SANDRE, .keep_all = T) %>%  
  bind_rows(data.frame(AFNOR = c("ERMS","FGNO","DIAS","CMFO","LSAP"), # ajout de ceux que l'on trouve sur le SANDRE à code
                       SANDRE = c(4767, 38468, 20336, 45516, 66512)))


# Base de donnees NAIADES ------------------------------------------------

# Importation de la base de donnees biologiques de NAIADE
Diatom <- as.tibble(fread("data/Donnees_de_base/fauneflore.csv")) %>%

  select("CODE_STATION" = CdStationMesureEauxSurface,
         "CODE_OPERATION" = RefOperationPrelBio,
         "Nom_groupe_taxo" = LbSupport,
         "DATE" = DateDebutOperationPrelBio,
         "SANDRE" = CdAppelTaxon,
         "Nom_latin_taxon" = NomLatinAppelTaxon,
         "RESULTAT" = RsTaxRep,
         "Code_groupe_taxo" = CdSupport) %>%

  filter(Code_groupe_taxo == 10) %>%
  arrange(DATE,
          CODE_STATION, CODE_OPERATION,
          Nom_latin_taxon, RESULTAT)

#
# Recupération données pandore --------------------------------------------


# Requêtage de la base de données

# Lien SQL / R
channel=odbcConnect("pandore")

# JOINTURE PANDORE / DONNEES NAIADES

Diatom2 <- Diatom %>% mutate(DATE = as.Date(DATE)) %>% select(-Code_groupe_taxo, -Nom_latin_taxon, -Nom_groupe_taxo) %>% 
bind_rows(as.tibble(sqlQuery(channel, "SELECT cd_opecont, cd_taxon, cd_OMNIDIA, comptage, cd_site, date_opecont, x, y, commune FROM compil
         join pandore.site using (cd_site)
         join pandore.listes_diatomee using (cd_opecont) WHERE date_opecont BETWEEN '2007-01-01' AND '2021-12-31'") %>% #Recuperation des donnees
                        select(CODE_OPERATION = "cd_opecont", CODE_STATION = "cd_site",
                               DATE = "date_opecont", SANDRE = "cd_taxon", CODE_TAXON = "cd_OMNIDIA", RESULTAT = "comptage", x, y, commune)) %>% 
              mutate(CODE_OPERATION = as.character(CODE_OPERATION))) %>% 
  arrange(DATE) %>% 
  distinct(.keep_all = T) # On enleve les lignes identiques car elles representent le meme prelevement

# Etape de transcodification du fichier FLORE pour recuperer les bons noms
# de taxons et ceux manquants

# Remplacement anciennes taxonommie par nouvelle
Transcoded_Flore <- Diatom2 %>% left_join(code, by = "SANDRE") %>% 
  mutate(AFNOR = if_else(is.na(AFNOR), CODE_TAXON, AFNOR)) %>% 
  select(DATE, CODE_STATION, AFNOR, SANDRE, RESULTAT, x, y, commune) %>% 
  filter(SANDRE != 0) %>% 
  # Fusion de la nouvelle taxonomie et remplaecement
  left_join(as.tibble(read.csv2("data/Donnees_utilisables/TableTranscodification.csv", stringsAsFactors = FALSE)) %>% 
              select(AFNOR = "ABRE", True_name = "CodeValid"), by = "AFNOR") %>% 
  mutate(AFNOR = if_else(is.na(True_name) == T, AFNOR, True_name)) %>% 
  select(-True_name) %>% filter(!is.na(AFNOR)) %>% rename(code_taxon = "AFNOR")


# Ajout coordonnees stations manquantes
Diatom3 <- Transcoded_Flore %>% mutate(CODE_STATION = str_remove(CODE_STATION, "^0+")) %>% 
  left_join(as.tibble(read.csv2("data/Donnees_utilisables/Station.csv", stringsAsFactors = FALSE)) %>% 
              select(CODE_STATION = CdStationMesureEauxSurface, x = CoordXStationMesureEauxSurface, 
                     y = CoordYStationMesureEauxSurface, Commune = LbCommune) %>% 
              mutate(x = as.numeric(x), y = as.numeric(y)), by = "CODE_STATION") %>% 
  mutate(x = if_else(is.na(x.x == T), x.y, x.x),
         y = if_else(is.na(y.x == T), y.y, y.x),
         Commune = if_else(is.na(Commune == T), commune, Commune),
         Commune = if_else(is.na(Commune==T), "Inconnue", Commune)) %>% 
  select(-x.x, -y.x, -x.y, -y.y, -commune) %>% 
  drop_na() %>% 
  distinct()


# Recuperation des données chimiques de Pandore
tic()
channel=odbcConnect("pandore")

Chimie_pandore <- as.tibble(sqlQuery(channel, "SELECT cd_opecont, cd_site, date_opecont, cd_obsPhyChi, cd_support, resultat, cd_param, cd_unite, nom_param FROM sandre_parametre
    join pandore.listes_phychi using(cd_param)
    join pandore.opecont_phychi using(cd_obsPhyChi)
    join pandore.compil using(cd_opecont)
    WHERE cd_param = 1302 OR cd_param = 1304 OR cd_param = 1311 OR 
                        cd_param = 1314 OR cd_param = 1335 OR cd_param = 1433 OR cd_param = 1340 AND cd_support = 3") %>% 
                      rename(CODE_STATION = cd_site,
                             Code_Unite_mesure = cd_unite,
                             Nom_parametre = nom_param,
                             Code_parametre = cd_param,
                             DATE = date_opecont,
                             Concentration = resultat,
                             Code_Prelevement = cd_obsPhyChi,
                             CdSupport = cd_support) %>% 
                      select(-cd_opecont) %>% 
                      arrange(DATE) %>% 
                      subset(CODE_STATION %in% c(unique(Diatom3$CODE_STATION))) %>% 
                      mutate(Code_Unite_mesure = as.character(Code_Unite_mesure),
                             Code_Prelevement = as.character(Code_Prelevement)) %>% 
                      select(CODE_STATION, Code_Prelevement, DATE,       
                             Code_parametre, Nom_parametre,
                             Concentration, Code_Unite_mesure))
toc()
# # Regroupement de toutes les années et enregistrement des données ---------

# On utilise la fonction Pick_date pour lancer automatiquement l'extraction
# des donnees et la fusion chimie_biologie

# ATTENTION, besoin de 3h00 minimum pour tout récupérer

# chargement des fonctions pour filtration des donnees 

source("R/Fonction_fusion_chimie_biologie.R")
source("R/Fonction_pick_date.R")


tic()
Tab_2007 = Pick_date(2007)
Tab_2008 = Pick_date(2008)
Tab_2009 = Pick_date(2009)
Tab_2010 = Pick_date(2010)
Tab_2011 = Pick_date(2011)
Tab_2012 = Pick_date(2012)
Tab_2013 = Pick_date(2013)
Tab_2014 = Pick_date(2014)
Tab_2015 = Pick_date(2015)
Tab_2016 = Pick_date(2016)
Tab_2017 = Pick_date(2017)
Tab_2018 = Pick_date(2018)
Tab_2019 = Pick_date(2019)
Tab_2020 = Pick_date(2020)
Tab_2021 = Pick_date(2021)
toc()
# Ouverture de la feuille excel de l'année choisie dans les fichiers traités


# Convertir sur 1000 les abondances avant de travailler
# Recuperer le script SEEE 
# Sortir un fichier qui a le meme tete que le script SEEE

list_of_files <- list.files(path = "./data/Donnees_utilisables",
                            pattern = "\\.xlsx$",
                            full.names = TRUE)

# Une fois que les donnees auront ete sauver au format R.data, on pourra les 
# re-importer directement sans passer par le code, juste avec load(data)

x <- as.tibble(list(file = c("data/Donnees_utilisables/2007_traitee.xlsx", "data/Donnees_utilisables/2007_traitee.xlsx",
                             "data/Donnees_utilisables/2008_traitee.xlsx", "data/Donnees_utilisables/2008_traitee.xlsx",
                             "data/Donnees_utilisables/2009_traitee.xlsx", "data/Donnees_utilisables/2009_traitee.xlsx",
                             "data/Donnees_utilisables/2010_traitee.xlsx", "data/Donnees_utilisables/2010_traitee.xlsx",
                             "data/Donnees_utilisables/2011_traitee.xlsx", "data/Donnees_utilisables/2011_traitee.xlsx",
                             "data/Donnees_utilisables/2012_traitee.xlsx", "data/Donnees_utilisables/2012_traitee.xlsx",
                             "data/Donnees_utilisables/2013_traitee.xlsx", "data/Donnees_utilisables/2013_traitee.xlsx",
                             "data/Donnees_utilisables/2014_traitee.xlsx", "data/Donnees_utilisables/2014_traitee.xlsx",
                             "data/Donnees_utilisables/2015_traitee.xlsx", "data/Donnees_utilisables/2015_traitee.xlsx",
                             "data/Donnees_utilisables/2016_traitee.xlsx", "data/Donnees_utilisables/2016_traitee.xlsx",
                             "data/Donnees_utilisables/2017_traitee.xlsx", "data/Donnees_utilisables/2017_traitee.xlsx",
                             "data/Donnees_utilisables/2018_traitee.xlsx", "data/Donnees_utilisables/2018_traitee.xlsx",
                             "data/Donnees_utilisables/2019_traitee.xlsx", "data/Donnees_utilisables/2019_traitee.xlsx",
                             "data/Donnees_utilisables/2020_traitee.xlsx", "data/Donnees_utilisables/2020_traitee.xlsx",
                             "data/Donnees_utilisables/2021_traitee.xlsx", "data/Donnees_utilisables/2021_traitee.xlsx"),
                    sheet = c("chimie", "flore", "chimie", "flore",
                              "chimie", "flore", "chimie", "flore",
                              "chimie", "flore", "chimie", "flore",
                              "chimie", "flore", "chimie", "flore",
                              "chimie", "flore", "chimie", "flore",
                              "chimie", "flore", "chimie", "flore",
                              "chimie", "flore", "chimie", "flore",
                              "chimie", "flore")))

# Ouverture de toutes les feuilles de tous les fichiers + combinaison de feuilles
# flore et chimie entre elles

Files <- map2(x$file, x$sheet, ~ read_xlsx(path = .x, sheet = .y))

# Pour recuperer les feuilles, chimie = impair, flore = pair
chimie_sheet = seq(1,30,2)
flore_sheet = seq(2,30,2)

Chimie <- bind_rows(Files[chimie_sheet]) %>%
  select(CODE_STATION,DATE,Code_Prelevement,
         Code_parametre,Nom_parametre,Code_Unite_mesure,Mediane)

Flore <- bind_rows(Files[flore_sheet]) %>%
  select(CODE_STATION, DATE, code_taxon, SANDRE, RESULTAT, Commune, x, y) %>%
  group_by(CODE_STATION, DATE) %>%
  mutate(Tot_indiv = sum(RESULTAT), RESULTAT = (RESULTAT*1000)/Tot_indiv) %>%
  ungroup() %>% mutate_if(is.numeric, round, 0)



# Sauvegarde pour R
save(Chimie, Flore, file = "./data/Donnees_utilisables/Donnees_completes.RData")


# Sauvegarde pour test du script IBD
 write.table(Flore[1:10000,] %>% drop_na(), 
             file = "./data/Donnees_utilisables/Test.txt", 
             sep = "\t",row.names = FALSE)





