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
library(sqlutils)
sqlPaths("sql/")

# require(RSQLite)
# 
# drv <- dbDriver("MySQL")
# 
# conR <- dbConnect(drv,
#                   dbname = "pandore",
#                   host = "10.69.192.179",
#                   port = 3306,
#                   user = "jamoneau",
#                   password = "aurelien")
# 
# sort(dbListTables(conR))
# 
# sqlutils::execQuery(connection = conR, query = "Requete_1_seb_le_meilleur")


channel=odbcConnect("pandore")
sqlutils::getQueries()



# Recuperation des codes 4 lettres des taxons 
code <- bind_rows(as.tibble(read.csv2("data/Donnees_utilisables/Codes 4 lettres taxons.csv", stringsAsFactors = FALSE) %>% 
                              mutate(SANDRE = as.numeric(SANDRE))) %>% 
                    select(AFNOR = "cd_apa", "SANDRE"),as.tibble(read.csv2("data/Donnees_utilisables/Base_Omnidia.csv", stringsAsFactors = FALSE) %>% 
                                                                   select(AFNOR = CODE, "SANDRE") %>% 
                                                                   filter(SANDRE!=0)  %>% 
                                                                   mutate(SANDRE = as.numeric(SANDRE))), as.tibble(read.csv2("data/Donnees_utilisables/Complement_code4L.csv", stringsAsFactors = FALSE)) %>% 
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

  filter(Code_groupe_taxo == 10) %>% # Recuperation uniquement des diatomees
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
Transcoded_Flore <- Diatom2 %>% 
  left_join(code, by = "SANDRE") %>% 
  mutate(AFNOR = if_else(is.na(AFNOR), CODE_TAXON, AFNOR)) %>% 
  select(DATE, CODE_STATION, AFNOR, SANDRE, RESULTAT, x, y, commune) %>% 
  filter(SANDRE != 0) %>% 
  # Fusion de la nouvelle taxonomie et remplaecement
  left_join(as.tibble(read.csv2("data/Donnees_utilisables/TableTranscodification.csv", stringsAsFactors = FALSE)) %>% 
              select(AFNOR = "ABRE", True_name = "CodeValid"), by = "AFNOR") %>% 
  mutate(AFNOR = if_else(is.na(True_name) == T, AFNOR, True_name)) %>% 
  select(-True_name) %>% filter(!is.na(AFNOR)) %>% rename(code_taxon = "AFNOR")


# Ajout coordonnees stations manquantes
Diatom3 <- Transcoded_Flore %>%
  mutate(CODE_STATION = str_remove(CODE_STATION, "^0+")) %>%
  left_join(as.tibble(read.csv2("data/Donnees_utilisables/Station.csv", stringsAsFactors = FALSE)) %>%
    select(
      CODE_STATION = CdStationMesureEauxSurface, x = CoordXStationMesureEauxSurface,
      y = CoordYStationMesureEauxSurface, Commune = LbCommune
    ) %>%
    mutate(x = as.numeric(x), y = as.numeric(y)), by = "CODE_STATION") %>%
  mutate(
    x = if_else(is.na(x.x == T), x.y, x.x),
    y = if_else(is.na(y.x == T), y.y, y.x),
    Commune = if_else(is.na(Commune == T), commune, Commune),
    Commune = if_else(is.na(Commune == T), "Inconnue", Commune)
  ) %>%
  select(-x.x, -y.x, -x.y, -y.y, -commune) %>%
  drop_na() %>%
  distinct()


# Recuperation des données chimiques de Pandore
tic()
channel=odbcConnect("pandore")

# seb

# Station que l'on veut selectionner avec SQL
vec <- paste0(Diatom3 %>% pull(CODE_STATION) %>% unique(),"'",collapse = ",'")
vec <- str_replace(vec,"'$","")

sqlQuery(channel, "SELECT cd_opecont, cd_site, date_opecont, cd_obsPhyChi, cd_support, resultat, cd_param, cd_unite, nom_param FROM sandre_parametre
    join pandore.listes_phychi using(cd_param)
    join pandore.opecont_phychi using(cd_obsPhyChi)
    join pandore.compil using(cd_opecont)
    WHERE cd_param IN (1302,1304,1311,1314,1335,1433,1340) AND 
    cd_support = 3 AND 
         cd_site LIKE ('6092000')   ") # Il faudra ajouter le vecteur de CODE_STATION a recuperer sous forme de vecteur R (voir commande seb mail)
#


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

save(Chimie_pandore, file = "data/Donnees_utilisables/Chimie_pandore.RData")
save(Diatom3, file = "data/Donnees_utilisables/Diatomees.RData")

write.csv2(Diatom3, file = "data/Donnees_utilisables/Diatomees.csv")
write.csv2(Chimie_pandore, file = "data/Donnees_utilisables/Chimie_pandore.csv")

# # Regroupement de toutes les années et enregistrement des données ---------

# On utilise la fonction Pick_date pour lancer automatiquement l'extraction
# des donnees et la fusion chimie_biologie

# ATTENTION, besoin de 3h00 minimum pour tout récupérer

# chargement des fonctions pour filtration des donnees 

source("R/Fonction_fusion_chimie_biologie.R")
source("R/Fonction_pick_date.R")
load("data/Donnees_utilisables/Diatomees.RData")
load("data/Donnees_utilisables/Chimie_pandore.RData")


# Sauvegarde pour test du script IBD
 write.table(Flore[1:10000,] %>% drop_na(), 
             file = "./data/Donnees_utilisables/Test.txt", 
             sep = "\t",row.names = FALSE)


 write.csv2(Flore, file = "data/Donnees_utilisables/Flore.csv")

 library(tidyverse)

Diatom3 %>%  filter(code_taxon == "ECTO")
