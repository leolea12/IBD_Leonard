# Type d'algorithme : IBD
# Auteur(s)         : UDAM
# Date              : 2021-06-16
# Version           : 1.2.4
# Interpreteur	   	: R version 4.0.2 (2020-06-22)
# Pre-requis        : Packages dplyr, tidyr
# Fichiers lies   	: 
# Commentaires 	  	: Indice Biologique Diatomées calculé selon la norme NF T90-354 (Avril 2016).
# La liste principale de 828 taxons identiques à celle de la base Omnidia auxquels sont raccordés 2226
# synonymes, formes anormales et taxons appariés. Cette table intègre tous les taxons de l’annexe A de 
# la Norme NF T90-354 d’avril 2016 et est complétée par ceux issus desdernières publications (nouveaux noms
#   ou mise en synonymie d’anciens noms…). La liste est établie d'après les travaux du groupe d'experts
# 'Taxinomie et Bioindication Diatomées' de la Forge logicielle SIE et validée en novembre 2020.

# Copyright 2020 UDAM
# Ce programme est un logiciel libre; vous pouvez le redistribuer ou le modifier
# suivant les termes de la GNU General Public License telle que publiee par la
# Free Software Foundation; soit la version 3 de la licence, soit (a votre gre)
# toute version ulterieure.
# Ce programme est distribue dans l'espoir qu'il sera utile, mais SANS AUCUNE
# GARANTIE; sans meme la garantie tacite de QUALITE MARCHANDE ou d'ADEQUATION A
# UN BUT PARTICULIER. Consultez la GNU General Public License pour plus de
# details.
# Vous devez avoir recu une copie de la GNU General Public License en meme temps
# que ce programme; si ce n'est pas le cas, consultez
# <http://www.gnu.org/licenses>.

## VERSION ----
indic  <- "IBD"
vIndic <- "v1.2.4"

## CHARGEMENT DES PACKAGES ----
dependencies <- c("dplyr", "tidyr")

loadDependencies <- function(dependencies) {
  suppressAll <- function(expr) {
    suppressPackageStartupMessages(suppressWarnings(expr))
  }

  lapply(dependencies,
         function(x)
         {
           suppressAll(library(x, character.only = TRUE))
         }
  )
  invisible()
}

loadDependencies(dependencies)

## IMPORT DES FICHIERS DE CONFIGURATION ----
IBD_params <- read.csv2("data/Donnes_traitees/IBD_params.csv", stringsAsFactors = FALSE) %>% 
  select_at(.vars = c("AFNOR", "SANDRE", paste0("CL", seq(7)), "Val.Ind."))

## DECLARATION DES FONCTIONS ----

## negation de %in%
`%not_in%` <- Negate(`%in%`)

#' Title Fonction permettant de transformer les abondances en pour mille
#'
#' @param data_entree : donnees d'inventaire
#'
#' @return : tableau avec le calcul des abondances relatives (pour 1000)
#' @export
#'
#' @examples : resultatsAx <- funAx(data_entree = data_entree)
#' 
funAx <- function(data_entree) {
  group_by(data_entree, CODE_OPERATION, CODE_TAXON) %>% ## regroupement des taxons avec codes identiques
    summarise(RESULTAT = sum(RESULTAT))           %>% ## somme des abondances
    group_by(CODE_OPERATION)                      %>% 
    mutate(TOTAL = sum(RESULTAT))                 %>% ## abondance total de l'operation
    ungroup()                                     %>% 
    mutate(Ax = 1000 * RESULTAT / TOTAL)          #%>% 
  # filter(Ax >= 7.5)
}


#' Title Fonction permettant de calculer les Fi (probabilite de presence des taxons dans les differentes classes)
#'
#' @param table : Tableau d'inventaire (avec les abondances relatives)
#' @param param : Tableau des profils des taxons
#'
#' @return : tableau avec le calcul des Fi
#' @export
#'
#' @examples : funFi(table = resultatsAx, param = IBD_params) 
#' 
funFi <- function(table, param) {
  left_join(x = table, y = param, 
            by = c("CODE_TAXON" = "AFNOR")) %>% 
    #filter(! is.na(SANDRE)) %>%  # ici retrait des codes taxons sans codes sandre
    select(-RESULTAT, -TOTAL, -SANDRE) %>% 
    gather(key = CLASSE, value = Px, 
           -CODE_OPERATION, -CODE_TAXON, -Ax, -Val.Ind.) %>% 
    group_by(CODE_OPERATION, CLASSE) %>% 
    summarise(Fi = sum(Ax * Px * Val.Ind.) / sum(Ax * Val.Ind.))
}

#' Title Fonction permettant de calculer B (valeurs d'ibd brutes, entre 0 et 7)
#'
#' @param table : Tableau de valeurs combinant les profils et les Fi
#'
#' @return
#' @export
#'
#' @examples resultatsAx  %>% 
#' funFi(table = ., param = IBD_params)  %>% 
#' funB(table = .) 
#' 
funB <- function(table) {
  weights <- data.frame(CLASSE = paste0("CL", seq(7)),
                        POIDS  = seq(7),
                        stringsAsFactors = FALSE)
  
  left_join(x = table, y = weights, by = "CLASSE") %>% 
    group_by(CODE_OPERATION) %>% 
    summarise(B = sum(Fi * POIDS)/sum(Fi))
}


#' Title Fonction permettant de calculer l'IBD a partir de B (passage des valeurs entre 0 et 20)
#'
#' @param table : Tableau contenant les valeurs de B retourne par la fonction funB
#'
#' @return
#' @export
#'
#' @examples
funIBD <- function(table) {
  mutate(table,
         IBD = case_when(B >= 0 & B<= 2  ~ 1,
                         B > 2  & B < 6  ~ funArrondi(4.75 * B - 8.5, 1), ## comme dans version 1.1.2
                         B >= 6 & B <= 7 ~ 20,
                         TRUE            ~ NA_real_))
}

#' Title Fonction permettant de generer les commentaires
#'
#' @param tableAx : Tableau des abondances relatives
#' @param param : Tableau des profils
#'
#' @return : Tableau des commentaires sur la validité de l'indice, le nombre de valves et le nombre de taxons pris en compte 
#' dans les notes d'indice
#' @export
#'
#' @examples funCommentaires(tableAx = resultatsAx, param = IBD_params)
funCommentaires <- function(tableAx, param) {
    # browser()
    ## le calcul des notes n'est effectué que pour les cas ou plus de 300 valves sont comptees -> retire ça ne semble pas trop plaire...
    ## on associe les info sur le nombre de valves avec l'indice (donne une idee de la "qualite" de l'indice)
    
    
    ibdValideTmp <- tableAx %>% 
        group_by(CODE_OPERATION) %>%
        mutate(COMMENTAIRES = if_else(condition = TOTAL < 400,
                                      true = paste0("Attention, moins de 400 individus au total (",TOTAL,")"),
                                      false = "")) %>% 
        mutate(CODE_PAR = "5856")
    
    ibdValide <- ibdValideTmp %>% 
        group_by(CODE_OPERATION) %>%
        #filter((CODE_TAXON %not_in% param$AFNOR)) %>% 
        mutate(COMMENTAIRES = if_else(condition = CODE_TAXON %not_in% param$AFNOR,
                                      true = paste0("Les taxons suivant, representant ",round(100 * sum(RESULTAT) / unique(TOTAL)),
                                                    "% des valves comptabilisees, n'ont pas ete pris en compte dans le calcul: ",
                                                    paste(CODE_TAXON, collapse = ", ")),
                                      false = COMMENTAIRES)) %>%
        select(CODE_OPERATION,COMMENTAIRES) %>% 
        distinct() %>% 
        mutate(CODE_PAR = "5856")
    
    
    
    ## commentaire sur les taxons non contributifs
    taxaNonContributifs <- 
        group_by(tableAx, CODE_OPERATION) %>% 
        mutate(S = n_distinct(CODE_TAXON)) %>% 
        filter((CODE_TAXON %not_in% param$AFNOR)) %>% ## selection des taxons non contrib 
        summarise(COMMENTAIRES = paste0(
            "Les taxons suivant, representant ",
            round(100 * n_distinct(CODE_TAXON) / unique(S)),
            "% des taxons de la liste floristique, n'ont pas ete pris en compte dans le calcul: ",
            paste(CODE_TAXON, collapse = ", ")
        )) %>% 
        mutate(CODE_PAR = "8060")
    
    ## commentaire sur les nombre de valves des taxons non contributifs
    nbValves <- 
        group_by(tableAx, CODE_OPERATION) %>% 
        filter((CODE_TAXON %not_in% param$AFNOR)) %>% 
        summarise(COMMENTAIRES = paste0(
            "Les taxons suivant, representant ",
            round(100 * sum(RESULTAT) / unique(TOTAL)),
            "% des valves comptabilisees, n'ont pas ete pris en compte dans le calcul: ",
            paste(CODE_TAXON, collapse = ", ")
        )) %>% 
        mutate(CODE_PAR = "8059")
    
    bind_rows(ibdValide, taxaNonContributifs, nbValves)
}


#' Title Fonction permettant de faire les arrondis a l'inferieur si 0 a 4 et au superieur si 5 a 9
#'
#' @param x : Valeur numerique
#' @param digits : nombre de chiffre apres la virgule
#'
#' @return
#' @export
#'
#' @examples
funArrondi <- function (x, digits = 0) {
  .local <- function(x, digits) {
    x <- x * (10^digits)
    ifelse(abs(x%%1 - 0.5) < .Machine$double.eps^0.5,
           ceiling(x)/(10^digits),
           round(x)/(10^digits))
  }

  if (is.data.frame(x))
    return(data.frame(lapply(x, .local, digits)))
  .local(x, digits)
}

#' Title Fonction initialisant le fichier de sortie
#'
#' @param data_entree : Tableau d'inventaire
#' @param paramsOut : Paremetres de sortie de l'indice. Dans ce cas il s'agit du nombre de taxon indicateur, 
#' du nombre de valves des taxons indicateurs et de la note d'indice sur 20
#' @param ... 
#'
#' @return : Tableau vide avec la structure des paramètres de sortie
#' @export
#'
#' @examples : 
#' paramsOut <- data.frame(CODE_PAR = c("8060", "8059", "5856"),LIB_PAR  = c("NbTaxonsIBDcontributifs", 
#'  "NbUniteDiatomique",
#'    "IndiceBioDiat"),
#'    stringsAsFactors = FALSE)
#'    
#' funSortie(data_entree = data_entree,paramsOut   = paramsOut, CODE_OPERATION, CODE_STATION, DATE) %>%
#' mutate(CODE_OPERATION = as.character(CODE_OPERATION),
#'  CODE_STATION   = as.character(CODE_STATION),
#'  DATE           = as.character(DATE))
#'  
funSortie <- function(data_entree, paramsOut, ...) {
  select(data_entree, ...) %>%
    distinct()             %>%
    (function(df) {
      df[rep(1:nrow(df), each = nrow(paramsOut)),] %>%
        as.tbl()
    })                     %>%
    mutate(CODE_PAR = rep(paramsOut$CODE_PAR, # creation des ligne des parametres a sortir
                          n() / nrow(paramsOut)),
           LIB_PAR  = rep(paramsOut$LIB_PAR,
                          n() / nrow(paramsOut)))
}

## Fonction permettant d'ecrire le fichier de sortie
#' Title
#'
#' @param indic : chaine de caractere avec le nom de l' indicateur
#' @param vIndic : chaine de caractere avec la version de l'indice
#' @param heure_debut : heure de lancement (date)
#' @param data_sortie : tableau d'inventaire (dataframe)
#' @param data_complementaire : tableau complementaire si necessaire
#' @param complementaire : Booleen pour dire si donnees complementaire ou pas
#' @param file : Nom du fichier de sortie
#' @param file_complementaire : nom du fichier de sortie pour les donnees complementaires
#'
#' @return : Tableau de sortie remplit avec les differents parametres de sortie de l'indice
#' @export
#'
#' @examples
funResult 		<- function(indic, vIndic, heure_debut,
                        data_sortie, data_complementaire, complementaire,
                        file, file_complementaire)
{
    # determination du temps de calcul
    heure_fin       <- Sys.time()
    heure_dif       <- heure_fin - heure_debut
    temps_execution <- paste0(round(heure_dif, 2),
                              attr(heure_dif, "units"))

    # creation du bandeau d'information
    etiquette <- paste(indic, vIndic, Sys.Date(),
                       "Temps d'execution :", temps_execution,
                       sep = ";")

    # sortie du bandeau d'information
    cat(paste0(etiquette, "\n"), file = file, sep = "")

    # sortie du fichier de sortie
    write.table(data_sortie, row.names = FALSE, quote = FALSE, sep = ";",
                file = file, append = TRUE)

    # Sortie complementaire
    if(complementaire)
    {
        if (file == "") {
            print("Fichier")
        }

        cat(paste0(etiquette, "\n"), file = file_complementaire, sep = "")
        write.table(data_complementaire, row.names = FALSE, quote = FALSE,
                    sep = ";", file = file_complementaire, append = TRUE)
    }

}# fin de la fonction funResult

## INITIALISATION DU TRAITEMENT ----
# Ne pas afficher les messages d'avis, ni transformation automatique en notation scientifique
options(warn = -1, scipen = 999)

# Recuperation du fichier d'entree
File           <- "data/Donnes_traitees/Test.txt"
complementaire <- FALSE

# Initialisation de l'heure
heure_debut <- Sys.time()

##  IMPORT DES FICHIERS ----
# Import du fichier d'entree
data_entree <- read.table(File, header = TRUE, sep = "\t",
                          stringsAsFactors = FALSE, quote = "\"",
                          colClasses = c(CODE_OPERATION = "character",
                                         CODE_STATION   = "character",
                                         CODE_TAXON     = "character")) %>% 
  filter(CODE_TAXON %in% IBD_params$AFNOR) # ici ajout de cette ligne pour faire comme dans la version 1.1.2 <!> Demander a S. Boutry quand se fait le calcul des abondances relatives
## attention, dans ce cas le nombre de taxon total est biaise car il ne s'agit que des taxons contributifs...

## INITIALISATION DU FICHIER DE SORTIE ----
paramsOut <- data.frame(CODE_PAR = c("8060", "8059", "5856"),
                        LIB_PAR  = c("NbTaxonsIBDcontributifs", 
                                     "NbUniteDiatomique",
                                     "IndiceBioDiat"),
                        stringsAsFactors = FALSE)

data_sortie <- funSortie(data_entree = data_entree,
                         paramsOut   = paramsOut,
                         CODE_OPERATION, CODE_STATION, DATE) %>%
  mutate(CODE_OPERATION = as.character(CODE_OPERATION),
         CODE_STATION   = as.character(CODE_STATION),
         DATE           = as.character(DATE))

## CALCUL DE L'INDICE ----
resultatsAx <- funAx(data_entree = data_entree)

resultatsTaxons <- resultatsAx                           %>% 
  left_join(x = ., 
            y = IBD_params,
            by = c("CODE_TAXON" = "AFNOR"))            %>% 
  #filter(! is.na(SANDRE))                              %>% ## attention ici !! pour transcode 2020 peut diff donc pas de retrait des code sandre a NA
  group_by(CODE_OPERATION)                             %>% 
  summarise(nbTaxa  = n_distinct(CODE_TAXON),
            nbUnite = sum(RESULTAT))                   %>% 
  gather(key = PAR, value = RESULTAT, -CODE_OPERATION) %>% 
  mutate(CODE_PAR = case_when(PAR == "nbTaxa"  ~ "8060",
                              PAR == "nbUnite" ~ "8059",
                              TRUE ~ NA_character_))   %>% 
  select(-PAR)

# construction du tableau de resultat
resultats <- resultatsAx  %>% 
  funFi(table = ., param = IBD_params)  %>% 
  funB(table = .)                         %>% 
  funIBD(table = .)                    %>% 
  transmute(CODE_OPERATION = CODE_OPERATION,
            CODE_PAR = "5856", # note d'indice
            RESULTAT = IBD) %>% 
  bind_rows(resultatsTaxons)

commentaires <- funCommentaires(tableAx = resultatsAx, param = IBD_params) 

## RESULTATS COMPLEMENTAIRES ----
if (complementaire) {
  data_complementaire <- NULL
} else {
  data_complementaire <- NULL
}

## SORTIE DES RESULTATS ----

data_sortie <- left_join(x  = data_sortie,
                         y  = resultats,
                         by = c("CODE_OPERATION", "CODE_PAR")) %>% 
    left_join(x = .,
              y = commentaires,
              by = c("CODE_OPERATION", "CODE_PAR"))            %>% 
    # mutate(RESULTAT = case_when(CODE_PAR  %in% c("8060", "8059") ~ 
    #                                 format(RESULTAT, digits = 0, nsmall = 0, trim = TRUE),
    #                             CODE_PAR == 5856 ~ 
    #                                 format(RESULTAT, digits = 1, nsmall = 1, trim = TRUE))) %>% 
    mutate(COMMENTAIRES = if_else(is.na(COMMENTAIRES), "", COMMENTAIRES)) # %>% ## si le comm est na avec mettre des ""
    # mutate(RESULTAT = if_else(CODE_PAR == 5856 & grepl("Moins de 400 individus au total", COMMENTAIRES),
    # "indice non calculable",
    # RESULTAT)) 
    ## attention : S'il n'y a aucun taxon indiciel alors indice non calculable
    # mutate(RESULTAT = if_else(CODE_PAR == 8060 & RESULTAT == 0,
    #                           "indice non calculable",
    #                           RESULTAT)) 

fichierResultat               <- paste0(indic, "_", vIndic, "_resultats.csv")
fichierResultatComplementaire <- paste0(indic, "_", vIndic,
                                        "_resultats_complementaires.csv")
funResult(indic               = indic,
          vIndic              = vIndic,
          heure_debut         = heure_debut,
          data_sortie         = data_sortie,
          data_complementaire = data_complementaire,
          complementaire      = complementaire,
          file                = fichierResultat,
          file_complementaire = fichierResultatComplementaire)
