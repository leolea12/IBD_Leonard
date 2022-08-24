# Type d'algorithme : IBD
# Auteur(s)         : UDAM
# Date              : 2021-06-16
# Version           : 1.2.4
# Interpreteur      : R version 4.0.2 (2020-06-22)
# Pre-requis        : Packages dplyr
# Fichiers lies     : 
# Commentaires 	  	: Indice Biologique Diatomees calcule selon la norme NF
# T90-354 (Avril 2016). Il repose sur 2009 especes de diatomees prises en compte
# dans le calcul correspondant a la base OMNIDIA 2017.

# Copyright 2020 
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
dependencies <- c("dplyr")

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

## FICHIERS DE CONFIGURATION ----

## DECLARATION DES FONCTIONS ----

## Fonction alternative a ifelse, plus simple
siNon <- function(test, yes, no) {
  if (test) {
    return(yes)
  } else {
    return(no)
  }
}

# Fonction permettant d'initialiser la sortie de tests
initResult <- function() {
  list(verif  = "ok",
       sortie = tibble(colonne = "", ligne = "", message = "")[0,])
}

# Fonction permettant de tester si le tableau est un data frame
funDataFrame <- function(Table, tableau) {
  if (!"data.frame" %in% class(Table)) {
    test <- list(verif = "ko",
                 sortie = tibble(colonne = "", ligne = "",
                                 message = paste0("Le tableau ", tableau,
                                                  " n'est pas un data.frame")))
  } else {
    test <- list(verif = "ok",
                 sortie = tibble(colonne = "", ligne = "", message = "")[0,])
  }

  test
}

# Fonction permettant de changer les valeurs de sortie/verif en fonction des
# resultats d'un test
funTest <- function(test, result) {
  result$verif <- siNon(test$verif == "ko", "ko", result$verif)

  result$sortie <- siNon(test$verif == "ko",
                         bind_rows(result$sortie,
                                   test$sortie),
                         result$sortie)

  result
}

# Fonction testant l'import des fichiers
funImport <- function(Table, result, empty = FALSE) {
  test <- siNon(empty,
                is.null(Table),
                any(is.null(Table), nrow(Table) == 0))

  out <- siNon(empty,
               "Le fichier doit etre au bon format",
               "Le fichier doit etre au bon format et non vide")
  if (test) {
    test <- list(verif  = "ko",
                 sortie = tibble(colonne = NA, ligne = NA,
                                 message = out))
  } else {
    test <- initResult()
  }

  funTest(test, result)
}

# Fonction permettant de tester la presence de champs obligatoires
funColonnes <- function(Table, cols, result) {
  # recupere le nom de l'objet passe a l'argument Table
  tableau <- deparse(substitute(Table))

  test <- funDataFrame(Table, tableau)

  if (test$verif == "ok") {

    test <- which(! cols %in% colnames(Table))

    if (length(test) > 0) {
      test <-
        list(verif = "ko",
             sortie = tibble(colonne = NA, ligne = NA,
                             message = paste0("Les champs obligatoires",
                                              " suivants ne sont pas ",
                                              "presents : ",
                                              paste(cols[test],
                                                    collapse = ", "))))
    } else {
      test <- list(verif  = "ok",
                   sortie = initResult())
    }
  }

  funTest(test, result)
}

# Fonction retournant un commentaire pour un test donné
funCommentaire <- function(test, message, Table) {
  test <- test[sapply(test, length) > 0]

  if (length(test) > 0) {
    return(
      list(verif = "ko",
           sortie = lapply(1:length(test),
                           function(i) {
                             tibble(colonne =
                                      paste0("Colonne ",
                                             names(test)[i]),
                                    ligne   =
                                      paste0("Ligne ",
                                             Table$ID[test[[i]]]),
                                    message = message)
                           }) %>%
             bind_rows()
      )
    )
  } else {
    return(list(verif = "ok",
                sortie = initResult()))
  }
}

# Fonction permettant de tester la presence de cellules vides
funVide <- function(Table, result) {
  tableau <- deparse(substitute(Table))

  test <- funDataFrame(Table, tableau)

  if (test$verif == "ok") {
    testEmpty <- function(x) {
      return(which(is.na(x) | x == ""))
    }

    test <- lapply(select(Table, -ID), testEmpty) %>%
      funCommentaire(test    = .,
                     message = "cellule vide",
                     Table   = Table)
  }

  funTest(test, result)
}

# Fonction permettant de tester la presence d'espace
funEspace <- function(Table, result) {
  tableau <- deparse(substitute(Table))

  test <- funDataFrame(Table, tableau)

  if (test$verif == "ok") {
    testSpace <- function(x) {
      which(grepl(" ", x))
    }

    test <- lapply(select(Table, -ID), testSpace) %>%
      funCommentaire(test    = .,
                     message = "cellule avec des caracteres 'espace'",
                     Table   = Table)
  }

  funTest(test, result)
}

# Fonction permettant de tester si les valeurs sont numeriques
funNumerique <- function(Table, tableau = NULL, result) {
  if (is.null(tableau)) tableau <- deparse(substitute(Table))

  test <- funDataFrame(Table, tableau)

  if (test$verif == "ok") {
    testNumeric <- function(x) {
      as.character(x)  %>%
        (function(i) {
          suppressWarnings(as.numeric(i))
        })           %>%
        is.na()      %>%
        which()
    }

    test <- lapply(select(Table, -ID), testNumeric) %>%
      funCommentaire(test    = .,
                     message = "cellule avec valeur non numerique",
                     Table   = Table)
  }

  funTest(test, result)
}

# Fonction permettant de tester si les nombres sont dans un intervalle donne
funIntervalle <- function(Table, mini, maxi, result) {
  tableau <- deparse(substitute(Table))

  test <- funDataFrame(Table, tableau)

  if (test$verif == "ok") {
    testInterval <- function(x, mini, maxi) {
      suppressWarnings(as.numeric(x)) %>%
        (function(x) {
          (x < mini | x > maxi) %>%
            which()
        })
    }

    test <- funNumerique(Table, tableau, initResult())       %>%
      funTest(test = lapply(select(Table, -ID),
                            testInterval, mini, maxi) %>%
                funCommentaire(test    = .,
                               message =
                                 paste0("cellule avec valeur ",
                                        "inferieure a ", mini,
                                        " ou superieure a ", maxi),
                               Table   = Table),
              result = .)
  }

  funTest(test, result)
}

# Fonction permettant de tester si les valeurs d'un champ sont plus grandes que
# celle d'un autre
funOrdre <- function(Table, lower, upper, result) {
    tableau = deparse(substitute(Table))

    test <- funDataFrame(Table, tableau)

    if (test$verif == "ok") {
        testOrder <- function(Table, lower, upper) {
            mutate_at(.tbl = Table,
                      .vars = c(lower, upper),
                      .funs = function(x){
                          suppressWarnings(as.numeric(x))
                      }) %>%
                mutate(.data = ., delta = .[[upper]] - .[[lower]]) %>%
                filter(delta < 0) %>%
                (function(df) {pull(df, ID) - 1})

        }

        funTest(test = list("NA" = testOrder(Table,
                                             lower = lower, upper = upper)) %>%
                    funCommentaire(test = .,
                                   message =
                                       paste0(lower, " plus grand que ", upper),
                                   Table = Table),
                result = result)
    }
}

# Fonction permettant de tester si les nombres sont entiers
funEntier <- function(Table, result) {
  tableau <- deparse(substitute(Table))

  test <- funDataFrame(Table, tableau)

  if (test$verif == "ok") {
    testInteger <-   function(x) {
      suppressWarnings(as.numeric(x)) %>%
        (function(x) abs(x - round(x)) >
           .Machine$double.eps^0.5) %>%
        which()                       %>%
        return()
    }

    test <- funNumerique(Table, tableau, initResult())         %>%
      funTest(test = lapply(select(Table, -ID), testInteger) %>%
                funCommentaire(test    = .,
                               message = paste0("cellule avec valeur ",
                                                "non entiere"),
                               Table   = Table),
              result = .)

  }

  funTest(test, result)
}

# Fonction permettant de tester si les nombres sont positifs
funPositif <- function(Table, result) {
  tableau <- deparse(substitute(Table))

  test <- funDataFrame(Table, tableau)

  if (test$verif == "ok") {
    testPositive <- function(x) {
      x <- suppressWarnings(as.numeric(x))

      which(x != abs(x))
    }

    test <- funNumerique(Table, tableau, initResult()) %>%
      funTest(test = lapply(select(Table, -ID), testPositive) %>%
                funCommentaire(test    = .,
                               message = "cellule avec valeur non positive",
                               Table   = Table),
              result = .)
  }

  funTest(test, result)
}

# Fonction permettant de tester le format des dates
funDate <- function(Table, dateFormat, result) {
  tableau <- deparse(substitute(Table))

  test <- funDataFrame(Table, tableau)

  if (test$verif == "ok") {
    testDate <- function(x, dateFormat) {
      suppressWarnings(as.Date(x, format = dateFormat)) %>%
        is.na()                                         %>%
        which()
    }

    test <- lapply(select(Table, -ID),
                   testDate, dateFormat = dateFormat) %>%
      funCommentaire(test    = .,
                     message = "cellule avec format de date non valide",
                     Table   = Table)
  }

  funTest(test, result)
}

# Fonction permettant de tester des codes
funCodes <- function(Table, codes, codeType, result) {
  tableau <- deparse(substitute(Table))

  test <- funDataFrame(Table, tableau)

  if (test$verif == "ok") {
    testCodes <- function(x, codes) {
      return(which(! x %in% codes))
    }

    test <- lapply(select(Table, -ID), testCodes, codes) %>%
      funCommentaire(test    = .,
                     message = paste0("cellule avec code ",
                                      codeType, " non valide"),
                     Table   = Table)
  }

  funTest(test, result)
}

# Fonction permettant de nettoyer les sorties
funSortie <- function(result) {
  if (result$verif == "ok") {
    ""
  } else {
    sortie <- result$sortie

    msgLevels <- c("vide", "espace",
                   "numerique", "entiere", "positive",
                   "date", "code")

    messages <- tibble(message = unique(sortie$message),
                       level   = NA)

    for (i in 1:length(msgLevels)) {
      i2 <- which(grepl(pattern = msgLevels[i], x = messages$message))

      messages$level[i2] <- i
    }

    messages <- arrange(messages, level) %>%
      mutate(message = factor(message, levels = message))

    mutate(sortie,
           message = factor(message,
                            levels = messages$message)) %>%
      group_by(colonne, ligne)                          %>%
      summarise(message = message[as.numeric(message) ==
                                    min(as.numeric(message))][1])
  }
}

## Fonction permettant de verifier la validite de l'ensemble des tests
funValid <- function(resultats) {
  sapply(resultats,
         function(x) {
           x$verif == "ko"
         }) %>%
    (function(x) siNon(any(x), "ko", "ok"))
}

## Fonction permettant d'ecrire le fichier de sortie
funResult <- function(indic, vIndic, heure_debut, valid, sortie, file) {
    # determination du temps de calcul
    heure_fin <- Sys.time()
    heure_dif <- heure_fin - heure_debut
    temps_execution <- paste(round(heure_dif, 2),
                             attr(heure_dif, "units"),
                             sep = "")

    # creation du bandeau d'information
    etiquette <- paste(indic, vIndic, Sys.Date(),
                       "Temps d'execution :", temps_execution)

    # information de validation ou non des donnees d'entree
    print(valid)

    cat(paste0(etiquette, "\n"), file = file, sep = "")

    for (i in names(sortie)) {
        cat(paste0("Fichier de donnees ", i, ":\n"), file = file,
            append = TRUE)
        write.table(sortie[[i]], file = file, sep = ";",
                    col.names = FALSE, row.names = FALSE, quote = FALSE,
                    append = TRUE)
        cat("\n", file = file, append = TRUE)
    }

}

## INITIALISATION DU TRAITEMENT ----

# Ne pas afficher les messages d'avis
options(warn = -1)

# Recuperation du fichier d'entree
File <- "IBD_entree_op100.txt"

# Initialisation de l'heure
heure_debut <- Sys.time()

### IMPORT DES FICHIERS ----

# Import du fichier d'entree
data_entree <- NULL
data_entree <- read.table(File, header = TRUE, sep = "\t", quote = "\"",
                          stringsAsFactors = FALSE,
                          colClasses = c(CODE_OPERATION = "character",
                                         CODE_STATION   = "character")) %>%
  mutate(ID = seq(n()) + 1)

## VALIDATION DES DONNEES ----

resultat <- initResult() %>%
  funImport(
    Table  = data_entree,
    empty  = FALSE,
    result = .)          %>%
  funColonnes(
    Table  = data_entree,
    cols   = c("CODE_OPERATION","CODE_STATION",
               "DATE", "CODE_TAXON", "RESULTAT"),
    result = .)

if (resultat$verif == "ok") {
  resultat <- resultat %>%
    funVide(
      Table    = select(data_entree,
                        ID, CODE_OPERATION, CODE_TAXON, RESULTAT),
      result   = .)    %>%
    funEspace(
      Table    = select(data_entree,
                        ID, CODE_TAXON, RESULTAT),
      result   = .)    %>%
    funEntier(
      Table   = select(data_entree,
                       ID, RESULTAT),
      result  = .) %>%
    funPositif(
      Table  = select(data_entree,
                      ID, RESULTAT),
      result = .)
}

# Parametre de succes/echec de la validation
valid <- funValid(resultats = list(resultat))

# SORTIE DU RAPPORT D'ERREUR ----
sortie <- list(faunistiques     = funSortie(resultat))

outputFile <- paste0(indic, "_", vIndic, "_rapport_erreur.csv")
funResult(indic       = indic,
          vIndic      = vIndic,
          heure_debut = heure_debut,
          valid       = valid,
          sortie      = sortie,
          file        = outputFile)
