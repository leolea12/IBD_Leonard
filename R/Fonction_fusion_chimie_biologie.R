
#' f_fusion, une fonction qui associe la chimie à la biologie
#'
#' @param data_chimie correspond à la table des paramètres physico-cimique
#' des stations d'échantillonnage de France pour une année donnée, 12
#' prélèvement par station par année
#' 
#' @param data_biologie correspond à la liste floristique
#' des stations d'échantillonnage de France pour une année donnée, 1 prélèvement
#' par station par année
#'
#' @return Renvoie un tableau de type tibble avec pour chaque prélèvement 
#' biologique d'une station, la médiane des paramètres physico-chimique 
#' correspondant entre 1 mois avant et 15 jours après. 
#' @export
#'
#' @examples

f_fusion <- function(data_chimie, data_biologie){
  
  # Recuperer uniquement les stations chimiquespresentant un prelevement
  # diatomique
  Chimie_Diatom <- data_chimie %>% 
    subset(CODE_STATION %in% c(unique(data_biologie$CODE_STATION)))
  
  # Construire un tibble qu'on va remplir ensuite, choix d'une date random
  # pour spécifier le type de colonne 
  test <-  as.tibble(data.frame(CODE_STATION = "chr",
                              DATE = as.Date("02/27/92", "%m/%d/%y"),
                              chimie = NA,
                              flore = NA))
  
  # Nombre d'iterations a specifier pour la barre de chargement
  n_iter <- length(data_biologie$CODE_STATION)
  
  # Demmarrage de la barre de progression
  pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                         total = n_iter,
                         complete = "=",   
                         incomplete = "-", 
                         current = ">",    
                         clear = FALSE,    
                         width = 100)
  
  for(i in data_biologie$CODE_STATION){
    
    # Afficher le compteur de la barre
    pb$tick()
  
    # Recuperer la date du prelevement diatomique au format character
    date_diatom <-  data_biologie %>% filter(CODE_STATION == i) %>% pull(DATE)
    
    # Faire correspondre chimie et biologie (1 mois avant ou 2 mois après)
    Coresp = Chimie_Diatom %>% 
      filter(CODE_STATION == i) %>% 
      rowwise() %>% 
      mutate(Dist_time = if_else(between(DATE, # 1 mois avant, 2 mois apres
                                         date_diatom[1] - 60, 
                                         date_diatom[1] + 15) == TRUE, 
                                 "ok", "no_ok")) %>%
      filter(Dist_time == "ok") %>% 
      unnest() %>% 
      group_by(Code_parametre) %>% 
      mutate(Mediane = median(Concentration, na.rm = TRUE),
             nb_prel = length(Concentration)) %>% 
      distinct(Code_parametre, .keep_all = T) %>% 
      select(-DATE, -Concentration, -Dist_time, -Nom_unite_mesure) %>%
      ungroup() %>% 
      nest_by(CODE_STATION) %>% 
      left_join(data_biologie %>% # Fusion des deux tables 
                  filter(CODE_STATION == i), by = "CODE_STATION") %>% 
      select(CODE_STATION, DATE, chimie = data.x, flore = data.y)
  
    test <-  test %>% add_row(Coresp)}
  
  return(test[-1,])
}
























