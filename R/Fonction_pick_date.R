
# Fonction pick_date pour selectionner la base de donnees chimique souhaitee


#' Pick_date, une fonction qui applique f_fusion pour une année donnée 
#'
#' @param Date 
#'
#' @return Renvoie un fichier xlsx contenant la liste floristique d'une année 
#' donnée sur une feuille et la chimie associée sur l'autre
#' @export
#'
#' @examples

Pick_date <-  function(Date){
  
  if(Date == 2007){
    
    # Fusion NAIADES/PANDORE

     Chimie <- Chimie_pandore %>% filter(year(DATE) == Date) %>% 
      bind_rows((as.tibble(fread("data/Donnees_de_base/analyses_2007.csv"))%>% 
                                                                   select("CODE_STATION" = CdStationMesureEauxSurface, "Code_Prelevement" = CdPrelevement, 
                                                                          "DATE" = DatePrel, "Code_parametre" = CdParametre, "Nom_parametre" = LbLongParamètre, 
                                                                          "Concentration" = RsAna, "Code_Unite_mesure" = CdUniteMesure, "Nom_unite_mesure" = SymUniteMesure, CdSupport) %>% 
                   mutate(CODE_STATION = str_remove(CODE_STATION, "^0+")) %>%                      
                   subset(CODE_STATION %in% c(unique(Diatom3$CODE_STATION))) %>% 
                  filter(Code_parametre == 1302 | Code_parametre == 1304 | Code_parametre == 1311
                                                                          | Code_parametre == 1314 | Code_parametre == 1335| Code_parametre == 1433
                                                                          | Code_parametre == 1340, CdSupport == 3) %>% 
                                                                   mutate(DATE = as.Date(DATE, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))) %>% 
                                                                   mutate(Concentration = as.numeric(Concentration)))) %>% 
      distinct() %>% left_join(sqlQuery(channel, "SELECT cd_unite, lb_unite FROM sandre_unite") %>% 
      rename(Code_Unite_mesure = cd_unite, Nom_unite_mesure = lb_unite) %>% 
        mutate(Code_Unite_mesure = as.character(Code_Unite_mesure)), by = "Code_Unite_mesure") %>% 
      select(-Nom_unite_mesure.x, -CdSupport) %>% 
      rename(Nom_unite_mesure = Nom_unite_mesure.y) %>% 
      group_nest(CODE_STATION, DATE)
  
    Diatom3 %>% group_nest(CODE_STATION, DATE) %>% 
      filter(year(DATE) == 2007, between(month(DATE), 5,9)) -> Diatom_2007
    
    Tab <- f_fusion(data_chimie = Chimie, data_biologie = Diatom_2007)
    
    df1 <- Tab %>% unnest(chimie) %>% select(-flore)
    
    df2 <- Tab %>% unnest(flore) %>% select(-chimie)
    
    dataset_names <- list('chimie' = df1, 'flore' = df2)
    
    write.xlsx(dataset_names, file = 'data/Donnees_utilisables/2007_traitee.xlsx')}
  
  if (Date == 2008){
    
    channel=odbcConnect("pandore")
    
    Chimie <- Chimie_pandore %>% filter(year(DATE) == Date) %>% 
      bind_rows((as.tibble(fread("data/Donnees_de_base/analyses_2008.csv"))%>% 
                   select("CODE_STATION" = CdStationMesureEauxSurface, "Code_Prelevement" = CdPrelevement, 
                          "DATE" = DatePrel, "Code_parametre" = CdParametre, "Nom_parametre" = LbLongParamètre, 
                          "Concentration" = RsAna, "Code_Unite_mesure" = CdUniteMesure, "Nom_unite_mesure" = SymUniteMesure, CdSupport) %>% 
                   mutate(CODE_STATION = str_remove(CODE_STATION, "^0+")) %>% 
                   subset(CODE_STATION %in% c(unique(Diatom3$CODE_STATION))) %>% 
                   filter(Code_parametre == 1302 | Code_parametre == 1304 | Code_parametre == 1311
                          | Code_parametre == 1314 | Code_parametre == 1335| Code_parametre == 1433
                          | Code_parametre == 1340, CdSupport == 3) %>% 
                   mutate(DATE = as.Date(DATE, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))) %>% 
                   mutate(Concentration = as.numeric(Concentration)))) %>% 
      distinct() %>% left_join(sqlQuery(channel, "SELECT cd_unite, lb_unite FROM sandre_unite") %>% 
                                 rename(Code_Unite_mesure = cd_unite, Nom_unite_mesure = lb_unite) %>% 
                                 mutate(Code_Unite_mesure = as.character(Code_Unite_mesure)), by = "Code_Unite_mesure") %>% 
      select(-Nom_unite_mesure.x, -CdSupport) %>% 
      rename(Nom_unite_mesure = Nom_unite_mesure.y) %>% 
      group_nest(CODE_STATION, DATE)
    
    Diatom3 %>% group_nest(CODE_STATION, DATE) %>% 
      filter(year(DATE) == 2008, between(month(DATE), 5,9)) -> Diatom_2008
    
    Tab <- f_fusion(data_chimie = Chimie, data_biologie = Diatom_2008)
    
    df1 <- Tab %>% unnest(chimie) %>% select(-flore)
    
    df2 <- Tab %>% unnest(flore) %>% select(-chimie)
    
    dataset_names <- list('chimie' = df1, 'flore' = df2)
    
    write.xlsx(dataset_names, file = 'data/Donnees_utilisables/2008_traitee.xlsx')}

  if (Date == 2009){
    
    channel=odbcConnect("pandore")
    
    Chimie <- Chimie_pandore %>% filter(year(DATE) == Date) %>% 
      bind_rows((as.tibble(fread("data/Donnees_de_base/analyses_2009.csv"))%>% 
                   select("CODE_STATION" = CdStationMesureEauxSurface, "Code_Prelevement" = CdPrelevement, 
                          "DATE" = DatePrel, "Code_parametre" = CdParametre, "Nom_parametre" = LbLongParamètre, 
                          "Concentration" = RsAna, "Code_Unite_mesure" = CdUniteMesure, "Nom_unite_mesure" = SymUniteMesure, CdSupport) %>% 
                   mutate(CODE_STATION = str_remove(CODE_STATION, "^0+")) %>%
                   subset(CODE_STATION %in% c(unique(Diatom3$CODE_STATION))) %>% 
                   filter(Code_parametre == 1302 | Code_parametre == 1304 | Code_parametre == 1311
                          | Code_parametre == 1314 | Code_parametre == 1335| Code_parametre == 1433
                          | Code_parametre == 1340, CdSupport == 3) %>% 
                   mutate(DATE = as.Date(DATE, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))) %>% 
                   mutate(Concentration = as.numeric(Concentration)))) %>% 
      distinct() %>% left_join(sqlQuery(channel, "SELECT cd_unite, lb_unite FROM sandre_unite") %>% 
                                 rename(Code_Unite_mesure = cd_unite, Nom_unite_mesure = lb_unite) %>% 
                                 mutate(Code_Unite_mesure = as.character(Code_Unite_mesure)), by = "Code_Unite_mesure") %>% 
      select(-Nom_unite_mesure.x, -CdSupport) %>% 
      rename(Nom_unite_mesure = Nom_unite_mesure.y) %>% 
      group_nest(CODE_STATION, DATE)
    
    Diatom3 %>% group_nest(CODE_STATION, DATE) %>% 
      filter(year(DATE) == 2009, between(month(DATE), 5,9)) -> Diatom_2009
    
    Tab <- f_fusion(data_chimie = Chimie, data_biologie = Diatom_2009)
    
    df1 <- Tab %>% unnest(chimie) %>% select(-flore)
    
    df2 <- Tab %>% unnest(flore) %>% select(-chimie)
    
    dataset_names <- list('chimie' = df1, 'flore' = df2)
    
    write.xlsx(dataset_names, file = 'data/Donnees_utilisables/2009_traitee.xlsx')}
  
  if (Date == 2010){
    
    channel=odbcConnect("pandore")
    
    Chimie <- Chimie_pandore %>% filter(year(DATE) == Date) %>% 
      bind_rows((as.tibble(fread("data/Donnees_de_base/analyses_2010.csv"))%>% 
                   select("CODE_STATION" = CdStationMesureEauxSurface, "Code_Prelevement" = CdPrelevement, 
                          "DATE" = DatePrel, "Code_parametre" = CdParametre, "Nom_parametre" = LbLongParamètre, 
                          "Concentration" = RsAna, "Code_Unite_mesure" = CdUniteMesure, "Nom_unite_mesure" = SymUniteMesure, CdSupport) %>% 
                   mutate(CODE_STATION = str_remove(CODE_STATION, "^0+")) %>%
                   subset(CODE_STATION %in% c(unique(Diatom3$CODE_STATION))) %>% 
                   filter(Code_parametre == 1302 | Code_parametre == 1304 | Code_parametre == 1311
                          | Code_parametre == 1314 | Code_parametre == 1335| Code_parametre == 1433
                          | Code_parametre == 1340, CdSupport == 3) %>% 
                   mutate(DATE = as.Date(DATE, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))) %>% 
                   mutate(Concentration = as.numeric(Concentration)))) %>% 
      distinct() %>% left_join(sqlQuery(channel, "SELECT cd_unite, lb_unite FROM sandre_unite") %>% 
                                 rename(Code_Unite_mesure = cd_unite, Nom_unite_mesure = lb_unite) %>% 
                                 mutate(Code_Unite_mesure = as.character(Code_Unite_mesure)), by = "Code_Unite_mesure") %>% 
      select(-Nom_unite_mesure.x, -CdSupport) %>% 
      rename(Nom_unite_mesure = Nom_unite_mesure.y) %>% 
      group_nest(CODE_STATION, DATE)
    
    Diatom3 %>% group_nest(CODE_STATION, DATE) %>% 
      filter(year(DATE) == 2010, between(month(DATE), 5,9)) -> Diatom_2010
    
    Tab <- f_fusion(data_chimie = Chimie, data_biologie = Diatom_2010)
    
    df1 <- Tab %>% unnest(chimie) %>% select(-flore)
    
    df2 <- Tab %>% unnest(flore) %>% select(-chimie)
    
    dataset_names <- list('chimie' = df1, 'flore' = df2)
    
    write.xlsx(dataset_names, file = 'data/Donnees_utilisables/2010_traitee.xlsx')}
  
  if (Date == 2011){
    
    channel=odbcConnect("pandore")
    
    Chimie <- Chimie_pandore %>% filter(year(DATE) == Date) %>% 
      bind_rows((as.tibble(fread("data/Donnees_de_base/analyses_2011.csv"))%>% 
                   select("CODE_STATION" = CdStationMesureEauxSurface, "Code_Prelevement" = CdPrelevement, 
                          "DATE" = DatePrel, "Code_parametre" = CdParametre, "Nom_parametre" = LbLongParamètre, 
                          "Concentration" = RsAna, "Code_Unite_mesure" = CdUniteMesure, "Nom_unite_mesure" = SymUniteMesure, CdSupport) %>% 
                   mutate(CODE_STATION = str_remove(CODE_STATION, "^0+")) %>%
                   subset(CODE_STATION %in% c(unique(Diatom3$CODE_STATION))) %>% 
                   filter(Code_parametre == 1302 | Code_parametre == 1304 | Code_parametre == 1311
                          | Code_parametre == 1314 | Code_parametre == 1335| Code_parametre == 1433
                          | Code_parametre == 1340, CdSupport == 3) %>% 
                   mutate(DATE = as.Date(DATE, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))) %>% 
                   mutate(Concentration = as.numeric(Concentration)))) %>% 
      distinct() %>% left_join(sqlQuery(channel, "SELECT cd_unite, lb_unite FROM sandre_unite") %>% 
                                 rename(Code_Unite_mesure = cd_unite, Nom_unite_mesure = lb_unite) %>% 
                                 mutate(Code_Unite_mesure = as.character(Code_Unite_mesure)), by = "Code_Unite_mesure") %>% 
      select(-Nom_unite_mesure.x, -CdSupport) %>% 
      rename(Nom_unite_mesure = Nom_unite_mesure.y) %>% 
      group_nest(CODE_STATION, DATE)
    
    Diatom3 %>% group_nest(CODE_STATION, DATE) %>% 
      filter(year(DATE) == 2011, between(month(DATE), 5,9)) -> Diatom_2011
    
    Tab <- f_fusion(data_chimie = Chimie, data_biologie = Diatom_2011)
    
    df1 <- Tab %>% unnest(chimie) %>% select(-flore)
    
    df2 <- Tab %>% unnest(flore) %>% select(-chimie)
    
    dataset_names <- list('chimie' = df1, 'flore' = df2)
    
    write.xlsx(dataset_names, file = 'data/Donnees_utilisables/2011_traitee.xlsx')}
  
  if (Date == 2012){
    
    channel=odbcConnect("pandore")
    
    Chimie <- Chimie_pandore %>% filter(year(DATE) == Date) %>% 
      bind_rows((as.tibble(fread("data/Donnees_de_base/analyses_2012.csv"))%>% 
                   select("CODE_STATION" = CdStationMesureEauxSurface, "Code_Prelevement" = CdPrelevement, 
                          "DATE" = DatePrel, "Code_parametre" = CdParametre, "Nom_parametre" = LbLongParamètre, 
                          "Concentration" = RsAna, "Code_Unite_mesure" = CdUniteMesure, "Nom_unite_mesure" = SymUniteMesure, CdSupport) %>% 
                   mutate(CODE_STATION = str_remove(CODE_STATION, "^0+")) %>%
                   subset(CODE_STATION %in% c(unique(Diatom3$CODE_STATION))) %>% 
                   filter(Code_parametre == 1302 | Code_parametre == 1304 | Code_parametre == 1311
                          | Code_parametre == 1314 | Code_parametre == 1335| Code_parametre == 1433
                          | Code_parametre == 1340, CdSupport == 3) %>% 
                   mutate(DATE = as.Date(DATE, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))) %>% 
                   mutate(Concentration = as.numeric(Concentration)))) %>% 
      distinct() %>% left_join(sqlQuery(channel, "SELECT cd_unite, lb_unite FROM sandre_unite") %>% 
                                 rename(Code_Unite_mesure = cd_unite, Nom_unite_mesure = lb_unite) %>% 
                                 mutate(Code_Unite_mesure = as.character(Code_Unite_mesure)), by = "Code_Unite_mesure") %>% 
      select(-Nom_unite_mesure.x, -CdSupport) %>% 
      rename(Nom_unite_mesure = Nom_unite_mesure.y) %>% 
      group_nest(CODE_STATION, DATE)
    
    Diatom3 %>% group_nest(CODE_STATION, DATE) %>% 
      filter(year(DATE) == 2012, between(month(DATE), 5,9)) -> Diatom_2012
    
    Tab <- f_fusion(data_chimie = Chimie, data_biologie = Diatom_2012)
    
    df1 <- Tab %>% unnest(chimie) %>% select(-flore)
    
    df2 <- Tab %>% unnest(flore) %>% select(-chimie)
    
    dataset_names <- list('chimie' = df1, 'flore' = df2)
    
    write.xlsx(dataset_names, file = 'data/Donnees_utilisables/2012_traitee.xlsx')}
 
  if (Date == 2013){
    
    channel=odbcConnect("pandore")
    
    Chimie <- Chimie_pandore %>% filter(year(DATE) == Date) %>% 
      bind_rows((as.tibble(fread("data/Donnees_de_base/analyses_2013.csv"))%>% 
                   select("CODE_STATION" = CdStationMesureEauxSurface, "Code_Prelevement" = CdPrelevement, 
                          "DATE" = DatePrel, "Code_parametre" = CdParametre, "Nom_parametre" = LbLongParamètre, 
                          "Concentration" = RsAna, "Code_Unite_mesure" = CdUniteMesure, "Nom_unite_mesure" = SymUniteMesure, CdSupport) %>% 
                   mutate(CODE_STATION = str_remove(CODE_STATION, "^0+")) %>%
                   subset(CODE_STATION %in% c(unique(Diatom3$CODE_STATION))) %>% 
                   filter(Code_parametre == 1302 | Code_parametre == 1304 | Code_parametre == 1311
                          | Code_parametre == 1314 | Code_parametre == 1335| Code_parametre == 1433
                          | Code_parametre == 1340, CdSupport == 3) %>% 
                   mutate(DATE = as.Date(DATE, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))) %>% 
                   mutate(Concentration = as.numeric(Concentration)))) %>% 
      distinct() %>% left_join(sqlQuery(channel, "SELECT cd_unite, lb_unite FROM sandre_unite") %>% 
                                 rename(Code_Unite_mesure = cd_unite, Nom_unite_mesure = lb_unite) %>% 
                                 mutate(Code_Unite_mesure = as.character(Code_Unite_mesure)), by = "Code_Unite_mesure") %>% 
      select(-Nom_unite_mesure.x, -CdSupport) %>% 
      rename(Nom_unite_mesure = Nom_unite_mesure.y) %>% 
      group_nest(CODE_STATION, DATE)
    
    Diatom3 %>% group_nest(CODE_STATION, DATE) %>% 
      filter(year(DATE) == 2013, between(month(DATE), 5,9)) -> Diatom_2013
    
    Tab <- f_fusion(data_chimie = Chimie, data_biologie = Diatom_2013)
    
    df1 <- Tab %>% unnest(chimie) %>% select(-flore)
    
    df2 <- Tab %>% unnest(flore) %>% select(-chimie)
    
    dataset_names <- list('chimie' = df1, 'flore' = df2)
    
    write.xlsx(dataset_names, file = 'data/Donnees_utilisables/2013_traitee.xlsx')}
  
  if (Date == 2014){
    
    channel=odbcConnect("pandore")
    
    Chimie <- Chimie_pandore %>% filter(year(DATE) == Date) %>% 
      bind_rows((as.tibble(fread("data/Donnees_de_base/analyses_2014.csv"))%>% 
                   select("CODE_STATION" = CdStationMesureEauxSurface, "Code_Prelevement" = CdPrelevement, 
                          "DATE" = DatePrel, "Code_parametre" = CdParametre, "Nom_parametre" = LbLongParamètre, 
                          "Concentration" = RsAna, "Code_Unite_mesure" = CdUniteMesure, "Nom_unite_mesure" = SymUniteMesure, CdSupport) %>% 
                   mutate(CODE_STATION = str_remove(CODE_STATION, "^0+")) %>%
                   subset(CODE_STATION %in% c(unique(Diatom3$CODE_STATION))) %>% 
                   filter(Code_parametre == 1302 | Code_parametre == 1304 | Code_parametre == 1311
                          | Code_parametre == 1314 | Code_parametre == 1335| Code_parametre == 1433
                          | Code_parametre == 1340, CdSupport == 3) %>% 
                   mutate(DATE = as.Date(DATE, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))) %>% 
                   mutate(Concentration = as.numeric(Concentration)))) %>% 
      distinct() %>% left_join(sqlQuery(channel, "SELECT cd_unite, lb_unite FROM sandre_unite") %>% 
                                 rename(Code_Unite_mesure = cd_unite, Nom_unite_mesure = lb_unite) %>% 
                                 mutate(Code_Unite_mesure = as.character(Code_Unite_mesure)), by = "Code_Unite_mesure") %>% 
      select(-Nom_unite_mesure.x, -CdSupport) %>% 
      rename(Nom_unite_mesure = Nom_unite_mesure.y) %>% 
      group_nest(CODE_STATION, DATE)
    
    Diatom3 %>% group_nest(CODE_STATION, DATE) %>% 
      filter(year(DATE) == 2014, between(month(DATE), 5,9)) -> Diatom_2014
    
   Tab <- f_fusion(data_chimie = Chimie, data_biologie = Diatom_2014)
   
   df1 <- Tab %>% unnest(chimie) %>% select(-flore)
   
   df2 <- Tab %>% unnest(flore) %>% select(-chimie)
   
   dataset_names <- list('chimie' = df1, 'flore' = df2)
   
   write.xlsx(dataset_names, file = 'data/Donnees_utilisables/2014_traitee.xlsx')}
  
  if (Date == 2015){
    
    channel=odbcConnect("pandore")
    
    Chimie <- Chimie_pandore %>% filter(year(DATE) == Date) %>% 
      bind_rows((as.tibble(fread("data/Donnees_de_base/analyses_2015.csv"))%>% 
                   select("CODE_STATION" = CdStationMesureEauxSurface, "Code_Prelevement" = CdPrelevement, 
                          "DATE" = DatePrel, "Code_parametre" = CdParametre, "Nom_parametre" = LbLongParamètre, 
                          "Concentration" = RsAna, "Code_Unite_mesure" = CdUniteMesure, "Nom_unite_mesure" = SymUniteMesure, CdSupport) %>% 
                   mutate(CODE_STATION = str_remove(CODE_STATION, "^0+")) %>%
                   subset(CODE_STATION %in% c(unique(Diatom3$CODE_STATION))) %>% 
                   filter(Code_parametre == 1302 | Code_parametre == 1304 | Code_parametre == 1311
                          | Code_parametre == 1314 | Code_parametre == 1335| Code_parametre == 1433
                          | Code_parametre == 1340, CdSupport == 3) %>% 
                   mutate(DATE = as.Date(DATE, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))) %>% 
                   mutate(Concentration = as.numeric(Concentration)))) %>% 
      distinct() %>% left_join(sqlQuery(channel, "SELECT cd_unite, lb_unite FROM sandre_unite") %>% 
                                 rename(Code_Unite_mesure = cd_unite, Nom_unite_mesure = lb_unite) %>% 
                                 mutate(Code_Unite_mesure = as.character(Code_Unite_mesure)), by = "Code_Unite_mesure") %>% 
      select(-Nom_unite_mesure.x, -CdSupport) %>% 
      rename(Nom_unite_mesure = Nom_unite_mesure.y) %>% 
      group_nest(CODE_STATION, DATE)
    
    Diatom3 %>% group_nest(CODE_STATION, DATE) %>% 
      filter(year(DATE) == 2015, between(month(DATE), 5,9)) -> Diatom_2015
    
    Tab <- f_fusion(data_chimie = Chimie, data_biologie = Diatom_2015)
    
    df1 <- Tab %>% unnest(chimie) %>% select(-flore)
    
    df2 <- Tab %>% unnest(flore) %>% select(-chimie)
    
    dataset_names <- list('chimie' = df1, 'flore' = df2)
    
    write.xlsx(dataset_names, file = 'data/Donnees_utilisables/2015_traitee.xlsx')}
  
  if (Date == 2016){
    
    channel=odbcConnect("pandore")
    
    Chimie <- Chimie_pandore %>% filter(year(DATE) == Date) %>% 
      bind_rows((as.tibble(fread("data/Donnees_de_base/analyses_2016.csv")) %>% 
                   select("CODE_STATION" = CdStationMesureEauxSurface, "Code_Prelevement" = CdPrelevement, 
                          "DATE" = DatePrel, "Code_parametre" = CdParametre, "Nom_parametre" = LbLongParamètre, 
                          "Concentration" = RsAna, "Code_Unite_mesure" = CdUniteMesure, "Nom_unite_mesure" = SymUniteMesure, CdSupport) %>% 
                   mutate(CODE_STATION = str_remove(CODE_STATION, "^0+")) %>% 
                   subset(CODE_STATION %in% c(unique(Diatom3$CODE_STATION))) %>% 
                   filter(Code_parametre == 1302 | Code_parametre == 1304 | Code_parametre == 1311
                          | Code_parametre == 1314 | Code_parametre == 1335| Code_parametre == 1433
                          | Code_parametre == 1340, CdSupport == 3) %>% 
                   mutate(DATE = as.Date(DATE, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))) %>% 
                   mutate(Concentration = as.numeric(Concentration)))) %>% 
      distinct() %>% left_join(sqlQuery(channel, "SELECT cd_unite, lb_unite FROM sandre_unite") %>% 
                                 rename(Code_Unite_mesure = cd_unite, Nom_unite_mesure = lb_unite) %>% 
                                 mutate(Code_Unite_mesure = as.character(Code_Unite_mesure)), by = "Code_Unite_mesure") %>% 
      select(-Nom_unite_mesure.x, -CdSupport) %>% 
      rename(Nom_unite_mesure = Nom_unite_mesure.y) %>% 
      group_nest(CODE_STATION, DATE)
    
    Diatom3 %>% group_nest(CODE_STATION, DATE) %>% 
      filter(year(DATE) == 2016, between(month(DATE), 5,9)) -> Diatom_2016
    
    Tab <- f_fusion(data_chimie = Chimie, data_biologie = Diatom_2016)
    
    df1 <- Tab %>% unnest(chimie) %>% select(-flore)
    
    df2 <- Tab %>% unnest(flore) %>% select(-chimie)
    
    dataset_names <- list('chimie' = df1, 'flore' = df2)
    
    write.xlsx(dataset_names, file = 'data/Donnees_utilisables/2016_traitee.xlsx')}
  
  if (Date == 2017){
    
    channel=odbcConnect("pandore")
    
    Chimie <- Chimie_pandore %>% filter(year(DATE) == Date) %>% 
      bind_rows((as.tibble(fread("data/Donnees_de_base/analyses_2017.csv"))%>% 
                   select("CODE_STATION" = CdStationMesureEauxSurface, "Code_Prelevement" = CdPrelevement, 
                          "DATE" = DatePrel, "Code_parametre" = CdParametre, "Nom_parametre" = LbLongParamètre, 
                          "Concentration" = RsAna, "Code_Unite_mesure" = CdUniteMesure, "Nom_unite_mesure" = SymUniteMesure, CdSupport) %>% 
                   mutate(CODE_STATION = str_remove(CODE_STATION, "^0+")) %>%
                   subset(CODE_STATION %in% c(unique(Diatom3$CODE_STATION))) %>% 
                   filter(Code_parametre == 1302 | Code_parametre == 1304 | Code_parametre == 1311
                          | Code_parametre == 1314 | Code_parametre == 1335| Code_parametre == 1433
                          | Code_parametre == 1340, CdSupport == 3) %>% 
                   mutate(DATE = as.Date(DATE, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))) %>% 
                   mutate(Concentration = as.numeric(Concentration)))) %>% 
      distinct() %>% left_join(sqlQuery(channel, "SELECT cd_unite, lb_unite FROM sandre_unite") %>% 
                                 rename(Code_Unite_mesure = cd_unite, Nom_unite_mesure = lb_unite) %>% 
                                 mutate(Code_Unite_mesure = as.character(Code_Unite_mesure)), by = "Code_Unite_mesure") %>% 
      select(-Nom_unite_mesure.x, -CdSupport) %>% 
      rename(Nom_unite_mesure = Nom_unite_mesure.y) %>% 
      group_nest(CODE_STATION, DATE)
    
    Diatom3 %>% group_nest(CODE_STATION, DATE) %>% 
      filter(year(DATE) == 2017, between(month(DATE), 5,9)) -> Diatom_2017
    
    Tab <- f_fusion(data_chimie = Chimie, data_biologie = Diatom_2017)
    
    df1 <- Tab %>% unnest(chimie) %>% select(-flore)
    
    df2 <- Tab %>% unnest(flore) %>% select(-chimie)
    
    dataset_names <- list('chimie' = df1, 'flore' = df2)
    
    write.xlsx(dataset_names, file = 'data/Donnees_utilisables/2017_traitee.xlsx')}
  
  if (Date == 2018){
    
    channel=odbcConnect("pandore")
    
    Chimie <- Chimie_pandore %>% filter(year(DATE) == Date) %>% 
      bind_rows((as.tibble(fread("data/Donnees_de_base/analyses_2018.csv"))%>% 
                   select("CODE_STATION" = CdStationMesureEauxSurface, "Code_Prelevement" = CdPrelevement, 
                          "DATE" = DatePrel, "Code_parametre" = CdParametre, "Nom_parametre" = LbLongParamètre, 
                          "Concentration" = RsAna, "Code_Unite_mesure" = CdUniteMesure, "Nom_unite_mesure" = SymUniteMesure, CdSupport) %>% 
                   mutate(CODE_STATION = str_remove(CODE_STATION, "^0+")) %>%
                   subset(CODE_STATION %in% c(unique(Diatom3$CODE_STATION))) %>% 
                   filter(Code_parametre == 1302 | Code_parametre == 1304 | Code_parametre == 1311
                          | Code_parametre == 1314 | Code_parametre == 1335| Code_parametre == 1433
                          | Code_parametre == 1340, CdSupport == 3) %>% 
                   mutate(DATE = as.Date(DATE, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))) %>% 
                   mutate(Concentration = as.numeric(Concentration)))) %>% 
      distinct() %>% left_join(sqlQuery(channel, "SELECT cd_unite, lb_unite FROM sandre_unite") %>% 
                                 rename(Code_Unite_mesure = cd_unite, Nom_unite_mesure = lb_unite) %>% 
                                 mutate(Code_Unite_mesure = as.character(Code_Unite_mesure)), by = "Code_Unite_mesure") %>% 
      select(-Nom_unite_mesure.x, -CdSupport) %>% 
      rename(Nom_unite_mesure = Nom_unite_mesure.y) %>% 
      group_nest(CODE_STATION, DATE)
    
    Diatom3 %>% group_nest(CODE_STATION, DATE) %>% 
      filter(year(DATE) == 2018, between(month(DATE), 5,9)) -> Diatom_2018
    
    Tab <- f_fusion(data_chimie = Chimie, data_biologie = Diatom_2018)
    
    df1 <- Tab %>% unnest(chimie) %>% select(-flore)
    
    df2 <- Tab %>% unnest(flore) %>% select(-chimie)
    
    dataset_names <- list('chimie' = df1, 'flore' = df2)
    
    write.xlsx(dataset_names, file = 'data/Donnees_utilisables/2018_traitee.xlsx')}
  
  if (Date == 2019){
    
    channel=odbcConnect("pandore")
    
    Chimie <- Chimie_pandore %>% filter(year(DATE) == Date) %>% 
      bind_rows((as.tibble(fread("data/Donnees_de_base/analyses_2019.csv"))%>% 
                   select("CODE_STATION" = CdStationMesureEauxSurface, "Code_Prelevement" = CdPrelevement, 
                          "DATE" = DatePrel, "Code_parametre" = CdParametre, "Nom_parametre" = LbLongParamètre, 
                          "Concentration" = RsAna, "Code_Unite_mesure" = CdUniteMesure, "Nom_unite_mesure" = SymUniteMesure, CdSupport) %>% 
                   mutate(CODE_STATION = str_remove(CODE_STATION, "^0+")) %>%
                   subset(CODE_STATION %in% c(unique(Diatom3$CODE_STATION))) %>% 
                   filter(Code_parametre == 1302 | Code_parametre == 1304 | Code_parametre == 1311
                          | Code_parametre == 1314 | Code_parametre == 1335| Code_parametre == 1433
                          | Code_parametre == 1340, CdSupport == 3) %>% 
                   mutate(DATE = as.Date(DATE, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))) %>% 
                   mutate(Concentration = as.numeric(Concentration)))) %>% 
      distinct() %>% left_join(sqlQuery(channel, "SELECT cd_unite, lb_unite FROM sandre_unite") %>% 
                                 rename(Code_Unite_mesure = cd_unite, Nom_unite_mesure = lb_unite) %>% 
                                 mutate(Code_Unite_mesure = as.character(Code_Unite_mesure)), by = "Code_Unite_mesure") %>% 
      select(-Nom_unite_mesure.x, -CdSupport) %>% 
      rename(Nom_unite_mesure = Nom_unite_mesure.y) %>% 
      group_nest(CODE_STATION, DATE)
    
    Diatom3 %>% group_nest(CODE_STATION, DATE) %>% 
      filter(year(DATE) == 2019, between(month(DATE), 5,9)) -> Diatom_2019
    
    Tab <- f_fusion(data_chimie = Chimie, data_biologie = Diatom_2019)
    
    df1 <- Tab %>% unnest(chimie) %>% select(-flore)
    
    df2 <- Tab %>% unnest(flore) %>% select(-chimie)
    
    dataset_names <- list('chimie' = df1, 'flore' = df2)
    
    write.xlsx(dataset_names, file = 'data/Donnees_utilisables/2019_traitee.xlsx')}
  
  if (Date == 2020){
    
    channel=odbcConnect("pandore")
    
    Chimie <- Chimie_pandore %>% filter(year(DATE) == Date) %>% 
      bind_rows((as.tibble(fread("data/Donnees_de_base/analyses_2020.csv"))%>% 
                   select("CODE_STATION" = CdStationMesureEauxSurface, "Code_Prelevement" = CdPrelevement, 
                          "DATE" = DatePrel, "Code_parametre" = CdParametre, "Nom_parametre" = LbLongParamètre, 
                          "Concentration" = RsAna, "Code_Unite_mesure" = CdUniteMesure, "Nom_unite_mesure" = SymUniteMesure, CdSupport) %>% 
                   mutate(CODE_STATION = str_remove(CODE_STATION, "^0+")) %>%
                   subset(CODE_STATION %in% c(unique(Diatom3$CODE_STATION))) %>% 
                   filter(Code_parametre == 1302 | Code_parametre == 1304 | Code_parametre == 1311
                          | Code_parametre == 1314 | Code_parametre == 1335| Code_parametre == 1433
                          | Code_parametre == 1340, CdSupport == 3) %>% 
                   mutate(DATE = as.Date(DATE, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))) %>% 
                   mutate(Concentration = as.numeric(Concentration)))) %>% 
      distinct() %>% left_join(sqlQuery(channel, "SELECT cd_unite, lb_unite FROM sandre_unite") %>% 
                                 rename(Code_Unite_mesure = cd_unite, Nom_unite_mesure = lb_unite) %>% 
                                 mutate(Code_Unite_mesure = as.character(Code_Unite_mesure)), by = "Code_Unite_mesure") %>% 
      select(-Nom_unite_mesure.x, -CdSupport) %>% 
      rename(Nom_unite_mesure = Nom_unite_mesure.y) %>% 
      group_nest(CODE_STATION, DATE)
    Diatom3 %>% group_nest(CODE_STATION, DATE) %>% 
      filter(year(DATE) == 2020, between(month(DATE), 5,9)) -> Diatom_2020
    
    Tab <- f_fusion(data_chimie = Chimie, data_biologie = Diatom_2020)
    
    df1 <- Tab %>% unnest(chimie) %>% select(-flore)
    
    df2 <- Tab %>% unnest(flore) %>% select(-chimie)
    
    dataset_names <- list('chimie' = df1, 'flore' = df2)
    
    write.xlsx(dataset_names, file = 'data/Donnees_utilisables/2020_traitee.xlsx')}
  
  if (Date == 2021){
    
    channel=odbcConnect("pandore")
    
    Chimie <- Chimie_pandore %>% filter(year(DATE) == Date) %>% 
      bind_rows((as.tibble(fread("data/Donnees_de_base/analyses_2021.csv"))%>% 
                   select("CODE_STATION" = CdStationMesureEauxSurface, "Code_Prelevement" = CdPrelevement, 
                          "DATE" = DatePrel, "Code_parametre" = CdParametre, "Nom_parametre" = LbLongParamètre, 
                          "Concentration" = RsAna, "Code_Unite_mesure" = CdUniteMesure, "Nom_unite_mesure" = SymUniteMesure, CdSupport) %>% 
                   mutate(CODE_STATION = str_remove(CODE_STATION, "^0+")) %>%
                   subset(CODE_STATION %in% c(unique(Diatom3$CODE_STATION))) %>% 
                   filter(Code_parametre == 1302 | Code_parametre == 1304 | Code_parametre == 1311
                          | Code_parametre == 1314 | Code_parametre == 1335| Code_parametre == 1433
                          | Code_parametre == 1340, CdSupport == 3) %>% 
                   mutate(DATE = as.Date(DATE, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))) %>% 
                   mutate(Concentration = as.numeric(Concentration)))) %>% 
      distinct() %>% left_join(sqlQuery(channel, "SELECT cd_unite, lb_unite FROM sandre_unite") %>% 
                                 rename(Code_Unite_mesure = cd_unite, Nom_unite_mesure = lb_unite) %>% 
                                 mutate(Code_Unite_mesure = as.character(Code_Unite_mesure)), by = "Code_Unite_mesure") %>% 
      select(-Nom_unite_mesure.x, -CdSupport) %>% 
      rename(Nom_unite_mesure = Nom_unite_mesure.y) %>% 
      group_nest(CODE_STATION, DATE)
    Diatom3 %>% group_nest(CODE_STATION, DATE) %>% 
      filter(year(DATE) == 2021, between(month(DATE), 5,9)) -> Diatom_2021
    
    Tab <- f_fusion(data_chimie = Chimie, data_biologie = Diatom_2021)
    
    df1 <- Tab %>% unnest(chimie) %>% select(-flore)
    
    df2 <- Tab %>% unnest(flore) %>% select(-chimie)
    
    dataset_names <- list('chimie' = df1, 'flore' = df2)
    
    write.xlsx(dataset_names, file = 'data/Donnees_utilisables/2021_traitee.xlsx')}
  
  return(Tab)
  
  }
