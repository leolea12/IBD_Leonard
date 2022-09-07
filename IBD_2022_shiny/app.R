#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(sf)
library(readr)
library(readxl)
library(leaflet)
library(cartography)
library(oceanis)
library(lubridate)
library(rsconnect)
library(DT)
library(data.table)
library(stringr)
library(shinythemes)
library(scales)
library(magick)
library(bslib)
library(mapview)
library(htmlwidgets)
library(leaflegend)
library(leafem)
library(EBImage)
getwd()

x <- "a1~!@#$%^&*(){}_+:\"<>?,./;'[]-="

Flore <- as.tibble(fread("Data/Flore.csv"))

dt_Chorologie <- as.tibble(read.csv2("Data/Chorologie.csv", stringsAsFactors = FALSE)) %>%
  select(-X, -V1)

selection_taxon <- dt_Chorologie %>%
  select(full_name) %>%
  unique() %>%
  arrange(full_name) %>% 
  pull(full_name)

map_base <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas,
    group = "Fond clair"
  ) %>%
  addProviderTiles(providers$CartoDB.DarkMatter,
    group = "Fond noir"
  ) %>%
  addProviderTiles(providers$GeoportailFrance.orthos,
    group = "Fond satellite"
  )

pal <- colorNumeric(palette = carto.pal(pal1 = "multi.pal", 15), domain = unique(dt_Chorologie$ID))

plot <- function(data, Date) {
  data %>%
    group_by(annee) %>%
    mutate(Occurence = n()) %>%
    ungroup() %>%
    group_by(code_taxon, annee) %>%
    mutate(Presence_moyenne = mean(ab_rel) * 100, na.rm = T) %>%
    select(code_taxon, annee, Presence_moyenne) %>%
    distinct(code_taxon, annee, .keep_all = T) %>%
    filter(annee == Date) %>%
    ggplot(aes(x = code_taxon, y = Presence_moyenne, fill = code_taxon)) +
    geom_bar(stat = "identity", color = "black") +
    labs(y = "Présence Moyenne (%)") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      text = element_text(size = 20),
      legend.position = "none"
    ) +
    labs(title = paste0("Représentation visuelle de l'importance moyenne de chaque taxon dans un échantillon pour l'année", ": ", as.character(Date)))
}

ui <- fluidPage(
  
  theme = shinytheme("united"),
  navbarPage(
    title = div("", img(src = 'https://www.association-francaise-halieutique.fr/wp-content/uploads/logoINRAE.jpg', id = "simulation", height = "65px",width = "65px",style = "position: relative; margin:-13px 0px; display:right-align;")),
    tags$head(tags$style(HTML('.navbar-static-top {background-color: #00a3a6; font-size:20px}',
                              '.navbar-default .navbar-nav>.active>a {background-color: #00a3a6;}',
                              '.navbar-nav > li > a, .navbar-brand {
                   padding-top:25px !important; 
                   padding-bottom:60px !important;
                   height: 65px;
                 }
                 .navbar {min-height:25px !important;}',
                              type='text/css', ".item {font-style: italic;} .selectize-dropdown {font-style: italic;}"))),
    
    tabPanel("Bienvenue !", 
             strong("Bienvenue sur l'application IBD 2022 !", style = "font-size:40px;"),
             br(),
             br(),
             p("Avant de vous lancer, prenez soin de lire attentivement ce petit tutoriel
               dans lequel vous apprendrez à parcourir l'interface et les différents menu
               qui la compose ! Vous aller avoir à votre disposition 3 onglets sur lesquels vous pourrez aller
               et venir librement. Parmis ces onglets vous trouverez:", style = "font-size:20px;text-align: justify;"),
             
             strong("l'onglet Temporalité", style = "color:#00a3a6;font-size:30px;text-align: justify;"),
             
             p("Il présente 1 menu déroulant comme celui ci dessous: ", style = "font-size:20px;text-align: justify;"),
             
            
            p("Il permet de sélectionner le taxon que vous voulez. 
            Joint à ce menu, 2 graphiques présentent le nombre de stations
             ou le taxon est vu chaque année ainsi que son abondance relative
             moyenne et enfin 1 carte interactive vous permet de visualiser 
             la chorologie du taxon sélectionné. En effet, à chaque 
             fois que vous sélectionnerez un taxon, la carte se mettra à jour ainsi
             que les graphiques. Vous pouvez librement cocher/décocher les annees sur la carte afin
               d'obtenir une représentation visuelle de la répartition du taxon en France
               pour l'année voulue. Vous pouvez également superposer les années si vous le
               souhaitez ! En cliquant sur un point de la carte, vous pourrez accéder
               aux informations concernant la Commune et la position géographique du taxon.", style = "font-size:20px;text-align: justify;"),
             
             strong("l'onglet Données", style = "color:#00a3a6;font-size:30px;"),
            
             p("Cet onglet vous permettra d'accéder aux données du taxon dans sa globalité.
               Comme sur excel, vous pouvez trier les colonnes dans l'order que vous voulez et effectuer
               des recherches plus précises si vous le souhaitez. chaque colonne présente
               une barre de recherches dans lequel vous pouvez indiquez les valeurs à extraire.
               Voici un exemple: ", style = "font-size:20px;text-align: justify;"),
            
            p("tapez:", style = "font-size:20px;text-align: justify;"),
            
            code("5...50", style = "color:#00a3a6;font-size:20px;text-align: justify;"),
            
            br(),
            
            p("dans la barre de recherche ABONDANCE de l'onglet données et voyez ce qu'il se passe. Cela permet de filtrer
              toute les abondances comprises entre 5 et 50 !
              Répétez cette opération pour chaque colonne en fonction de vos besoin en prenant
              soin de ne pas oublier les ... entre les deux valeurs", style = "font-size:20px;text-align: justify;"),
            
            p("Toujours dans cet onglet Données, les boutons: ", style = "font-size:20px;"),
            
             
            code("Download", style = "color:#00a3a6;font-size:20px;"), p("et", style = "font-size:20px;"), code("pdf", style = "color:#00a3a6;font-size:20px;"),
            
             p("Vous permettent de télécharger les données en cas de besoin (Pour nous faire un 
               retour sur d'éventuelles erreur par exemple), le bouton pdf génère automatiquement un aperçu 
               du fichier tandis que le bouton dowload vous invite à enregistrer un fichier csv à l'endroit de votre choix
               sur votre session.", style = "font-size:20px;text-align: justify;"),
            
            strong("l'onglet Synthèse", style = "color:#00a3a6;font-size:20px;text-align: justify;", style = "font-size:30px;text-align: justify;"),
            
            br(),
            
            p("Vous permet de visualiser de manière synthétique la part moyenne
              représentée par chaque taxons dans 1 échantillon pour une année 
              souhaitée", style = "font-size:20px;text-align: justify;"),
            
            p("Cette application à été concue pour que vous puissiez visualiser
                   la données de manière concrète et que vous ayez à disposition
                   un outil vous permettant de nous faire des retours sur l'utilité 
                   d'utiliser les nouveaux taxons que vous avez proposé dans la mise
                   à jour de l'IBD 2022. N'hésitez donc pas à nous faire tous les 
                   retours que vous jugerez nécessaires !", style = "font-size:20px;text-align: justify;"),
            
            strong("Bonne naviguation !",
                   style="margin-left: auto; margin-right: auto;font-size:30px;color:#00a3a6;text-align: justify;"),
            
            br(),
            br(),
            
            img(src='https://i.pinimg.com/236x/e4/2f/d1/e42fd142f47c12ed979bae806c8eed75--science-room-science-week.jpg',
                height=147,
                width=236,
                style="display: block; margin-left: auto; margin-right: auto;text-align: justify;"),
           
          br(),
            br()
            ),
    
    tabPanel(
      "Temporalité",
      sidebarLayout(
        sidebarPanel(
          selectInput("categorical_variable",
            label = "Choix du code taxon :",
            choices = selection_taxon,
            selectize = TRUE
          ),
          code("Taxons compris dans cette appelation: ", style = "font-size:15px;color:#00a3a6"), p(textOutput("name_list"), style = "font-size:15px;color:black;"),
          plotOutput("Plot1", width = "100%"), plotOutput("Plot2",  width = "100%")
        ),
        mainPanel(
          tabPanel("Carte", fluidRow(leafletOutput("mapFiltered", width = "100%", height="1000px")),
          )
        )
      )
    ),
    tabPanel("Données", dataTableOutput("Donnees", width = "100%"), downloadButton("downloadData", "Download")),
    
    navbarMenu(
      "Evolution par années",
      tabPanel("2007", plotOutput("Plot_2007", width = "100%", height="800px")),
      tabPanel("2008", plotOutput("Plot_2008", width = "100%", height="800px")),
      tabPanel("2009", plotOutput("Plot_2009", width = "100%", height="800px")),
      tabPanel("2010", plotOutput("Plot_2010", width = "100%", height="800px")),
      tabPanel("2011", plotOutput("Plot_2011", width = "100%", height="800px")),
      tabPanel("2012", plotOutput("Plot_2012", width = "100%", height="800px")),
      tabPanel("2013", plotOutput("Plot_2013", width = "100%", height="800px")),
      tabPanel("2014", plotOutput("Plot_2014", width = "100%", height="800px")),
      tabPanel("2015", plotOutput("Plot_2015", width = "100%", height="800px")),
      tabPanel("2016", plotOutput("Plot_2016", width = "100%", height="800px")),
      tabPanel("2017", plotOutput("Plot_2017", width = "100%", height="800px")),
      tabPanel("2018", plotOutput("Plot_2018", width = "100%", height="800px")),
      tabPanel("2019", plotOutput("Plot_2019", width = "100%", height="800px")),
      tabPanel("2020", plotOutput("Plot_2020", width = "100%", height="800px")),
      tabPanel("2021", plotOutput("Plot_2021", width = "100%", height="800px")),
    )
  )
)

# Define server logic ----

server <- function(input, output, session) {

  mapFiltered <- reactive({
    
    leaflet_data <- dt_Chorologie %>% filter(full_name %in% input$categorical_variable) %>%
      mutate(annee = as.factor(annee)) %>% 
      left_join(data.frame(
        full_name = sort(dt_Chorologie %>% select(full_name) %>% unique() %>% pull(full_name)),
        img = c("www/Achnanthidium atomoides (ADAM).jpg",
                "www/Achnanthidium caravelense (ADCV).png",
                "www/Achnanthidium delmontii (ADMO).jpg",
                "www/Achnanthidium druartii (ADRU).jpg",
                "www/Achnanthidium hoffmannii (AHOF).png",
                "www/Achnanthidium minutissima groupe  (ADMI).png",
                "www/Achnanthidium subhudsonis var  kraeuselii (ADSK).jpg",
                "www/Achnanthidium tropicocatenatum (ADTC).png",
                "www/Achnanthidium zhakovschikovii (AZHA).png",
                "www/Adlafia baicalensis (AFBA).jpg",
                "www/Adlafia langebertalotii (ALBL).png",
                "www/Amphora macedoniensis (AMCD).jpg",
                "www/Cocconeis (COCO).jpg",
                "www/Cocconeis euglyptoides (CEUO).jpg",
                "www/Coconeis anciennement placentula (CPLA).jpg",
                "www/Cymbella excisiformis (CEXF).jpg",
                "www/Cymbella lancettula (CLTL).jpg",
                "www/Cymbella subhelvetica (CSBH).jpg",
                "www/Cymbella subtruncata var  subtruncata (CSUT).jpg",
                "www/Cymbella tridentina (CTDE).jpg",
                "www/Cymbopleura pyrenaica (CBPY).jpg",
                "www/Encyonema bonapartei (EBNA).png",
                "www/Encyonema simile (ENSI).jpg",
                "www/Encyonopsis alpina (ECAL).jpg",
                "www/Encyonopsis neerlandica (ENEE).jpg",
                "www/Eunotia arcubus var  arcubus (EARB).jpg",
                "www/Eunotia cantonati (ECTO).jpg",
                "www/Eunotia juettnerae (EJUE).jpg",
                "www/Eunotia pseudogroenlandica (EPSG).jpg",
                "www/Fragilaria canariensis (FCAN).jpeg",
                "www/Fragilaria candidagilae (FCAD).jpg",
                "www/Fragilaria gracilis (FGRA).jpg",
                "www/Fragilaria microvaucheriae (FMIV).jpg",
                "www/Fragilaria neointermedia (FNIN).png",
                "www/Fragilaria nevadensis (FNEV).jpg",
                "www/Fragilaria pararumpens (FPRU).png",
                "www/Fragilaria perdelicatissima (FPDE).jpg",
                "www/Gomphonema angustivalva (GAGV).png",
                "www/Gomphonema auritum (GAUR).png",
                "www/Gomphonema cuneolus (GCUN).jpg",
                "www/Gomphonema innocens (GINN).jpg",
                "www/Gomphonema minusculum (GMIS).jpg",
                "www/Gomphonema pumilum  groupe  (GPUM).jpg",
                "www/Gomphonema tenoccultum (GTNO).jpg",
                "www/Gomphonema varioreduncum (GVRD).jpg",
                "www/Halamphora thumensis (HTHU).jpg",
                "www/Hippodonta pseudacceptata (HPDA).png",
                "www/Hippodonta ruthnielseniae (HRUT).jpg",
                "www/Luticola hlubikovae (LHLU).jpg",
                "www/Navicula difficillimoides (NDFO).jpg",
                "www/Navicula phylleptosoma (NPHP).jpg",
                "www/Navicula subalpina (NSBN).jpg",
                "www/Nitzschia costei (NYCO).jpg",
                "www/Nitzschia soratensis (NSTS).jpg",
                "www/Planothidium minutissimum (PMNT).jpg",
                "www/Planothidium reichardtii (PLRC).jpg",
                "www/Platessa saxonica (PSXO).jpg",
                "www/Pseudostaurosira polonica (PSPO).png",
                "www/Pseudostaurosira sopotensis (PSOT).jpg",
                "www/Punctastriata ovalis (POVA).jpg",
                "www/Sellaphora atomoides (SEAT).png",
                "www/Sellaphora bosniaca (SBOS).png",
                "www/Sellaphora capitata (SECA).jpg",
                "www/Sellaphora crassulexigua (SCRA).jpg",
                "www/Sellaphora nigri (SNIG).png",
                "www/Sellaphora pseudoarvensis (SPDV).jpg",
                "www/Sellaphora raederae (SRAE).jpg",
                "www/Sellaphora saprotolerans (SESP).jpg",
                "www/Sellaphora schaumburgii (SSBG).jpg")) %>% 
          group_by(full_name) %>% mutate(label = cur_group_id()), by = "full_name")
      
    titre <- dt_Chorologie %>%
      filter(full_name %in% input$categorical_variable) %>%
      pull(name_valid) %>%
      unique()

    map <- map_base %>%
      addCircleMarkers(
        data = leaflet_data,
        group = ~annee,
        color = ~ pal(ID),
        opacity = 1, 
        radius = 1,
        popup = paste("Année: ", leaflet_data$annee, "<br>",
                      "Taxon: ", leaflet_data$full_name, "<br>",
                      "Commune: ", leaflet_data$Commune, "<br>",
                      "Coordonnées: ", paste0(leaflet_data$lon, " : ",leaflet_data$lat),"<br>")) %>%
      addLegendImage(images = leaflet_data[1,]$img,
                     title = htmltools::tags$div('Aspect visuel du taxon',
                                                 style = 'font-size: 20px;
                                             text-align: center;'),
                     labels = paste(""),
                     width = 350,
                     height = 350,
                     orientation = 'vertical',
                     position = 'bottomleft') %>% 
      addLayersControl(
        position = "topleft",
        baseGroups = c(
          "Fond satellite",
          "Fond clair",
          "Fond noir"
        ),
        overlayGroups = dt_Chorologie %>% pull(annee),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup(group = dt_Chorologie$annee) %>% 
      setView(lng = 1.6
              , lat = 46.8
              , zoom = 6) %>%
      setMaxBounds( lng1 = -4
                    , lat1 = 50
                    , lng2 = 8
                    , lat2 = 36.5 )
      
  })
  
  # output$Imagen<- renderImage({
  #   if(input$categorical_variable=="Achnanthidium atomoides (ADAM)") Leg<-"www/Achnanthidium atomoides (ADAM).jpg"
  #   if(input$categorical_variable=="Achnanthidium caravelense (ADCV)") Leg<-"www/Achnanthidium caravelense (ADCV).png"
  #   if(input$categorical_variable=="Achnanthidium delmontii (ADMO)") Leg<-"Achnanthidium delmontii (ADMO).jpg"
  #   if(input$categorical_variable=="Achnanthidium druartii (ADRU)") Leg<-"Achnanthidium druartii (ADRU).jpg"
  #   if(input$categorical_variable=="Achnanthidium hoffmannii (AHOF)") Leg<-"www/Achnanthidium hoffmannii (AHOF).png"
  #   if(input$categorical_variable=="Achnanthidium minutissima groupe  (ADMI)") Leg<-"www/Achnanthidium minutissima groupe  (ADMI).png"
  #   if(input$categorical_variable=="Achnanthidium subhudsonis var  kraeuselii (ADSK)") Leg<-"www/Achnanthidium subhudsonis var  kraeuselii (ADSK).jpg"
  #   if(input$categorical_variable=="Achnanthidium tropicocatenatum (ADTC)") Leg<-"www/Achnanthidium tropicocatenatum (ADTC).png"
  #   if(input$categorical_variable=="Achnanthidium zhakovschikovii (AZHA)") Leg<-"www/Achnanthidium zhakovschikovii (AZHA).png"
  #   if(input$categorical_variable=="Adlafia baicalensis (AFBA)") Leg<-"www/Adlafia baicalensis (AFBA).jpg"
  #   if(input$categorical_variable=="Adlafia langebertalotii (ALBL)") Leg<-"www/Adlafia langebertalotii (ALBL).png"
  #   if(input$categorical_variable=="Amphora macedoniensis (AMCD)") Leg<-"www/Amphora macedoniensis (AMCD).jpg"
  #   if(input$categorical_variable=="Cocconeis (COCO)" ) Leg<-"www/Cocconeis (COCO).jpg"
  #   if(input$categorical_variable=="Cocconeis euglyptoides (CEUO)") Leg<-"www/Cocconeis euglyptoides (CEUO).jpg"
  #   if(input$categorical_variable=="Coconeis anciennement placentula (CPLA)"  ) Leg<-"www/Coconeis anciennement placentula (CPLA).jpg"
  #   if(input$categorical_variable=="Cymbella excisiformis (CEXF)") Leg<-"www/Cymbella excisiformis (CEXF).jpg"
  #   if(input$categorical_variable=="Cymbella lancettula (CLTL)") Leg<-"www/Cymbella lancettula (CLTL).jpg"
  #   if(input$categorical_variable=="Cymbella subhelvetica (CSBH)") Leg<-"www/Cymbella subhelvetica (CSBH).jpg"
  #   if(input$categorical_variable=="Cymbella subtruncata var  subtruncata (CSUT)") Leg<-"www/Cymbella subtruncata var  subtruncata (CSUT).jpg"
  #   if(input$categorical_variable=="Cymbella tridentina (CTDE)") Leg<-"www/Cymbella tridentina (CTDE).jpg"
  #   if(input$categorical_variable=="Cymbopleura pyrenaica (CBPY)") Leg<-"www/Cymbopleura pyrenaica (CBPY).jpg"
  #   if(input$categorical_variable=="Encyonema bonapartei (EBNA)") Leg<-"www/Encyonema bonapartei (EBNA).png"
  #   if(input$categorical_variable=="Encyonema simile (ENSI)") Leg<-"www/Encyonema simile (ENSI).jpg"
  #   if(input$categorical_variable=="Encyonopsis alpina (ECAL)") Leg<-"www/Encyonopsis alpina (ECAL).jpg"
  #   if(input$categorical_variable=="Encyonopsis neerlandica (ENEE)") Leg<-"www/Encyonopsis neerlandica (ENEE).jpg"
  #   if(input$categorical_variable=="Eunotia arcubus var  arcubus (EARB)") Leg<-"www/Eunotia arcubus var  arcubus (EARB).jpg"
  #   if(input$categorical_variable=="Eunotia cantonati (ECTO)") Leg<-"www/Eunotia cantonati (ECTO).jpg"
  #   if(input$categorical_variable=="Eunotia juettnerae (EJUE)") Leg<-"www/Eunotia juettnerae (EJUE).jpg"
  #   if(input$categorical_variable=="Eunotia pseudogroenlandica (EPSG)") Leg<-"www/Eunotia pseudogroenlandica (EPSG).jpg"
  #   if(input$categorical_variable=="Fragilaria canariensis (FCAN)") Leg<-"www/Fragilaria canariensis (FCAN).jpg"
  #   if(input$categorical_variable=="Fragilaria candidagilae (FCAD)") Leg<-"www/Fragilaria candidagilae (FCAD).jpg"
  #   if(input$categorical_variable== "Fragilaria gracilis (FGRA)") Leg<-"www/Fragilaria gracilis (FGRA).jpg"
  #   if(input$categorical_variable=="Fragilaria microvaucheriae (FMIV)") Leg<-"www/Fragilaria microvaucheriae (FMIV).jpg"
  #   if(input$categorical_variable=="Fragilaria neointermedia (FNIN)" ) Leg<-"www/Fragilaria neointermedia (FNIN).png"
  #   if(input$categorical_variable=="Fragilaria nevadensis (FNEV)" ) Leg<-"www/Fragilaria nevadensis (FNEV).jpg"
  #   if(input$categorical_variable=="Fragilaria pararumpens (FPRU)") Leg<-"www/Fragilaria pararumpens (FPRU).png"
  #   if(input$categorical_variable=="Fragilaria perdelicatissima (FPDE)") Leg<-"www/Fragilaria perdelicatissima (FPDE).jpg"
  #   if(input$categorical_variable=="Gomphonema angustivalva (GAGV)") Leg<-"www/Gomphonema angustivalva (GAGV).png"
  #   if(input$categorical_variable=="Gomphonema auritum (GAUR)") Leg<-"www/Gomphonema auritum (GAUR).png"
  #   if(input$categorical_variable=="Gomphonema cuneolus (GCUN)") Leg<-"www/Gomphonema cuneolus (GCUN).jpg"
  #   if(input$categorical_variable=="Gomphonema innocens (GINN)") Leg<-"www/Gomphonema innocens (GINN).jpg"
  #   if(input$categorical_variable=="Gomphonema minusculum (GMIS)") Leg<-"www/Gomphonema minusculum (GMIS).jpg"
  #   if(input$categorical_variable=="Gomphonema pumilum  groupe  (GPUM)") Leg<-"www/Gomphonema pumilum  groupe  (GPUM).jpg"
  #   if(input$categorical_variable=="Gomphonema tenoccultum (GTNO)") Leg<-"www/Gomphonema tenoccultum (GTNO).jpg"
  #   if(input$categorical_variable=="Gomphonema varioreduncum (GVRD)") Leg<-"www/Gomphonema varioreduncum (GVRD).jpg"
  #   if(input$categorical_variable=="Halamphora thumensis (HTHU)") Leg<-"www/Halamphora thumensis (HTHU).jpg"
  #   if(input$categorical_variable=="Hippodonta pseudacceptata (HPDA)") Leg<-"www/Hippodonta pseudacceptata (HPDA).png"
  #   if(input$categorical_variable=="Hippodonta ruthnielseniae (HRUT)") Leg<-"www/Hippodonta ruthnielseniae (HRUT).jpg"
  #   if(input$categorical_variable=="Luticola hlubikovae (LHLU)") Leg<-"www/Luticola hlubikovae (LHLU).jpg"
  #   if(input$categorical_variable=="Navicula difficillimoides (NDFO)") Leg<-"www/Navicula difficillimoides (NDFO).jpg"
  #   if(input$categorical_variable=="Navicula phylleptosoma (NPHP)") Leg<-"www/Navicula phylleptosoma (NPHP).jpg"
  #   if(input$categorical_variable=="Navicula subalpina (NSBN)") Leg<-"www/Navicula subalpina (NSBN).jpg"
  #   if(input$categorical_variable=="Nitzschia costei (NYCO)") Leg<-"www/Nitzschia costei (NYCO).jpg"
  #   if(input$categorical_variable=="Nitzschia soratensis (NSTS)") Leg<-"www/Nitzschia soratensis (NSTS).jpg"
  #   if(input$categorical_variable== "Planothidium minutissimum (PMNT)") Leg<-"www/Planothidium minutissimum (PMNT).jpg"
  #   if(input$categorical_variable=="Planothidium reichardtii (PLRC)") Leg<-"www/Planothidium reichardtii (PLRC).jpg"
  #   if(input$categorical_variable=="Platessa saxonica (PSXO)") Leg<-"www/Platessa saxonica (PSXO).jpg"
  #   if(input$categorical_variable=="Pseudostaurosira polonica (PSPO)") Leg<-"www/Pseudostaurosira polonica (PSPO).png"
  #   if(input$categorical_variable=="Pseudostaurosira sopotensis (PSOT)") Leg<-"www/Pseudostaurosira sopotensis (PSOT).jpg"
  #   if(input$categorical_variable=="Punctastriata ovalis (POVA)") Leg<-"www/Punctastriata ovalis (POVA).jpg"
  #   if(input$categorical_variable=="Sellaphora atomoides (SEAT)") Leg<-"www/Sellaphora atomoides (SEAT).png"
  #   if(input$categorical_variable=="Sellaphora bosniaca (SBOS)") Leg<-"www/Sellaphora bosniaca (SBOS).png"
  #   if(input$categorical_variable=="Sellaphora capitata (SECA)") Leg<-"www/Sellaphora capitata (SECA).jpg"
  #   if(input$categorical_variable=="Sellaphora crassulexigua (SCRA)") Leg<-"www/Sellaphora crassulexigua (SCRA).jpg"
  #   if(input$categorical_variable=="Sellaphora nigri (SNIG)") Leg<-"www/Sellaphora nigri (SNIG).png"
  #   if(input$categorical_variable=="Sellaphora pseudoarvensis (SPDV)") Leg<-"www/Sellaphora pseudoarvensis (SPDV).jpg"
  #   if(input$categorical_variable=="Sellaphora raederae (SRAE)") Leg<-"www/Sellaphora raederae (SRAE).jpg"
  #   if(input$categorical_variable=="Sellaphora saprotolerans (SESP)") Leg<-"www/Sellaphora saprotolerans (SESP).jpg"
  #   if(input$categorical_variable=="Sellaphora schaumburgii (SSBG)") Leg<-"www/Sellaphora schaumburgii (SSBG).jpg"
  #   list(src=Leg, width = "50%", height = "50%", style="display: block; margin-left: auto; margin-right: auto;")
  # }, deleteFile = FALSE)  

  output$mapFiltered <- renderLeaflet({
    mapFiltered()
    
  })

  output$name_list <- renderText({
    as.character(dt_Chorologie %>%
      filter(full_name %in% input$categorical_variable) %>%
      select(taxons_apparies) %>%
      unique() %>%
      mutate(taxons_apparies = if_else(is.na(taxons_apparies) == T, "Aucun", taxons_apparies)))
  })

  output$Plot1 <- renderPlot({
    dt_Chorologie %>%
      filter(full_name %in% input$categorical_variable) %>%
      mutate(annee = as.factor(annee)) %>%
      group_by(annee) %>%
      summarise(Abondance_relative = mean(ab_rel, na.rm = TRUE)) %>%
      ggplot2::ggplot(aes(y = Abondance_relative, x = annee, group = 1)) +
      geom_line(size = 0.5) +
      geom_point(size = 2) +
      labs(
        title = "Abondance relative moyenne",
        x = "Année", y = "Abondance relative"
      ) +
      InraeThemes::theme_inrae() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  })

  output$Plot2 <- renderPlot({
    dt_Chorologie %>%
      filter(full_name %in% input$categorical_variable) %>%
      mutate(annee = as.factor(annee)) %>%
      group_by(annee) %>%
      summarise(Occurence = n()) %>%
      ggplot2::ggplot(aes(y = Occurence, x = annee, group = 1)) +
      geom_line(size = 0.5) +
      geom_point(size = 2) +
      labs(
        title = "Nombre de recensement",
        x = "Année", y = "Nombre de stations"
      ) +
      InraeThemes::theme_inrae() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  })

  output$Donnees <- renderDataTable(datatable(dt_Chorologie %>%
    filter(full_name %in% input$categorical_variable) %>%
    select(
      "ANNEE" = annee, CODE = code_taxon, NOM = name_valid, STATION = CODE_STATION,
      COMMUNE = Commune, ABONDANCE = RESULTAT, "ABONDANCE_RELATIVE (pour 400)" = ab_rel, TOTAL = Tot_indiv
    ),
  extensions = "Buttons",
  options = list(
    pageLength = 20, dom = "lftipB",
    buttons = c("pdf"),
    scroller = TRUE
  ),
  filter="top", selection="multiple", escape=FALSE))

  output$Plot_2007 <- renderPlot(
    {
      plot(dt_Chorologie, 2007)
    },
  )
  output$Plot_2008 <- renderPlot(
    {
      plot(dt_Chorologie, 2008)
    },
  )
  output$Plot_2009 <- renderPlot(
    {
      plot(dt_Chorologie, 2009)
    },
  )
  output$Plot_2010 <- renderPlot(
    {
      plot(dt_Chorologie, 2010)
    },
  )
  output$Plot_2011 <- renderPlot(
    {
      plot(dt_Chorologie, 2011)
    },
  )
  output$Plot_2012 <- renderPlot(
    {
      plot(dt_Chorologie, 2012)
    },
  
  )
  output$Plot_2013 <- renderPlot(
    {
      plot(dt_Chorologie, 2013)
    },
  )
  output$Plot_2014 <- renderPlot(
    {
      plot(dt_Chorologie, 2014)
    },
  )
  output$Plot_2015 <- renderPlot(
    {
      plot(dt_Chorologie, 2015)
    },
  )
  output$Plot_2016 <- renderPlot(
    {
      plot(dt_Chorologie, 2016)
    },
  )
  output$Plot_2017 <- renderPlot(
    {
      plot(dt_Chorologie, 2017)
    },
  )
  output$Plot_2018 <- renderPlot(
    {
      plot(dt_Chorologie, 2018)
    },
  )
  output$Plot_2019 <- renderPlot(
    {
      plot(dt_Chorologie, 2019)
    },
  )
  output$Plot_2020 <- renderPlot(
    {
      plot(dt_Chorologie, 2020)
    },
  )
  output$Plot_2021 <- renderPlot(
    {
      plot(dt_Chorologie, 2021)
    },
  )

  output$downloadData <- downloadHandler(
    filename = function() {
      paste(as.character(dt_Chorologie %>%
        filter(full_name %in% input$categorical_variable) %>%
        unique() %>%
        pull(full_name)), Sys.Date(), ".csv", sep = ".")
    },
    content = function(file) {
      write.csv2(dt_Chorologie %>% filter(full_name %in% input$categorical_variable), file)
    }
  )
}



# Run the app ----
shinyApp(ui = ui, server = server)








