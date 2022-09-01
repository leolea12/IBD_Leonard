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

getwd()

x <- "a1~!@#$%^&*(){}_+:\"<>?,./;'[]-="

Flore <- as.tibble(fread("Data/Flore.csv"))

dt_Chorologie <- as.tibble(read.csv2("Data/Chorologie.csv", stringsAsFactors = FALSE)) %>%
  select(-X, -V1)

selection_taxon <- dt_Chorologie %>%
  select(full_name) %>%
  unique() %>%
  arrange(full_name)

map_base <- leaflet() %>%
  setView(lng = 3.4, lat = 47, zoom = 6) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas,
    group = "Fond clair"
  ) %>%
  addProviderTiles(providers$CartoDB.DarkMatter,
    group = "Fond noir"
  ) %>%
  addProviderTiles(providers$GeoportailFrance.orthos,
    group = "Fond satellite"
  )

pal <- colorNumeric(palette = carto.pal(pal1 = "pastel.pal", 15), domain = unique(dt_Chorologie$ID))

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
    "Barre de Naviguation",
    tags$head(tags$style(HTML('.navbar-static-top {background-color: #00a3a6;}',
                              '.navbar-default .navbar-nav>.active>a {background-color: #00a3a6;}'))),
    tabPanel("Bienvenue !", 
             strong("Bienvenue sur Shiny IBD 2022 !", style = "font-size:20px;"),
             p("Avant de vous lancer, prenez soin de lire attentivement ce petit tutoriel
               dans lequel vous apprendrez à parcourir l'interface et les différents menu
               qui la compose ! Vous aller avoir à votre disposition 3 onglets sur lesquels vous pourrez aller
               et venir librement. Parmis ces onglets vous trouverez:"),
             
             code("l'onglet Temporalité", style = "color:#00a3a6"),
             
             p("Il présente 1 menu déroulant comme celui ci dessous: "),
             
            
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
               aux informations concernant la Commune et la position géographique du taxon"),
             
             code("l'onglet Données", style = "color:#00a3a6"),
            
             p("Cet onglet vous permettra d'accéder aux données du taxon dans sa globalité.
               Comme sur excel, vous pouvez trier les colonnes dans l'order que vous voulez et effectuer
               des recherches plus précises si vous le souhaitez. chaque colonne présente
               une barre de recherches dans lequel vous pouvez indiquez les valeurs à extraire.
               Voici un exemple: "),
            
            strong("tapez:"),
            code("5...50", style = "color:#00a3a6"),
            
            br(),
            
            p("dans l'onglet ABONDANCE et voyez ce qu'il se passe. Cela permet de filtrer
              toute les abondances comprises entre 5 et 50 !
              Répétez cette opération pour chaque colonne en fonction de vos besoin en prenant
              soin de ne pas oublier les ... entre les deux valeurs"),
            
            p("Toujours dans cet onglet Données, les boutons: "),
            
             
            code("Download", style = "color:#00a3a6"), p("et"), code("pdf", style = "color:#00a3a6"),
            
             p("Vous permettent de télécharger les données en cas de besoin (Pour nous faire un 
               retour sur d'éventuelles erreur par exemple)"),
            
            code("l'onglet Complément", style = "color:#00a3a6"),
            
            br(),
            
            p("Vous permet de visualiser de manière synthétique la part moyenne
              représentée par chaque taxons dans 1 échantillon pour une année 
              souhaitée"),
            
            p("Cette application à été concue pour que vous puissiez visualiser
                   la données de manière concrète et que vous ayez à disposition
                   un outil vous permettant de nous faire des retours sur l'utilité 
                   d'utiliser les nouveaux taxons que vous avez proposé dans la mise
                   à jour de l'IBD 2022. N'hésitez donc pas à nous faire tous les 
                   retours que vous jugerez nécessaires !"),
            
            strong("Sur ce, je vous souhaite une agréable naviguation !",
                   style="margin-left: auto; margin-right: auto;font-size:20px;color:#00a3a6"),
            
            br(),
            br(),
            
            img(src='https://i.pinimg.com/236x/e4/2f/d1/e42fd142f47c12ed979bae806c8eed75--science-room-science-week.jpg',
                height=147,
                width=236,
                style="display: block; margin-left: auto; margin-right: auto;"),
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
            selectize = FALSE
          ),
          code("Taxons appariés: ", style = "font-size:20px;color:#00a3a6"), code(textOutput("name_list"), style = "font-size:15px;color:black"),
          plotOutput("Plot1", width = "100%"), plotOutput("Plot2", width = "100%")
        ),
        mainPanel(
          tabPanel("Carte", fluidRow(leafletOutput("mapFiltered", width = "100%", height="1000px")))
        )
      )
    ),
    tabPanel("Données", dataTableOutput("Donnees", width = "100%"), downloadButton("downloadData", "Download")),
    
    navbarMenu(
      "Compléments",
      tabPanel("2007", plotOutput("Plot_2007", width = "100%")),
      tabPanel("2008", plotOutput("Plot_2008", width = "100%")),
      tabPanel("2009", plotOutput("Plot_2009", width = "100%")),
      tabPanel("2010", plotOutput("Plot_2010", width = "100%")),
      tabPanel("2011", plotOutput("Plot_2011", width = "100%")),
      tabPanel("2012", plotOutput("Plot_2012", width = "100%")),
      tabPanel("2013", plotOutput("Plot_2013", width = "100%")),
      tabPanel("2014", plotOutput("Plot_2014", width = "100%")),
      tabPanel("2015", plotOutput("Plot_2015", width = "100%")),
      tabPanel("2016", plotOutput("Plot_2016", width = "100%")),
      tabPanel("2017", plotOutput("Plot_2017", width = "100%")),
      tabPanel("2018", plotOutput("Plot_2018", width = "100%")),
      tabPanel("2019", plotOutput("Plot_2019", width = "100%")),
      tabPanel("2020", plotOutput("Plot_2020", width = "100%")),
      tabPanel("2021", plotOutput("Plot_2021", width = "100%")),
    )
  )
)

# Define server logic ----

server <- function(input, output, session) {

  mapFiltered <- reactive({
    
    leaflet_data <- dt_Chorologie %>% filter(full_name %in% input$categorical_variable) %>%
      mutate(annee = as.factor(annee))
      
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
                      "Coordonnées: ", paste0(leaflet_data$lon, " : ",leaflet_data$lat))
      ) %>%
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
              , zoom = 5.5) %>%
      setMaxBounds( lng1 = -4
                    , lat1 = 50
                    , lng2 = 8
                    , lat2 = 36.5 )
  })
    
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
      COMMUNE = Commune, ABONDANCE = RESULTAT, ABONDANCE_RELATIVE = ab_rel, TOTAL = Tot_indiv
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
    height = 800,
    width = 1700
  )
  output$Plot_2008 <- renderPlot(
    {
      plot(dt_Chorologie, 2008)
    },
    height = 800,
    width = 1700
  )
  output$Plot_2009 <- renderPlot(
    {
      plot(dt_Chorologie, 2009)
    },
    height = 800,
    width = 1700
  )
  output$Plot_2010 <- renderPlot(
    {
      plot(dt_Chorologie, 2010)
    },
    height = 800,
    width = 1700
  )
  output$Plot_2011 <- renderPlot(
    {
      plot(dt_Chorologie, 2011)
    },
    height = 800,
    width = 1700
  )
  output$Plot_2012 <- renderPlot(
    {
      plot(dt_Chorologie, 2012)
    },
    height = 800,
    width = 1700
  )
  output$Plot_2013 <- renderPlot(
    {
      plot(dt_Chorologie, 2013)
    },
    height = 800,
    width = 1700
  )
  output$Plot_2014 <- renderPlot(
    {
      plot(dt_Chorologie, 2014)
    },
    height = 800,
    width = 1700
  )
  output$Plot_2015 <- renderPlot(
    {
      plot(dt_Chorologie, 2015)
    },
    height = 800,
    width = 1700
  )
  output$Plot_2016 <- renderPlot(
    {
      plot(dt_Chorologie, 2016)
    },
    height = 800,
    width = 1700
  )
  output$Plot_2017 <- renderPlot(
    {
      plot(dt_Chorologie, 2017)
    },
    height = 800,
    width = 1700
  )
  output$Plot_2018 <- renderPlot(
    {
      plot(dt_Chorologie, 2018)
    },
    height = 800,
    width = 1700
  )
  output$Plot_2019 <- renderPlot(
    {
      plot(dt_Chorologie, 2019)
    },
    height = 800,
    width = 1700
  )
  output$Plot_2020 <- renderPlot(
    {
      plot(dt_Chorologie, 2020)
    },
    height = 800,
    width = 1700
  )
  output$Plot_2021 <- renderPlot(
    {
      plot(dt_Chorologie, 2021)
    },
    height = 800,
    width = 1700
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
