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
    labs(title = paste0("Représentation visuelle de l'importance de chaque taxon pour l'année", ": ", as.character(Date)))
}


ui <- fluidPage(
  theme = shinytheme("united"),
  navbarPage(
    "Barre de Naviguation",
    tabPanel(
      "Temporalité",
      sidebarLayout(
        sidebarPanel(
          selectInput("categorical_variable",
            label = "Choix du code taxon :",
            choices = selection_taxon,
            selectize = FALSE
          ),
          code("Taxons appariés: ", style = "font-size:20px;"), code(textOutput("name_list"), style = "font-size:15px;"),
          plotOutput("Plot1", height = "350"), plotOutput("Plot2", height = "350")
        ),
        mainPanel(
          tabPanel("Carte", fluidRow(leafletOutput("mapFiltered", width = "1250", height = "900")))
        )
      )
    ),
    tabPanel("Données", dataTableOutput("Donnees", width = "1800"), downloadButton("downloadData", "Download")),
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
server <- function(input, output) {
  mapFiltered <- reactive({
    titre <- dt_Chorologie %>%
      filter(full_name %in% input$categorical_variable) %>%
      pull(name_valid) %>%
      unique()

    map <- map_base %>%
      addCircles(
        data = dt_Chorologie %>% filter(full_name %in% input$categorical_variable) %>%
          mutate(annee = as.factor(annee)),
        group = ~annee,
        label = ~Commune, color = ~ pal(ID),
        opacity = 1, radius = 1
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
      hideGroup(group = dt_Chorologie$annee)
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
        title = "Importance du taxon dans les échantillons",
        x = "Année", y = "Abondance relative (1/N)"
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
        title = "Nombre de stations de recensement",
        x = "Année", y = "Occurence"
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
  )
  ))

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
