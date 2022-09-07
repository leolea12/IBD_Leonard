

library(tidyverse)
library(sf)
library(readr)
library(readxl)

Flore <- as.tibble(fread("Data/Flore.csv"))

New_taxons <- read.csv2("Data/Fichier_DREAL_1.csv", stringsAsFactors = FALSE)

`%notin%` <- Negate(`%in%`)
Communes <- c(
  "Mana", "Roura", "Regina", "Maripasoula", "Papaichton", "Kourou",
  "Saint-Laurent-du-Maroni", "Iracoubo", "Montsinery-Tonnegrande", "Apatou",
  "Massangis"
)

dt_Chronologie <- Flore %>% filter(RESULTAT != 0) %>% 
  distinct() %>%
  sf::st_as_sf(coords = c("x", "y"), crs = 2154) %>%
  sf::st_transform(crs = 4326) %>%
  tidyr::extract(geometry, c("lon", "lat"), "\\((.*), (.*)\\)", convert = TRUE) %>%
  select(-geometry) %>%
  mutate(
    lon = if_else(Commune == "Massangis", 4.00, lon),
    lat = if_else(Commune == "Massangis", 47.6, lat)
  ) %>%
  filter(Commune %notin% Communes[-11]) %>%
  as_tibble() %>%
  mutate(annee = lubridate::year(lubridate::ymd(DATE))) %>%
  mutate(ab_rel = round((RESULTAT * 400 / Tot_indiv)/400, 5)) %>%
  group_by(annee) %>%
  mutate(ID = cur_group_id()) %>%
  ungroup() %>%
  left_join(as.tibble(read.csv2("Data/table_transcodage.csv", stringsAsFactors = FALSE)) %>%
    mutate(
      name_valid = sub("\\_g.*", "", name_valid),
      code_taxon = Code_valid
    ) %>%
    select(code_taxon, name_valid) %>% distinct(), by = "code_taxon") %>%
  left_join(as.tibble(read.csv2("Data/Nouveau_taxons_DREAL.csv", stringsAsFactors = FALSE)) %>%
    select(code_taxon = abre, nom_IBD) %>% distinct(), by = "code_taxon") %>%
  mutate(
    name_valid = if_else(is.na(nom_IBD) == TRUE, name_valid, nom_IBD),
    annee = as.factor(annee)
  ) %>%
  filter(code_taxon %in% unique(New_taxons %>% pull(abre))) %>%
  select(-nom_IBD) %>%
  mutate(
    name_valid = str_replace_all(name_valid, "[^[:alnum:]]", " "),
    add_names = case_when(
      code_taxon == "CEUO" ~ "Cocconeis euglyptoides",
      code_taxon == "COCO" ~ "Cocconeis",
      code_taxon == "SEAT" ~ "Sellaphora atomoides",
      code_taxon == "SNIG" ~ "Sellaphora nigri"
    ),
    name_valid = if_else(is.na(add_names) == T, name_valid, add_names),
    full_name = paste0(name_valid, " ", "(", code_taxon, ")")
  ) %>%
  left_join(as.tibble(read.csv2("Data/table_transcodage.csv", stringsAsFactors = FALSE)) %>%
    select(abre, name, code_taxon = Code_valid) %>% unique() %>%
    group_by(code_taxon) %>% filter(abre %notin% code_taxon) %>% mutate(list = paste0(abre, " ", sub("\\_g.*", "", name))) %>%
    mutate(taxons_apparies = paste(list, collapse = " / ")) %>%
    select(-abre, -name, -list) %>% distinct(), by = "code_taxon") %>%
  select(-add_names)



write.csv2(dt_Chronologie, file = ("Data/Chorologie.csv"))

# ajouer au shiny les taxons appariés
