library(leaflet)
library(tidyverse)

# Mapa. Ubicación IES ----

# Microdatos

MUNI_IES_172 <- Matriculados %>% group_by(YEAR, SEMESTRE, IES, COD_MUN_IES, MUN_IES, SECTOR) %>% count() %>% 
                filter(YEAR == 2017, SEMESTRE == 2) %>% rename("ID" = "COD_MUN_IES") %>% ungroup()
             

MUNI_IES_172 <- left_join(MUNI_IES_172, Cabeceras, by = "ID")


# Mapa

pal <- colorFactor(palette = c("red", "blue"), levels = c("OFICIAL", "PRIVADA"))

MAP_IES <- MUNI_IES_172 %>% leaflet() %>% addTiles() %>% 
  setView(lng = -72.95, lat = 4.083 , zoom = 6) %>% 
  addCircleMarkers(popup = ~paste0("<b>", IES, "</b>", "<br/>", MUN_IES), radius = 6, color = ~pal(SECTOR)) %>% 
  addLegend(title = "SECTOR" ,position = "bottomright", pal = pal, values = c("OFICIAL", "PRIVADA")) %>% 
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.083 , -72.95) ,  6); }")))

MAP_IES

#label = ~paste0("<b>", IES, "</b>", "<br/>", MUN_IES)
#, label = ~NOMBRE

# Mapa. Ubicación PROGRAMAS ----

MUNI_PROGRAMAS_172 <- Matriculados %>% group_by(YEAR, SEMESTRE, IES, COD_MUN_PROG, MUN_PROG, SECTOR) %>% count() %>% 
  filter(YEAR == 2017, SEMESTRE == 2) %>% rename("ID" = "COD_MUN_PROG") %>% ungroup()


MUNI_PROGRAMAS_172 <- left_join(MUNI_PROGRAMAS_172, Cabeceras, by = "ID") %>% filter(!is.na(lng))

# Mapa General

# pal <- colorFactor(palette = c("red", "blue"), levels = c("OFICIAL", "PRIVADA"))

MAP_PROGRAMAS <- MUNI_PROGRAMAS_172 %>% leaflet() %>% addTiles() %>% 
  setView(lng = -72.95, lat = 4.083 , zoom = 6) %>% 
  addCircleMarkers(popup = ~paste0("<b>", IES, "</b>", "<br/>", MUN_PROG), radius = 6) %>%  #color = ~pal(SECTOR)) %>% 
  #addLegend(title = "SECTOR" ,position = "bottomright", pal = pal, values = c("OFICIAL", "PRIVADA")) %>% 
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.083 , -72.95) ,  6); }")))

MAP_PROGRAMAS

# Mapa U Privadas

MAP_PROGRAMAS_PRIV <- MUNI_PROGRAMAS_172 %>% filter(SECTOR == "PRIVADA") %>% leaflet() %>% addTiles() %>% 
  setView(lng = -72.95, lat = 4.083 , zoom = 6) %>% 
  addCircleMarkers(popup = ~paste0("<b>", IES, "</b>", "<br/>", MUN_PROG), radius = 6, col = "red") %>%  #color = ~pal(SECTOR)) %>% 
  #addLegend(title = "SECTOR" ,position = "bottomright", pal = pal, values = c("OFICIAL", "PRIVADA")) %>% 
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.083 , -72.95) ,  6); }")))

MAP_PROGRAMAS_PRIV

# Mapa U Oficilaes

MAP_PROGRAMAS_OFIC <- MUNI_PROGRAMAS_172 %>% filter(SECTOR == "OFICIAL") %>% leaflet() %>% addTiles() %>% 
  setView(lng = -72.95, lat = 4.083 , zoom = 6) %>% 
  addCircleMarkers(popup = ~paste0("<b>", IES, "</b>", "<br/>", MUN_PROG), radius = 6, col = "green") %>%  #color = ~pal(SECTOR)) %>% 
  #addLegend(title = "SECTOR" ,position = "bottomright", pal = pal, values = c("OFICIAL", "PRIVADA")) %>% 
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.083 , -72.95) ,  6); }")))

MAP_PROGRAMAS_OFIC

# Mapa. Programas NIVEL ----

MUNI_PROG_NIVEL_172 <- Matriculados %>% group_by(YEAR, SEMESTRE, IES, COD_MUN_PROG, MUN_PROG, NIVEL) %>% count() %>% 
  filter(YEAR == 2017, SEMESTRE == 2) %>% rename("ID" = "COD_MUN_PROG") %>% ungroup()


MUNI_PROG_NIVEL_172 <- left_join(MUNI_PROG_NIVEL_172, Cabeceras, by = "ID") %>% filter(!is.na(lng))

# PREGRADO

MAP_MUNI_PROG_NIVEL_PRE_172 <- MUNI_PROG_NIVEL_172 %>% filter(NIVEL == "PREGRADO") %>% leaflet() %>% addTiles() %>% 
  setView(lng = -72.95, lat = 4.083 , zoom = 6) %>% 
  addCircleMarkers(popup = ~paste0("<b>", IES, "</b>", "<br/>", MUN_PROG), radius = 6, col = "blue") %>%  #color = ~pal(SECTOR)) %>% 
  #addLegend(title = "SECTOR" ,position = "bottomright", pal = pal, values = c("OFICIAL", "PRIVADA")) %>% 
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.083 , -72.95) ,  6); }")))

MAP_MUNI_PROG_NIVEL_PRE_172

# POSTGRADO

MAP_MUNI_PROG_NIVEL_POS_172 <- MUNI_PROG_NIVEL_172 %>% filter(NIVEL == "POSGRADO") %>% leaflet() %>% addTiles() %>% 
  setView(lng = -72.95, lat = 4.083 , zoom = 6) %>% 
  addCircleMarkers(popup = ~paste0("<b>", IES, "</b>", "<br/>", MUN_PROG), radius = 6, col = "red") %>%  #color = ~pal(SECTOR)) %>% 
  #addLegend(title = "SECTOR" ,position = "bottomright", pal = pal, values = c("OFICIAL", "PRIVADA")) %>% 
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.083 , -72.95) ,  6); }")))

MAP_MUNI_PROG_NIVEL_POS_172

# Mapa. Programas DOC Y MAES ----

MUNI_PROG_DOCT_172 <- Matriculados %>% group_by(YEAR, SEMESTRE, IES, COD_MUN_PROG, MUN_PROG, NIV_FOR) %>% count() %>% 
  filter(YEAR == 2017, SEMESTRE == 2) %>% rename("ID" = "COD_MUN_PROG") %>% ungroup()


MUNI_PROG_DOCT_172 <- left_join(MUNI_PROG_DOCT_172, Cabeceras, by = "ID") %>% filter(!is.na(lng))


# ESPECIALIZACIÓN

MAP_MUNI_PROG_NIVEL_ESP_172 <- MUNI_PROG_DOCT_172 %>% filter(NIV_FOR == "Especialización") %>% leaflet() %>% addTiles() %>% 
  setView(lng = -72.95, lat = 4.083 , zoom = 6) %>% 
  addCircleMarkers(popup = ~paste0("<b>", IES, "</b>", "<br/>", MUN_PROG), radius = 6, col = "blue") %>%  #color = ~pal(SECTOR)) %>% 
  #addLegend(title = "SECTOR" ,position = "bottomright", pal = pal, values = c("OFICIAL", "PRIVADA")) %>% 
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.083 , -72.95) ,  6); }")))

MAP_MUNI_PROG_NIVEL_ESP_172

# MAESTRÍA

MAP_MUNI_PROG_NIVEL_MAES_172 <- MUNI_PROG_DOCT_172 %>% filter(NIV_FOR == "Maestría") %>% leaflet() %>% addTiles() %>% 
  setView(lng = -72.95, lat = 4.083 , zoom = 6) %>% 
  addCircleMarkers(popup = ~paste0("<b>", IES, "</b>", "<br/>", MUN_PROG), radius = 6, col = "green") %>%  #color = ~pal(SECTOR)) %>% 
  #addLegend(title = "SECTOR" ,position = "bottomright", pal = pal, values = c("OFICIAL", "PRIVADA")) %>% 
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.083 , -72.95) ,  6); }")))

MAP_MUNI_PROG_NIVEL_MAES_172

# DOCTORADO

MAP_MUNI_PROG_NIVEL_DOC_172 <- MUNI_PROG_DOCT_172 %>% filter(NIV_FOR == "Doctorado") %>% leaflet() %>% addTiles() %>% 
  setView(lng = -72.95, lat = 4.083 , zoom = 6) %>% 
  addCircleMarkers(popup = ~paste0("<b>", IES, "</b>", "<br/>", MUN_PROG), radius = 6, col = "red") %>%  #color = ~pal(SECTOR)) %>% 
  #addLegend(title = "SECTOR" ,position = "bottomright", pal = pal, values = c("OFICIAL", "PRIVADA")) %>% 
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.083 , -72.95) ,  6); }")))

MAP_MUNI_PROG_NIVEL_DOC_172
