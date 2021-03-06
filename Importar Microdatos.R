# ANÁLISIS MATRICULADOS SNIES----

# Paquetes y versiones requeridas

library(tidyverse) # version 1.2.1
library(readxl)    # version 1.0.0
library(gganimate)
library(gifski)
library(gapminder)


#Función Importar

importar <- function(year, periodo){
  read_excel(paste0("Microdatos/Matriculados/",year,".xlsx"), 
             sheet = periodo, guess_max = 100000)
}


# Importar bases de datos por año y periodo 

M0013 <- importar("Historico 2013", "Matriculados 2000-2013") 

M2014 <- importar("Serie 2014", "Matricula_2014") 
M2015 <- importar("Serie 2015", "Matricula_2015") 
M2016 <- importar("Serie 2016", "Matriculados_2016") 
M2017 <- importar("Serie 2017", "Hoja1") 

# Arreglar base 2000-2013

M0013 <- M0013 %>% gather(GENERO, TOTAL, 'Hombre 2000-1':'Mujer 2013-2') %>%
  separate(GENERO, c("GENERO", "AÑO"), sep = " ") %>% 
  separate(AÑO, c("YEAR", "SEMESTRE")) %>% 
  mutate(GENERO = ifelse(GENERO == "Hombre", "MASCULINO", "FEMENINO"),
         ID_GENERO = NA,
         YEAR = as.numeric(YEAR),
         SEMESTRE = as.numeric(SEMESTRE)) %>% 
  filter(TOTAL != 0) %>% 
  mutate(NIVEL = case_when(NIV_FOR == "UNIVERSITARIA" ~ "PREGRADO",
                           NIV_FOR == "TECNICA PROFESIONAL" ~ "PREGRADO",
                           NIV_FOR == "TECNOLOGICA" ~ "PREGRADO",
                           NIV_FOR == "ESPECIALIZACION" ~ "POSGRADO",
                           NIV_FOR == "MAESTRIA" ~ "POSGRADO",
                           NIV_FOR == "DOCTORADO" ~ "POSGRADO"))

# Pegar las bases de datos importadas por abajo

Matriculados <- bind_rows(M0013, M2014, M2015, M2016, M2017)  

# Trasformar Variables

# Carácter de las instituciones

# Matriculados <- Matriculados %>% mutate(CARÁCTER = toupper(CARÁCTER))
Matriculados <- Matriculados %>% mutate(CARÁCTER = toupper(CARÁCTER),
                                        CARÁCTER = ifelse(CARÁCTER == "INSTITUCION TECNICA PROFESIONAL",
                                                          "INSTITUCIÓN TÉCNICA PROFESIONAL",
                                                          CARÁCTER),
                                        CARÁCTER = ifelse(CARÁCTER == "INSTITUCION TECNOLOGICA",
                                                          "INSTITUCIÓN TECNOLÓGICA",
                                                          CARÁCTER),
                                        CARÁCTER = ifelse(CARÁCTER == "INSTITUCION UNIVERSITARIA/ESCUELA TECNOLOGICA",
                                                          "INSTITUCIÓN UNIVERSITARIA/ESCUELA TECNOLÓGICA",
                                                          CARÁCTER)) %>% 
  mutate(CARÁCTER = ifelse(CARÁCTER == "INSTITUCIÓN TÉCNICA PROFESIONAL", "Técnica", CARÁCTER),
         CARÁCTER = ifelse(CARÁCTER == "INSTITUCIÓN TECNOLÓGICA", "Tecnológica", CARÁCTER),
         CARÁCTER = ifelse(CARÁCTER == "INSTITUCIÓN UNIVERSITARIA/ESCUELA TECNOLÓGICA", "IU/ET", CARÁCTER),
         CARÁCTER = ifelse(CARÁCTER == "UNIVERSIDAD", "Universitaria", CARÁCTER)
  ) %>%
  mutate(NIV_FOR = ifelse(NIV_FOR == "FORMACION TECNICA PROFESIONAL", "Técnica", NIV_FOR),
         NIV_FOR = ifelse(NIV_FOR == "Formación técnica profesional", "Técnica", NIV_FOR),
         NIV_FOR = ifelse(NIV_FOR == "TECNICA PROFESIONAL", "Técnica", NIV_FOR),
         NIV_FOR = ifelse(NIV_FOR == "TECNOLOGICA", "Tecnológica", NIV_FOR),
         NIV_FOR = ifelse(NIV_FOR == "Tecnológica", "Tecnológica", NIV_FOR),
         NIV_FOR = ifelse(NIV_FOR == "Universitaria", "Universitaria", NIV_FOR),
         NIV_FOR = ifelse(NIV_FOR == "UNIVERSITARIA", "Universitaria", NIV_FOR),
         NIV_FOR = ifelse(NIV_FOR == "DOCTORADO", "Doctorado", NIV_FOR),
         NIV_FOR = ifelse(NIV_FOR == "MAESTRIA", "Maestría", NIV_FOR),
         NIV_FOR = ifelse(NIV_FOR == "ESPECIALIZACION", "Especialización", NIV_FOR),
         NIV_FOR = ifelse(NIV_FOR == "Especialización Médico Quirúrgica", "Especialización", NIV_FOR),
         NIV_FOR = ifelse(NIV_FOR == "Especialización Universitaria", "Especialización", NIV_FOR),
         NIV_FOR = ifelse(NIV_FOR == "Especialización Técnico Profesional", "Especialización TyT", NIV_FOR),
         NIV_FOR = ifelse(NIV_FOR == "Especialización Tecnológica", "Especialización TyT", NIV_FOR)
  ) %>% 
  mutate(METODOLOGIA = ifelse(METODOLOGIA == "A DISTANCIA (TRADICIONAL)", "Distancia (tradicional)", METODOLOGIA),
         METODOLOGIA = ifelse(METODOLOGIA == "DISTANCIA (TRADICIONAL)", "Distancia (tradicional)", METODOLOGIA),
         METODOLOGIA = ifelse(METODOLOGIA == "A DISTANCIA (VIRTUAL)", "Distancia (virtual)", METODOLOGIA),
         METODOLOGIA = ifelse(METODOLOGIA == "VIRTUAL", "Distancia (virtual)", METODOLOGIA),
         METODOLOGIA = ifelse(METODOLOGIA == "PRESENCIAL", "Presencial", METODOLOGIA),
         METODOLOGIA = ifelse(METODOLOGIA == "SIN METODOLOGIA DEFINIDA", "Sin Definir", METODOLOGIA)
  ) %>% 
  mutate(AREA = ifelse(AREA == "AGRONOMIA VETERINARIA Y AFINES", "Agronomía, veterinaria y afines", AREA),
         AREA = ifelse(AREA == "AGRONOMIA, VETERINARIA Y AFINES", "Agronomía, veterinaria y afines", AREA),
         AREA = ifelse(AREA == "BELLAS ARTES", "Bellas artes", AREA),
         AREA = ifelse(AREA == "CIENCIAS DE LA EDUCACION", "Ciencias de la educación", AREA),
         AREA = ifelse(AREA == "CIENCIAS DE LA SALUD", "Ciencias de la salud", AREA),
         AREA = ifelse(AREA == "CIENCIAS SOCIALES Y HUMANAS", "Ciencias sociales y humanas", AREA),
         AREA = ifelse(AREA == "ECONOMIA ADMINISTRACION CONTADURIA Y AFINES", "Economía, administración, contaduría y afines", AREA),
         AREA = ifelse(AREA == "ECONOMIA, ADMINISTRACION, CONTADURIA Y AFINES", "Economía, administración, contaduría y afines", AREA),
         AREA = ifelse(AREA == "INGENIERIA ARQUITECTURA URBANISMO Y AFINES", "Ingeniería, arquitectura, urbanismo y afines", AREA),
         AREA = ifelse(AREA == "INGENIERIA, ARQUITECTURA, URBANISMO Y AFINES", "Ingeniería, arquitectura, urbanismo y afines", AREA),
         AREA = ifelse(AREA == "MATEMATICAS Y CIENCIAS NATURALES", "Matemáticas y ciencias naturales", AREA))  

# DIVIPOLA ----

# Lectura del estándar División Política y Administrativa de Colombia - DIVIPOLA

DIVIPOLA <- read.table("Estandares/DIVIPOLA_20160930.csv", sep=";", header=T)


# Base de datos con información de cabeceras municipales del archivo divipola

Cabeceras <- DIVIPOLA %>% select(code_dept=Código.Departamento,
                                 code_mun=Código.Municipio,
                                 departamentos=Nombre.Departamento,
                                 municipios=Nombre.Municipio,
                                 tipo_centro=Tipo.Centro.Poblado,
                                 longitud=Longitud,
                                 latitud=Latitud) %>% 
  filter(tipo_centro == "CABECERA MUNICIPAL (CM)") %>% 
  rename("ID" = "code_mun", "lng" = "longitud", "lat" = "latitud")   


Cabeceras$lng	<- as.numeric(str_replace(Cabeceras$lng, ",", "."))
Cabeceras$lat	<- as.numeric(str_replace(Cabeceras$lat, ",", "."))
