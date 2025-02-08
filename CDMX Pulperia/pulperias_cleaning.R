library(tidyverse)
library(sf)

#Set working directory
setwd("C:/Users/steph/Documents/GitHub/Independent-Work/CDMX Pulperia")

pulperia <- read.xlsx("pulperias.xlsx", sheet = 2)
empleos <- read.xlsx("empleo_09.xlsx")
comercio <- read.xlsx("comercioemp_09.xlsx")

# Remove the first three rows
empleos <- empleos[-c(1:3), ]

# Make the fourth row the column names
colnames(empleos) <- as.character(unlist(empleos[1, ]))

# Remove the row now used as column names
empleos <- empleos[-1, ]

empleos <- empleos %>% 
  slice(-483:-500) %>% 
  slice(-1:-71)

not_needed <- c("cve_entidad","desc_entidad","cve_municipio","id_indicador")

empleos <- empleos %>% 
  select(-all_of(not_needed)) 

PEA <- empleos %>% 
select(desc_municipio,indicador,`2020`) %>% 
  filter(indicador == "Porcentaje de población de 12 años y más económicamente activa")

PEA <- PEA  %>% 
  rename(PEA = `2020`) %>% 
  select(desc_municipio, PEA)

## comercio

comercio <- comercio[-c(1:3), ]
colnames(comercio) <- as.character(unlist(comercio[1, ]))
comercio_clean <- comercio[-1, ]

comercio_trim <- comercio_clean %>% 
  slice(-1:-20)

comercio_trim<- comercio_trim %>% 
  select(desc_municipio,indicador,`2008`,`2018`)

comercio_trim <- comercio_trim %>% 
  filter(indicador == "Total de ingresos por suministro de bienes y servicios. Gran sector 43-46. Comercio") %>% 
  rename(ingresos = `2008`)

comercio_trim <- comercio_trim %>% 
  select(desc_municipio,ingresos)

### Creacion de variables pulpe

pulperia <- pulperia %>%
  mutate(
    Telefono_binary = ifelse(!is.na(`Telefono`) & `Telefono` != "", 1, 0),
    Correo_binary = ifelse(!is.na(`Correo.Electrónico`) & `Correo.Electrónico` != "", 1, 0),
    Pagina_binary = ifelse(!is.na(`Págiona.web`) & `Págiona.web` != "", 1, 0),
    Count_non_missing_percentage = (`Telefono_binary` + `Correo_binary` + `Pagina_binary`) / 3 * 100
  )
