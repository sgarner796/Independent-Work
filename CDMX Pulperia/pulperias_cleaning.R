library(tidyverse)
library(sf)

#Set working directory
setwd("M:/p/GitHub/Independent Work/CDMX Pulperia")

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
  select(-all_of(not_needed)) %>% 
  filter(indicador == " Porcentaje de la población de 12 años y más económicamente activa ocupada")

PEA <- empleos %>% 
  select(desc_municipio,indicador,`2020`) %>% 
  filter(indicador == "Porcentaje de población de 12 años y más económicamente activa")

## comercio

comercio <- comercio[-c(1:3), ]
colnames(comercio) <- as.character(unlist(comercio[1, ]))
comercio_clean <- comercio[-1, ]

comercio_trim <- comercio_clean %>% 
  slice(-1:-20)



