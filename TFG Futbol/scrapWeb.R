########################################## Descripción #######################################################
#                         Web scrapping de la página www.transfermarkt.es
#                       Se obtienen los datos del dinero invertido en fichajes
#                         por los equipos de las 5 mayores ligas de Europa


# carga de librerias ----------------------------------------------------------------------------------------
PAQUETES <- c("pacman")
inst <- match(PAQUETES, .packages(all=TRUE))
need <- which(is.na(inst))
if (length(need) > 0) install.packages(PAQUETES[need])
#Cargar paquetes
lapply(PAQUETES, require, character.only=T)
pacman::p_load(tidyverse, rvest, xlsx, readr, stringr)

# Función que realiza el scrapping --------------------------------------------------------------------------

scrap_ligas <- function(x, y){
  # Realiza el scrapping de las tablas por liga, la cual trae 
  # información de: equipo, plantilla, edad, valor total de 
  # fichajes y sigma valor de los fichajes
  #
  # Args:
  #   x: url de la web
  #   y: número de equipos de la liga correspondiente
  #
  # Returns:
  #   Una tabla con los datos de equipo, plantilla, edad, valor total de 
  #   fichajes y sigma valor de los fichajes de la liga correspondiente
  #   al url  
  
  url_pagina <- as.character(x)
  xml_pagina <- read_html(url_pagina)
  
  #Establece límites según el número de clubes
  ll <- if_else(y==20,40,36)
  jj <- if_else(y==20,84,76)
  
  #Clubes
  clubesTemporal <-  html_nodes(xml_pagina, ".hauptlink, no-border-links show-for-small show-for-pad") %>%
    html_text()
  clubesTemporal <- clubesTemporal[1:ll]
  clubes <- vector()
  for(i in seq(2,length(clubesTemporal),2)){
    clubes[ceiling(i/2)] <- clubesTemporal[i]
  }
  
  #Plantila
  plantillaTemporal <- html_nodes(xml_pagina, '.zentriert') %>% 
    html_text()
  plantillaTemporal <- plantillaTemporal[5:jj]
  plantilla <- vector()
  for(i in seq(4,length(plantillaTemporal),4)){
    plantilla[ceiling(i/4)] <-  plantillaTemporal[i]
  }
  
  #Valores de mercado
  valoresTemporal <- html_nodes(xml_pagina, '.hide-for-pad') %>% 
    html_text()
  valoresTemporal1 <- valoresTemporal[12:length(valoresTemporal)]
  valorMercadoTemp <- vector()
  for(i in seq(1,length(valoresTemporal1),5)){
    valorMercadoTemp[ceiling(i/5)] <- valoresTemporal1[i]
  }
  
  valorMercadoTemp <- str_replace_all(valorMercadoTemp,'mill\\.','') %>%
    str_sub(1,6) %>%
    str_trim()
  
  #Meto esto para el caso de cifras de "mil millones"
  valorMercado <- vector()
  for (i in seq_along(valorMercadoTemp)) {
    if ("m" %in% str_split(valorMercadoTemp[i], " ")[[1]]) {
      valorMercado[i] <- str_replace_all(valorMercadoTemp[i],'m','') %>% 
        str_replace_all(' ','0') %>% 
        str_replace_all(',','')
    }
    else{
      valorMercado[i] <- valorMercadoTemp[i]
    }
  }
  
  
  valoresTemporal2 <- valoresTemporal[13:length(valoresTemporal)]
  thetaValor <- vector()
  for(i in seq(1,length(valoresTemporal2),5)){
    thetaValor[ceiling(i/5)] <- valoresTemporal2[i]
  }
  
  thetaValor <- str_replace_all(thetaValor,'mill\\.','') %>%
    str_sub(1,6) %>%
    str_trim()
  
  valoresTemporal3 <- valoresTemporal[10:length(valoresTemporal)]
  edad <- vector()
  for(i in seq(1,length(valoresTemporal3),5)){
    edad[ceiling(i/5)] <- valoresTemporal3[i]
  }
  
  
  
  tablaProv <- as_tibble(cbind(plantilla, edad, valorMercado, thetaValor))
  
  tabla <-  tablaProv %>% 
    map(function(x) str_replace(x,',','.')) %>%
    map(as.numeric) %>% 
    transform(plantilla = as.integer(plantilla)) %>% 
    cbind(clubes) %>%    
    transform(clubes = as.character(clubes)) %>% 
    as_tibble() %>% 
    select(clubes, plantilla:thetaValor)
  
  return(tabla)
  
}

# Scrapping  ---------------------------------------------------------------------------------------------

ligasEuropa <- c('https://www.transfermarkt.es/primera-division/startseite/wettbewerb/ES1/saison_id/2017',
                 
                 'https://www.transfermarkt.es/premier-league/startseite/wettbewerb/GB1/saison_id/2017',
                 
                 'https://www.transfermarkt.es/serie-a/startseite/wettbewerb/IT1/saison_id/2017',
                 
                 'https://www.transfermarkt.es/ligue-1/startseite/wettbewerb/FR1/saison_id/2017',
                 
                 'https://www.transfermarkt.es/1-bundesliga/startseite/wettbewerb/L1/saison_id/2017')

numeroClubes <- c(rep(20,4),18)

tablaEuropa <- tibble()

for(i in 1:length(ligasEuropa)){
  tabla <- scrap_ligas(ligasEuropa[i], numeroClubes[i])
  tablaEuropa <- bind_rows(tablaEuropa, tabla)
}

Liga <- c(rep('España',20), rep('Inglaterra',20), rep('Italia', 20), rep('Francia', 20), rep('Alemania',18))

tablaEuropa <- bind_cols(tablaEuropa, as_tibble(Liga)) 

colnames(tablaEuropa)[ncol(tablaEuropa)] <- 'Liga'

#Guardamos como excel y csv

setwd("") #Coloca entre las comillas la ruta donde quieres que se guarden los archivos

write.xlsx(x = as.data.frame(tablaEuropa), file = "ligas.xlsx",
           row.names = FALSE)
write.csv(tablaEuropa, file = 'ligas2.csv', row.names = FALSE)


