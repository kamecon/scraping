###Carga de paquetes ----

if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
pacman::p_load(tidyverse, miceadds, xlsx, tidytext, plyr)
pacman::p_load_gh("trinker/sentimentr")


##Funciones ----

scrapeAsin <-  function(Asin, producto){
  
  # Construye una tabla para cada producto con los comentarios
  # las fechas de los mismos, el rating (estrellas) y si el 
  # comentario ha sido escrito por un vine o sonsumidor normal
  # Dicha función fue realizada a partir del código del post
  # https://justrthings.com/2016/08/17/web-scraping-and-sentiment-analysis-of-amazon-reviews/
  # y utiliza una versión de la función `amazon_scraper`
  # https://raw.githubusercontent.com/rjsaito/Just-R-Things/master/Text%20Mining/amazonscraper.R
  #
  # Args:
  #   Asin: asin del producto
  #   producto: nombre del producto
  #
  # Returns:
  #   La tabla arriba descrita

pages <- 10

reviews_all <- NULL
for(page_num in 1:pages){
  print(page_num)
  print(Asin)
  print(producto)
  url <- paste0("http://www.amazon.com/product-reviews/",Asin,"/?pageNumber=", page_num)
  doc <- read_html(url)
  
  comments <- doc %>%
    html_nodes("#cm_cr-review_list .review-text") %>%
    html_text()
  #Si el producto tiene menos de 10 paginas de comentarios, se verifca si la pagina n tiene comentarios y si no se hace un break en el loop
  if (purrr::is_empty(comments)==TRUE) {
    break
  }
  
  reviews <- amazon_scraper(doc, reviewer = F, delay = 2)
  reviews_all <- rbind(reviews_all, cbind(producto, reviews))
  
}

return(reviews_all)

}


tablas_prodAsin <- function(producto){
  
  # Busca los elementos cargados en el entorno de trabajo
  # con el nombre del producto (se asume que estan cargados
  # 2 archivos por producto, uno con los nombres de los productos
  # -Art_producto- y con el asin correspondiente -Asin_producto-), 
  # y les hace un column bind de manera que quede una tabla con
  # el nombre del articulo en un columna y el asin en otra
  #
  # Args:
  #   producto: nombre del producto
  #
  # Returns:
  #   La tabla arriba descrita
  
  #Como mget() no funcionaba con ls() dentro de la función, usamos la solucion propuesta en https://stackoverflow.com/questions/49196301/using-mget-within-a-function-in-r
  lista <-  mget(apropos(producto), inherits = TRUE)
  tabla <-  bind_cols(lista)
  return(tabla)
}

tabla_sentimiento <- function(tablaFiltro,tabla){
  
  # Toma la tabla original y filtrada de los productos y sus comentarios y ratings
  # (estrellas) y hace un analisis de sentimiento a los comentarios de cada uno 
  # de los productos. Finalmente calcula la media del sentimiento  de cada 
  # producto
  #
  # Args:
  #   tablaFiltro: tabla de productos, comentarios y ratings filtrada
  #   tabla : tabla de productos, comentarios y ratings original
  #
  # Returns:
  #   vector con media de sentimientos por producto
  
  a  <- list()
  a <- purrr::map(tablaFiltro$producto %>% as.vector(),
                  function(x) with(tabla[[x]], sentiment_by(comments)) %>%
                    dplyr::summarise(media_sentimiento=mean(ave_sentiment))
  )
  
  names(a) <- tabla$producto %>% as.vector()
  
  return(a)
  
}


sentimiento_bing <- function(lista){
  # Convierte la lista de productos con sus respectivos comentarios
  # y demas informacion en una tabla con los productos y su correspondiente 
  # polaridad de sentimiento.
  # En esta funcion se calcula la polaridad usando el lexicon BING
  # de la libreria tidytext. En dicho lexicon se clasifican las 
  # palabras en "positivas" o "negativas", se calcula la polaridad
  # como la suma de las palabras positivas menos las negativas.
  # Se calcula la polaridad de cada producto como la suma de las
  # polaridades de cada uno de sus comentarios.
  # Requiere las librerias tidyverse, tidytext y plyr
  
  # Args:
  #   lista: lista generada por la funcion `scrapeAsin`
  #
  # Returns:
  #   Una tabla don dos columnas: una para el nombre del producto
  #   y otra con su polaridad
  
  #Quitamos los elementos NULL de la lista (productos que no tienen comentarios)
  lista_filtrada <- plyr::compact(lista)
  
  #Creamos un vector con los nombres de los productos filtrados
  pdcto <- names(lista_filtrada)
  
  #Lista vacia para rellenar en el map
  prueba_tokens <- list()
  
  ## Se explica lo que se hace a continuación en cada linea de la funcion qeu se aplica en el map:
  # 1- convertimos en tibble para poder pasarle el unnest_tokens
  # 2- colocamos el indice como una columna aparte para saber a que comentario pertencen los tokens
  # 3- quitamos los stop words
  # 4- hacemos el inner join con el lexicon para saber que palabras positivas y negativas hay en el comentario
  # 5- contamos los sentimientos por comentario (nº de palabaras positivas o negativas)
  # 6- Se ha incluido esto y la linea posterior como solucion a este problema https://es.stackoverflow.com/questions/256001/generalizar-suma-de-columnas-con-mutate-para-frames-de-distintas-dimensiones
  # 7- lo colocamos en formato wide para que quede una columna por comentario negativo o positivo
  # 8- se calcula la polaridad como la diferencia entre comentarios positivos y negativos
  # 9- se calcula la polaridad "media" como la suma de la polaridad de los comentarios 
  
  prueba_tokens <- purrr::map(pdcto,
                              function(x) lista_filtrada[[x]]$comments %>%
                                as_tibble() %>%
                                rownames_to_column() %>%
                                unnest_tokens(word, value) %>%
                                anti_join(stop_words, by="word") %>%
                                inner_join(get_sentiments("bing"), by="word") %>%
                                count(sentiment, rowname) %>%
                                mutate(sentiment = factor(sentiment, levels = c("positive", "negative"))) %>%
                                spread(sentiment,n,fill = 0, drop = FALSE) %>%
                                mutate(polaridad=positive-negative) %>%
                                dplyr::summarise(ave_sentimiento=sum(polaridad))
                              
  )
  
  #Como la lista resultante no tiene nombres, le asignamos el nombre de cada producto
  names(prueba_tokens) <- pdcto
  
  #Convertimos la lista en un tibble donde la 1º columna es el nombre del producto y la 2º la polaridad
  sentimiento_medio <-  prueba_tokens %>%
    unlist %>%
    as.tibble() %>%
    mutate(producto=names(prueba_tokens)) %>%
    select(2:1) %>% 
    dplyr::rename(sentimientoBING = value)
  
  return(sentimiento_medio)
  
}


###Tabla de productos ----

#Se construye una tabla para cada producto con los comentarios del mismo, la fecha, la valoracion (estrellas), el formato de producto y si el comentarista es un cliente vine o normal

#Load Asins
load.Rdata(filename = "listaAsin.RData", "Asin_focos")

#Load product names
load.Rdata(filename = "listaArt.RData", "Art_focos")


nombres <- c("focos")

#Se construyen las tablas de nombres de producto y asin para cada categoria
tablas_producto <- list()
for (producto in nombres) {
  tablas_producto[[producto]] <- tablas_prodAsin(producto) 
}


#Creamos un vector de prodcutos y asines
nombres <- tablas_producto$focos[1] %>% unlist() %>%  as.vector()
asines <- tablas_producto$focos[2] %>% unlist() %>%  as.vector()

#Aplicamos la funcion scrapeAsin a los asines y nombres
review_focos <- list()
review_focos <- purrr::map2(asines,nombres, scrapeAsin)

names(review_focos) <- nombres

save(review_focos, file = "review_focos.RData")


### Analisis de sentimiento ----


load.Rdata(filename = "review_focos.RData", "review_focos")

#Filtramos y nos quedamos solo con los productos con ratings mayores a 3.5 y mas de 20 comentarios. Ordenamos de mayor a menor segun el rating

filtro_focos <- review_focos %>%
  dplyr::bind_rows() %>%
  as_tibble() %>%
  dplyr::filter(ver.purchase==1) %>% 
  dplyr::group_by(producto) %>%
  dplyr::summarise(media_star = mean(stars), numero_comments = n()) %>% 
  dplyr::filter(numero_comments >=20) %>% 
  dplyr::filter(media_star >=3.5) %>% 
  dplyr::arrange(desc(media_star)) 


#Tomamos la tabla original y filtrada de los productos y sus comentarios y ratings (estrellas) y hacemos un analisis de sentimiento a los comentarios de cada uno de los productos. Finalmente calcula la media del sentimiento 

filtro_focos$sentimiento <- tabla_sentimiento(filtro_focos, review_focos) %>%
  unlist() %>%
  as.vector()

#Ahora hacemos un analisis de sentimiento basado en unigramas (palabras) con el lexicon BING, para eso usamos la funcion sentimiento_bing

sentimentBING_focos <- sentimiento_bing(review_focos)

#Juntamos con la tabla de sentimientos anterior para comparar

filtro_focos_BING <- inner_join(sentimentBING_focos,filtro_focos, by="producto") %>%
  dplyr::select(-numero_comments) %>%
  dplyr::arrange(desc(media_star)) %>% 
  dplyr::select(1,3:4,2)

