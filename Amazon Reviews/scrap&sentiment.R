###Carga de paquetes ----

if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
pacman::p_load(tidyverse, miceadds, xlsx)
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

