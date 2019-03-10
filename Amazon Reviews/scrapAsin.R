url <- "https://www.amazon.com/s?k=GARDEN&i=tools-intl-ship&bbn=256643011&rh=n%3A256643011%2Cn%3A468240%2Cn%3A495224%2Cn%3A495236%2Cn%3A495238&dc&page=2&__mk_es_US=%C3%85M%C3%85%C5%BD%C3%95%C3%91&qid=1551714309&rnid=495236&ref=sr_pg_2"


doc <- read_html(url)
numero <- doc %>%
  html_nodes(".s-precache-url , .a-disabled") %>%
  html_text()
#paginas <- numero[3] %>%
paginas <- numero %>%
  tail(1) %>%
  as.numeric()

lista <- vector()

for (pagina in 1:paginas) {
  locprov <- str_locate(string = url, pattern = "page=")
  locprov2 <- locprov[2]
  if (pagina <=10) {
    str_sub(url, (locprov2+1),(locprov2+1)) <- as.character(pagina)
  } else {
    str_sub(url, (locprov2+1),(locprov2+2)) <- as.character(pagina)
  }
  print(url)
  articulos <- read_html(url) %>%
    html_nodes(".a-text-normal") %>%
    html_attr("href")
  articulos2 <- articulos[5:length(articulos)] %>%
    na.omit()
  #En articulos2 aparecen los links repetidos 2 veces
  #Con esto se eliminan los numeros impares, con esto eliminamos las repeticiones
  articulos3 <- articulos2[c(TRUE,FALSE)] 
  lista <- c(lista, articulos3)
}
  
#Extraemos los asin de la lista de articulos. Sabemos que son los 10 números que estan luego del caracter 'dp/'
listaAsin <- lista %>%
  purrr::map(function(x) str_split(x, "dp/") %>% #esto crea una lista con los elementos del split
               pluck(2) %>%  #sacamos el segundo elemento
               substr(1,10) %>% #nos quedamos con las 10 primeros string
               unlist() %>% #convertimos en vector
               as.vector()  
             )

#Se hace algo similar pero más sencillo para hacer un vector de nombre de los articulos
listaArt <- lista %>%
  str_split("/") %>%
  pluck(2) %>%
  unlist() %>%
  as.vector()

save(lista, file = "lista.RData")
save(listaAsin, file = "listaAsin.RData")
save(listaArt, file = "listaArt.RData")

