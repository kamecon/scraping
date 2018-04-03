#CODIGO PARA DESCARGAR LOS CHEAT SHEETS DE RSTUDIO


## Paquetes empleados ----
PAQUETES <- c("pacman")
inst <- match(PAQUETES, .packages(all=TRUE))
need <- which(is.na(inst))
if (length(need) > 0) install.packages(PAQUETES[need])

pacman::p_load(rvest, XML)

## Scraping

url.macro <- "https://www.rstudio.com/resources/cheatsheets/"
tmp <- read_html(url.macro)
#Defino la ruta de descarga
download.folder = 'c:/Users/Kamal/Documents/R/Cheat_sheets/'
docu <- xmlParse(tmp)
#Buscar los href
docu.links <- xpathSApply(docu, "//a/@href")
#Buscar los pdf
archivo.url <- as.character(docu.links[grep('pdf', docu.links)])

#Primero se descargan solo los cheat sheets en español

#Buscamos los pdf's en español
archivo.url2 <- as.character(archivo.url[grep('spanish', archivo.url)])

span.url <- rep(0,length(archivo.url2))
for (i in 1:((length(archivo.url2)-1))){
  span.url[i] <-  gsub('https://www.rstudio.com/wp-content/uploads/2015/03/','', archivo.url2[i])
}

span.url[length(archivo.url2)] <- gsub('https://www.rstudio.com/wp-content/uploads/2015/04/','', archivo.url2[length(archivo.url2)])

#Para que la descarga de los pdf se haga de manera correcta se ha colocado mode = 'wb'
for (i in 1:length(archivo.url2)) {
  pp <- archivo.url2[i]
  pp.name <- paste0(download.folder, span.url[i])
  download.file(pp, pp.name, method = 'auto', quiet = FALSE, mode = "wb",
                cacheOK = TRUE, extra = getOption("download.file.extra"))
} 


cheat.url <- unique(archivo.url[1:23])

cheat.url2 <- rep(0,length(cheat.url))
for (i in 1:length(cheat.url)){
  cheat.temp <-  strsplit(cheat.url[i], split = '/')
  cheat.url2[i] <-  cheat.temp[[1]][8]
}

for (i in 1:length(cheat.url)) {
  pp <- cheat.url[i]
  pp.name <- paste0(download.folder, cheat.url2[i])
  download.file(pp, pp.name, method = 'auto', quiet = FALSE, mode = "wb",
                cacheOK = TRUE, extra = getOption("download.file.extra"))
}

