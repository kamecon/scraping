# Scraping

En este repositorio encontarás varios scraps que he realizado para obtener datos, descargar archivos, etc.

## Carpetas

### TFG Fútbol

Web scraping de la web www.transfermarkt.es, en la cual se obtienen los datos del dinero invertido en fichajes por los equipos de las 5 mayores ligas de Europa. Dicho código forma parte del TFG de Álvaro Fuster del cual soy su supervisor.

Esta compuesta por dos archivos: `scrapWeb.R` que realiza el scrap y `Graficos.R` que tomando como input el archivo `ligas.csv` generado en el código anterior, realiza varios gráficos y los guarda en el directorio de trabajo. **Es necesario definir el directorio de trabajo**.

Asimismo 5 archivos excel con datos de los derechos de televisión por liga.

### Amazon Reviews

Scraping de los comentarios de varios productos en Amazon. Dicho código forma parte del TFG de Antonio González del cual soy su supervisor.

De momento hay un script `scrapAsin.R` que para una categoría dada obtiene todos los productos de la misma y extrae el nombre de los productos y su ASIN.

### R Cheatsheets

Código que descarga los cheat sheets de R studio `ScrapSheets.R`. Algo antiguo, habría que revisarlo


