# Scraping

En este repositorio encontarás varios scraps que he realizado para obtener datos, descargar archivos, etc.

## Carpetas

### TFG Fútbol

Web scraping de la web www.transfermarkt.es, en la cual se obtienen los datos del dinero invertido en fichajes por los equipos de las 5 mayores ligas de Europa. Dicho código forma parte del TFG de Álvaro Fuster del cual soy su supervisor.

Está compuesta por dos archivos: `scrapWeb.R` que realiza el scrap y `Graficos.R` que tomando como input el archivo `ligas.csv` generado en el código anterior, realiza varios gráficos y los guarda en el directorio de trabajo. **Es necesario definir el directorio de trabajo**.

Asimismo 5 archivos excel con datos de los derechos de televisión por liga.

### Amazon Reviews

Scraping de los comentarios de varios productos en Amazon. Dicho código forma parte del TFG de Antonio González del cual soy su supervisor.

De momento está compuesta por dos archivos: `scrapAsin.R` que para una categoría dada obtiene todos los productos de la misma y extrae el nombre de los productos y su ASIN. 
El archivo `scrap&sentiment.R` toma las tablas generadas en el script anterior con las listas de asines y productos, las une en una sola tabla, y construye mediante un scraping una tabla por producto con los comentarios del mismo, la fecha, la valoracion (estrellas), el formato de producto y si el comentarista es un cliente vine o normal.
Posteriormente se hace un analisis de sentimiento a los comentarios de cada uno de los productos. Finalmente calcula la media del sentimiento por producto

Gran parte del análisis está basado en este [post](https://justrthings.com/2016/08/17/web-scraping-and-sentiment-analysis-of-amazon-reviews/) escrito por [Riki Saito](https://github.com/rjsaito)

### R Cheatsheets

Código que descarga los cheat sheets de R studio `ScrapSheets.R`. Algo antiguo, habría que revisarlo


