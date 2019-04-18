# Scraping

En este repositorio encontar�s varios scraps que he realizado para obtener datos, descargar archivos, etc.

## Carpetas

### TFG F�tbol

Web scraping de la web www.transfermarkt.es, en la cual se obtienen los datos del dinero invertido en fichajes por los equipos de las 5 mayores ligas de Europa. Dicho c�digo forma parte del TFG de �lvaro Fuster del cual soy su supervisor.

Est� compuesta por dos archivos: `scrapWeb.R` que realiza el scrap y `Graficos.R` que tomando como input el archivo `ligas.csv` generado en el c�digo anterior, realiza varios gr�ficos y los guarda en el directorio de trabajo. **Es necesario definir el directorio de trabajo**.

Asimismo 5 archivos excel con datos de los derechos de televisi�n por liga.

### Amazon Reviews

Scraping de los comentarios de varios productos en Amazon. Dicho c�digo forma parte del TFG de Antonio Gonz�lez del cual soy su supervisor.

De momento est� compuesta por dos archivos: `scrapAsin.R` que para una categor�a dada obtiene todos los productos de la misma y extrae el nombre de los productos y su ASIN. 
El archivo `scrap&sentiment.R` toma las tablas generadas en el script anterior con las listas de asines y productos, las une en una sola tabla, y construye mediante un scraping una tabla por producto con los comentarios del mismo, la fecha, la valoracion (estrellas), el formato de producto y si el comentarista es un cliente vine o normal.
Posteriormente se hace un analisis de sentimiento a los comentarios de cada uno de los productos. Finalmente calcula la media del sentimiento por producto

Gran parte del an�lisis est� basado en este [post](https://justrthings.com/2016/08/17/web-scraping-and-sentiment-analysis-of-amazon-reviews/) escrito por [Riki Saito](https://github.com/rjsaito)

### R Cheatsheets

C�digo que descarga los cheat sheets de R studio `ScrapSheets.R`. Algo antiguo, habr�a que revisarlo


