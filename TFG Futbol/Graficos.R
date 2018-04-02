pacman::p_load(tidyverse, xlsx, readr,readxl,
                broom, stringr)

#setwd('')

liga <- read.csv('ligas2.csv') %>% 
  transform(clubes = as.character(clubes)) %>%
  transform(clubes = iconv(clubes, to='UTF-8')) %>% #Para poder hacer el join con derechosTV y reconozca acentos
  as_tibble() #%>% 
#  transform(clubes = str_trim(clubes))

#Cálculo de los equipos por percentiles

top84 <- liga %>%
  group_by(Liga) %>%
  mutate(cuartil84 = quantile(valorMercado, 0.84)) %>%
  filter(valorMercado >= cuartil84) %>% 
  summarise(media = mean(valorMercado)) %>% 
  arrange(desc(media)) %>% 
  as.data.frame()
write.xlsx(top84, 'Top84.xlsx', row.names = FALSE)

top85 <- liga %>%
  group_by(Liga) %>%
  mutate(cuartil85 = quantile(valorMercado, 0.85)) %>%
  filter(valorMercado >= cuartil85) %>% 
  summarise(media = mean(valorMercado)) %>% 
  arrange(desc(media))%>% 
  as.data.frame()
write.xlsx(top85, 'Top85.xlsx', row.names = FALSE)

top80 <- liga %>%
  group_by(Liga) %>%
  mutate(cuartil80 = quantile(valorMercado, 0.8)) %>%
  filter(valorMercado >= cuartil80) %>% 
  summarise(media = mean(valorMercado)) %>% 
  arrange(desc(media))%>% 
  as.data.frame()
write.xlsx(top80, 'Top80.xlsx', row.names = FALSE)


notop84 <- liga %>%
  group_by(Liga) %>%
  mutate(cuartil84 = quantile(valorMercado, 0.84)) %>%
  filter(valorMercado <= cuartil84) %>% 
  summarise(media = mean(valorMercado)) %>% 
  arrange(desc(media))%>% 
  as.data.frame()
write.xlsx(notop84, 'NoTop84.xlsx', row.names = FALSE)

notop85 <- liga %>%
  group_by(Liga) %>%
  mutate(cuartil85 = quantile(valorMercado, 0.85)) %>%
  filter(valorMercado <= cuartil85) %>% 
  summarise(media = mean(valorMercado)) %>% 
  arrange(desc(media))%>% 
  as.data.frame()
write.xlsx(notop85, 'NoTop85.xlsx', row.names = FALSE)

equiposTop84 <- liga %>%
  group_by(Liga) %>%
  mutate(cuartil84 = quantile(valorMercado, 0.84)) %>%
  filter(valorMercado >= cuartil84) %>% 
  select(clubes, valorMercado, Liga)%>% 
  as.data.frame()
write.xlsx(equiposTop84, 'TeamsTop84.xlsx', row.names = FALSE)

equiposTop85 <- liga %>%
  group_by(Liga) %>%
  mutate(cuartil85 = quantile(valorMercado, 0.85)) %>%
  filter(valorMercado >= cuartil85) %>% 
  select(clubes, valorMercado, Liga)%>% 
  as.data.frame()
write.xlsx(equiposTop85, 'TeamsTop85.xlsx', row.names = FALSE)

equiposTop80 <- liga %>%
  group_by(Liga) %>%
  mutate(cuartil80 = quantile(valorMercado, 0.80)) %>%
  filter(valorMercado >= cuartil80) %>% 
  select(clubes, valorMercado, Liga) %>% 
  as.data.frame()
write.xlsx(equiposTop80, 'TeamsTop80.xlsx', row.names = FALSE)

low45 <-  liga %>%
  group_by(Liga) %>%
  mutate(cuartil45 = quantile(valorMercado, 0.45)) %>%
  filter(valorMercado <= cuartil45) %>% 
  summarise(media = mean(valorMercado)) %>% 
  arrange(desc(media))%>% 
  as.data.frame()
write.xlsx(low45, 'Pobres.xlsx', row.names = FALSE)


#Boxplot
p <- ggplot(liga)
p <- p + aes(x=Liga, y = valorMercado) + geom_boxplot(fill='steelblue', alpha = 0.7)
p <- p + scale_y_continuous(name = "Valor de mercado") +
  scale_x_discrete(name= "Liga") + labs(caption = "Fuente: https://www.transfermarkt.es")
p <- p + ggtitle("Diagramas de Cajas de los Valores de Mercado") + 
  theme(plot.title = element_text(hjust = 0.5)) #Esto es para centrar el t?tulo
p
ggsave('cajas.jpg')


#Histrogramas
h <- ggplot(liga)
h <- h + aes(valorMercado)
h <- h + geom_histogram(fill='steelblue', col = 'black') 
h <- h + facet_wrap(~Liga) + ggtitle("Histogramas de los Valores de Mercado") +
  theme(plot.title = element_text(hjust = 0.5)) #Esto es para centrar el t?tulo
h <- h + labs(x="Valor de Mercado", y="Frecuencia", caption = "Fuente: https://www.transfermarkt.es")
h
ggsave('distribucion.jpg')



#Cargamos derechos de TV
TVSpain <- read_excel("TVSpain.xlsx", col_names = c('clubes','derechosTV'),
                      col_types = c('text', 'numeric')) %>%
  slice(1:20)
TVAlemania <- read_excel("TVAlemania.xlsx", col_names = c('clubes','derechosTV'),
                         col_types = c('text', 'numeric')) %>%
  slice(1:18)
TVFrancia <- read_excel("TVFrancia.xlsx", col_names = c('clubes','derechosTV'),
                        col_types = c('text', 'numeric')) %>%
  slice(1:20)
TVItalia <- read_excel("TVItalia.xlsx", col_names = c('clubes','derechosTV'),
                       col_types = c('text', 'numeric')) %>%
  slice(1:20)
TVInglaterra <- read_excel("TVInglaterra.xlsx", col_names = c('clubes','derechosTV'),
                           col_types = c('text', 'numeric')) %>%
  slice(1:20)



mediaSpain2 <- TVSpain %>% 
  filter(str_detect(clubes,'Alav') | str_detect(clubes,'Lega') | str_detect(clubes,'Osasu')) %>% 
  summarise(media = mean(derechosTV))
mediaSpain2

mediaFra2 <- TVFrancia %>% 
  filter(str_detect(clubes,'Nancy') | str_detect(clubes,'Dijon') | str_detect(clubes,'Metz')) %>% 
  summarise(media = mean(derechosTV))
mediaFra2

mediaIta2 <- TVItalia %>% 
  filter(str_detect(clubes,'Carpi') | str_detect(clubes,'Frosinone') | str_detect(clubes,'Bologna')) %>% 
  summarise(media = mean(derechosTV))
mediaIta2


derechosTV <- bind_rows(TVSpain,TVInglaterra,TVItalia,TVFrancia,TVAlemania)

liga2 <- left_join(liga, derechosTV, by = 'clubes')

l <- ggplot(liga2, aes(x=valorMercado, y=derechosTV)) + 
  geom_point(aes(colour=Liga), size=3)+
  labs(x = "Valor de mercado",
       y = "Derecchos de TV",
       caption = "Fuente: https://www.transfermarkt.es") +
  scale_color_brewer(palette = 'Set1')
l
ggsave('dispersion.jpg')

ll <- ggplot(liga2, aes(x=valorMercado, y=derechosTV)) + 
  geom_point(aes(shape = Liga), size=3) +
  labs(x = "Valor de mercado",
       y = "Derecchos de TV",
       caption = "Fuente: https://www.transfermarkt.es")
ll
ggsave('dispersion2.jpg')


lo <- ggplot(liga2, aes(x=valorMercado, y=derechosTV, color=Liga)) + 
  geom_point()+
  labs(x = "Valor de mercado",
       y = "Derecchos de TV",
       caption = "Fuente: https://www.transfermarkt.es") +
    scale_color_brewer(palette = 'Set1') + 
  geom_smooth(method = "gam", se=FALSE)
lo
ggsave('dispersionLine.jpg')

#Tabla con coeficientes de la linea de regresión

ligaSensibilidad <- liga2 %>% 
  nest(-Liga) %>% 
  mutate(modelos = map(data, ~lm(derechosTV~valorMercado, .))) %>% 
  mutate(tidied = map(modelos, tidy)) %>%
  unnest(tidied) %>%
  filter(term == "valorMercado") %>%
  arrange(desc(estimate)) %>%
  select(Liga, estimate)

colnames(ligaSensibilidad)[2] <- 'sensibilidad'
write.xlsx(as.data.frame(ligaSensibilidad), 'Sensibilidad.xlsx', row.names = FALSE)

Big3 <- liga2 %>% 
  group_by(Liga) %>%
  arrange(desc(valorMercado)) %>%
  slice(1:3) %>%
  select(clubes, valorMercado, derechosTV)
Big3 <- Big3[,2:4]


lp <- ggplot(liga2, aes(x=valorMercado, y=derechosTV)) + 
  geom_point(aes(colour=Liga), size=3)+
  scale_color_brewer(palette = 'Set1')+
  labs(title = "Derechos de Televisión vs Valor de los Fichajes",
       subtitle ="Se señalan los 3 equipos con mayor valor de mercado de cada liga",
       x = "Valor de mercado",
       y = "Derecchos de TV",
       caption = "Fuente: https://www.transfermarkt.es") +
  theme(plot.title = element_text(hjust = 0.5)) + #Esto es para centrar el t?tulo
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom") +
  geom_label_repel(aes(label=clubes), data = Big3)
lp
ggsave('dispersion3.jpg')


l2 <- ggplot(liga2, aes(x=valorMercado, y=derechosTV)) + 
  geom_point() + 
  labs(x = "Valor de mercado",
       y = "Derecchos de TV",
       caption = "Fuente: https://www.transfermarkt.es") +
  stat_smooth(method = 'lm', se=FALSE) + 
  facet_wrap(~Liga, scales = 'free')
l2
ggsave('dispersionLiga1.jpg')

l2e <- ggplot(liga2, aes(x=valorMercado, y=derechosTV)) + 
  geom_point() + 
  labs(x = "Valor de mercado",
       y = "Derecchos de TV",
       caption = "Fuente: https://www.transfermarkt.es") +
  stat_smooth(method = 'lm') + 
  facet_wrap(~Liga, scales = 'free')
l2e
ggsave('dispersionLiga2.jpg')

l2g <- ggplot(liga2, aes(x=valorMercado, y=derechosTV)) + 
  geom_point() + 
  labs(x = "Valor de mercado",
       y = "Derecchos de TV",
       caption = "Fuente: https://www.transfermarkt.es") +
  stat_smooth() + 
  facet_wrap(~Liga, scales = 'free')
l2g
ggsave('dispersionLiga3.jpg')
