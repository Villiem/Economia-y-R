library(ggplot2)
library(ggthemes)
str(presidential)
presidentials <- subset(presidential, start > economics$date[1])
## Son dos df diferentes.
ggplot(economics) +
  geom_rect(
    aes(xmin= start, xmax = end, fill = party),
    ymin = -Inf, ymax = Inf, alpha = 0.2,
    data = presidentials) +
  geom_vline(
    aes(xintercept = as.numeric(start)),
    data = presidentials,
    color = 'grey50', alpha = 0.5) +
  geom_text(aes(x = start, y = 2500, label = name),
            data = presidentials,
            size =3, vjust = 0, hjust =0, nudge_x = 50) +
  geom_line(aes(date, unemploy)) +
  scale_fill_manual(values = c('blue', 'red'))

str(economics)
View(economics)

## Para crear el gráfico, necesito un df
## con name, start, end, partido.
# Veamos si podemos usar wikipedia
library(rvest)
url <- 'https://es.wikipedia.org/wiki/Anexo:Gobernantes_de_M%C3%A9xico'
h<- read_html(url)
tab <- h %>% html_nodes('table')
View(tab)
## nos interesa la tabla 30 y 31
pres <- tab[[30]] %>% html_table()
pres2 <- tab[[31]] %>% html_table()

pres <- rbind(pres, pres2)
View(pres)
## Ya tenemos los presidentes pero tenemos que hacer limpieza, separar las fechas
pres <- pres[,c(3,6,7)]
## separamos inicio y final
library(tidyverse)
pres <- pres %>% separate(Período, c('inicio', 'final'), '-')
view(pres)
## Funciona pero tenemos que hacer bien las fechas.
## Nuestra estrategia será aprovechar que todas las fechas son iguales excepto en el año
x <- str_sub(pres$inicio, start = -4)
y <- str_sub(pres$final, start = -4)
## Si tuvieramos más columnas utilizaría un sapply 
## Pero al ser solamente dos está bien.
x <- paste0(x, '-12-01')
y <- paste0(y, '-11-30')
## y finalmente pegamos y declaramos como fecha
pres <- pres %>% mutate(
  inicio = as.Date(x),
  final = as.Date(y)
)
head(pres)
str(pres)
## Ya empieza a tomar forma
## lo último que queremos son las iniciales
## Esto lo haré con un df en lugar de un case_when
par <- unique(pres$Partido)
## Esperaba 3 pero parece que fui un poco lejos con los presidentes
## Da igual solo son dos siglas más
siglas <- tibble(
  Partido = par,
  siglas = c('PNR', 'PRM', 'PRI', 'PAN', 'MORENA')
)
pres <- pres %>% left_join(siglas, by = 'Partido')
view(pres)

# aquí me di cuenta que sería útil las siglas en lugar de los nombres y
# que el último valor de final era NA
siglas_presidentes <- str_extract_all(pres$Presidente, "[A-Z]+" ) %>% 
  lapply(paste, collapse = '') %>%
  unlist()

pres <- pres %>% mutate(
  final = if_else(is.na(final), Sys.Date(),final),
  siglas_presidentes = siglas_presidentes
)
## por fin terminamos

#Ahora solo queda obtener los datos de cualquier variable y graficar
install.packages("siebanxicor")
library(siebanxicor)
library(ggalt)
setToken('915022593c7ce68c87708913d1733c25479f0df604da9a3840de32568a5834b3')
deuda <- getSeriesData('SG195', startDate = '1980-01-01', endDate = '2020-05-30')
deuda <- getSerieDataFrame(deuda, 'SG195')
view(deuda)


presidential <- subset(pres, inicio > deuda$date[1])
p<-ggplot(deuda) +
  geom_rect(
    aes(xmin= inicio, xmax = final, fill = siglas),
    ymin = -Inf, ymax = Inf, alpha = 0.2,
    data = presidential) +
  geom_vline(
    aes(xintercept = as.numeric(inicio)),
    data = presidential,
    color = 'grey50', alpha = 0.5) +
  geom_text(aes(x = inicio, y = 2500, label = siglas_presidentes),
            data = presidential,
            size =3, vjust = 0, hjust =0, nudge_x = 50) +
  geom_line(aes(date, value)) +
  scale_fill_manual(values = c('red4', 'blue','red')) +
  geom_text(
    aes(x = as.Date("2000-01-21"), y = 3500, label = "github.com/villiem"),
    size = 3, vjust = 0, hjust = 0, color = "forestgreen", alpha = 0.2
  )+
  labs(
    title = 'Deuda Externa Neta del Sector Público Económico Amplio, \nSaldos al final del periodo',
    subtitle = 'Miles de Millones de Pesos',
    caption = 'Elaboración propia con datos de Banxico',
    x = 'Fecha',
    y = 'Deuda') + theme_economist() + guides(fill=guide_legend(title="Partido:"))

p1 <- p + annotate("text", x = as.Date('2008-01-01'), y = 100, label = "Ley del ISSSTE", size = 3)+
  theme(plot.caption = element_text(size=18, hjust=0.5, face="italic", color="black"))
?ggsave

ggsave('gráfica_deuda_presidentes.jpeg', plot = p1)
