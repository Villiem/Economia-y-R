library(inegiR)
library(tidyverse)
library(readxl)
library(ggthemes)
token_indica <- "Saca tu propio token"


## Segun el catalogo de INEGI, estos son las variables que podria usar.
unemp_id <- "497613"
sub_id <- "583755"
inf_id <- "583753"


datos <- inegi_series_multiple(c(unemp_id, sub_id, inf_id), token = token_indica)
View(datos)
## Un gran DF donde lo que nos interesa es el indicator_id y graficamos para ver si son lo que queremos.

datos %>% filter(meta_indicatorid == 497613) %>% ggplot(aes(x = date, y = values)) + geom_line()
datos %>% filter(meta_indicatorid == 583755) %>% ggplot(aes(x = date, y = values)) + geom_line()
datos %>% filter(meta_indicatorid == 583753) %>% ggplot(aes(x = date, y = values)) + geom_line()


inf <- datos %>% filter(meta_indicatorid == 583753) #La inflacion
Salmin <- read_excel("data/SalarioMínimoHistórico1877-2019_corteabril.xls",
sheet = "Datos")
View(Salmin) ## Los datos de salario minimom

## cambiemos los nombres
names <- c("year", "sal_min_nom", "inpc_jul_2018", "sal_min_real")

colnames(Salmin) <- names

## Tomemos los datos desde 1934 del salario minimo real.
salmin_34 <- Salmin %>% filter(year > 1934)

ggplot(salmin_34, aes(x = year, y =sal_min_real)) + geom_line() + theme_economist() 
ggplot(data = inf, aes(x = date, y = values, color = "red")) + geom_line()

## Agrupamos por anio y sacamos el promedio, pues nuestra inflacion es trimestral.
inf_anual <- inf %>%
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  group_by(year) %>%
  summarise(inf_media = mean(values))

view(inf_anual)
view(salmin_34)

salmin_70 <- salmin_34 %>% filter(year >= 1970) %>% select(year, sal_min_real)## para tener data frames identicos

## Ahora juntamos ambos data frames, arroja un error porque uno no es numeric
inf_anual <- inf_anual %>% mutate(year = as.numeric(year)) %>% rename(inf_media = media)

join_inf_salmin <- inner_join(salmin_70, inf_anual, by = "year")
view(join_inf_salmin)
## Ya tenemos en un solo df el salario minimo real y la inflacion, ahora sacaremos la tasa de crecimiento de ambos y vamos a graficar
join_inf_salmin <- join_inf_salmin %>% arrange((year)) 
join_inf_salmin <- join_inf_salmin %>% mutate(tasa_salmin = ((sal_min_real/lag(sal_min_real))-1) *100)

view(join_inf_salmin)

join_inf_salmin %>% ggplot(aes(tasa_infl, tasa_salmin)) + geom_point()  ## Sale fea la chingadera, podriamos intentar

df1 <- join_inf_salmin %>% select(year, tasa_salmin, inf_media)

df2 <-pivot_longer(df1, cols = c("tasa_salmin", "inf_media"), names_to = "variable", values_to = "values")

df2 %>% ggplot(aes(x = year, y= values, col = variable)) + geom_line() + labs(title="Salario mínimo e inflación (tasas de crecimiento)",   #Las anotaciones con la sintaxis usual de ggplot2::
                                                                            subtitle="Base 2018", 
                                                                            caption="Elaboración propia con datos de INEGI 2018\n github.com/villiem") +
  theme_wsj() + scale_y_continuous(labels=function(x) paste0(x,"%")) + scale_color_manual(labels = c("Inflación", "Salario Mínimo"), values = c("red", "blue"))



