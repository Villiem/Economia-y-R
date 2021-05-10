##### Descarga de datos
setwd('./covidGraph/')
if (!dir.exists('./data')) {
        dir.create('./data')
}
url <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'

download.file(url,destfile="./data/datos.csv")

library(data.table)
library(tidyverse)
df_covid_raw <- read_csv('data/datos.csv')

df_covid <- df_covid_raw %>% 
        pivot_longer(
        cols = !1:4,
        names_to = "fecha") %>%
        mutate(fecha = lubridate::mdy(fecha)) %>% 
        janitor::clean_names()%>% 
        arrange(country_region, fecha) %>% 
        group_by(country_region) %>% 
        mutate(casos = value - lag(value)) %>% 
        ungroup() %>% ## Podemos reemplazar los na por cero o quitarlos
        filter(!is.na(casos))



paises <- tibble(pais = unique(df_covid$country_region)) # 192 paises
#df_clean %>% filter(country_region%in%'US') %>% tail() ## Nomas para verificar con google

## Vale, en este punto lo que necesitamos es crear las 6 regiones del mapa
## Algunas son continentes y otras paises, así que vamos con las más sencillas

library(countrycode)
paises$continente <- countrycode(sourcevar = paises$pais,
            origin = "country.name",
            destination = "continent")

paises$region <- countrycode(sourcevar = paises$pais,
                             origin = "country.name",
                             destination = "region")

paises %>% filter(str_detect(region, "Middle")) %>% distinct(region) ## pense que podia haber Middle East con otras combinacion
## Hora de crear la region final
paises <- paises %>% 
        mutate(region_graf = case_when(
                str_detect(region, "Middle") ~ "Middle East and Africa",
                continente %in% "Africa" ~ "Middle East and Africa", ## No supe como hacer el str_detect en dos columnas y francamente no vale el tiempo buscarlo
                pais %in% 'US' ~ "United States",
                pais %in% 'India' ~ "India",
                continente %in% "Asia" ~ "Rest of Asia",
                TRUE ~ continente # Esto mantiene intactos americas y europa. 
        ))%>%
        filter(!region_graf%in%"Oceania" |!is.na(region_graf)) %>% # Este continente no le importo a la revista
        select(pais, region_graf)

df_graf <- left_join(paises, df_covid, by = c("pais" = "country_region"))


by_region <-
        df_graf %>%
        filter(fecha > as.Date("2020-3-1")) %>%
        # mutate(month = month(fecha), year = year(fecha)) %>%
        group_by(region_graf, fecha) %>%
        summarise(casos_totales = sum(casos, na.rm = T)) %>%
        mutate(media_movil = zoo::rollmeanr(casos_totales, 7, fill = NA, allign = "right")) %>%
        ## Nota acerca de rollmean,
        ## existen diferentes alligns, no se cual usaron en la grafica
        ## pero supondre que es el promedio de los 7 dias anteriores
        ## El default es que el promedio sea tres dias anteriores y tres posteriores
        ungroup() %>%
        filter(!is.na(media_movil) & !region_graf%in% "Oceania") %>%
        select(region_graf, fecha, media_movil)
df_final <- by_region %>% drop_na() %>%
mutate(region_graf = factor(region_graf, levels = c("Middle East and Africa",
                                                    "United States",
                                                    "Europe",
                                                    "Americas",
                                                    "Rest of Asia","India")),
       media_movil = media_movil/1e3)

