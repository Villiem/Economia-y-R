##### Descarga de datos
setwd('./covidGraph/')
if (!dir.exists('./data')) {
        dir.create('./data')
}
url <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'

download.file(url,destfile="./data/datos.csv")

library(data.table)
library(tidyverse)
df <- read_csv('data/datos.csv')
head(df)
df_pivot <- df %>% pivot_longer(
        cols = !1:4,
        names_to = "fecha") #%>% 

df_pivot <- df_pivot %>%  
        mutate(fecha = lubridate::mdy(fecha)) %>% 
        janitor::clean_names()

df_clean <- df_pivot %>% 
        arrange(country_region, fecha) %>% 
        group_by(country_region) %>% 
        mutate(
                value = value - lag(value)
        ) %>% 
        ungroup()
paises <- tibble(pais = unique(df_clean$country_region)) # 192 paises
df_clean %>% filter(country_region%in%'US') %>% tail() ## Nomas para verificar con google

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
        ))




