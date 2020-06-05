library(siebanxicor)
library(tidyverse)

deuda <- getSeriesData('SE28647', startDate = '1980-01-01', endDate = '2020-05-30')
deuda <- getSerieDataFrame(deuda, 'SE28647')
deuda <- deuda %>% filter(date >= as.Date('1995-01-01'))
presidential <- subset(pres, final > deuda$date[1])

p<-ggplot(deuda) +
geom_rect(
aes(xmin= inicio, xmax = final, fill = siglas),
ymin = -Inf, ymax = Inf, alpha = 0.2,
data = presidential) +
geom_vline(
aes(xintercept = as.numeric(inicio)),
data = presidential,
color = 'grey50', alpha = 0.5) +
geom_text(aes(x = inicio, y = 450, label = siglas_presidentes),
data = presidential,
size =3, vjust = 0, hjust =0, nudge_x = 50) +
geom_line(aes(date, value)) +
scale_fill_manual(values = c('red4', 'blue','red')) +
geom_text(
aes(x = as.Date("2008-01-21"), y = 250, label = "github.com/villiem"),
size = 3, vjust = 0, hjust = 0, color = "forestgreen", alpha = 0.2
)+
labs(
title = 'Remesas Familiares Promedio Total',
subtitle = 'Dólares mensuales ',
caption = 'Elaboración propia con datos de Banxico',
x = '',
y = 'Dólares') + theme_economist() + guides(fill=guide_legend(title="Partido:")) +
  theme(plot.caption = element_text(size=18, hjust=0.5, face="italic", color="black")) +
  scale_x_date(date_breaks = '2 years', date_labels = "%Y")

