# Haciendo la grafica, admito que no soy muy detallista
# y no es mi fuerte
library(scales)
library(ggrepel)
p1 <- ggplot(df_final, aes(x = fecha, y = media_movil, fill = region_graf)) + 
        geom_area() +
        labs(title = "India now accounts for over a third of all\nconfirmed covid-19 cases in the world",
             subtitle = "New confirmed covid-19 cases, by country-region, \'000 \nSeven-day moving average",
             caption = "Source: Johns Hopkins University CSSE")

# EL acercamiento de los colores que usaron
# https://pattern-library.economist.com/color.html

colores <- c('#91b8bd',
            "#acc8d4",
            '#d4dddd',
            '#8abbd0',
            '#D7D7D7',
            '#f51b00')
p1 <- p1 + scale_y_continuous(position = "right") +
        scale_fill_manual(values = colores) + theme(panel.grid.minor = element_blank(), 
                                                 panel.grid.major = element_line(color = "gray50", size = 0.5),
                                                 panel.grid.major.x = element_blank(),
                                                 panel.background = element_blank(),
                                                 line = element_blank()) +
        scale_x_date(date_breaks = "month", labels = label_date_short(format = c("%Y","%b"), sep = "\n"))

# Definitivamente se puede mejorar, pero ya es noche

p2 <- p1 + 
        theme_economist_white(gray_bg = F) +
        theme(
        plot.title = element_text(colour = "red", size = 20),
        plot.subtitle = element_text(face = "bold", hjust = 0),
        legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.caption = element_text(hjust = 0))

### Agregando las etiquetas

labels <- tibble(
        etiqueta = c("Middle East and Africa",
                  "United States",
                  "Europe",
                  "Americas",
                  "Rest of Asia",
                  "India"), ## Deberia dejar de repetir tanto esto
        fecha = as.Date(c("2020-11-15", "2021-1-1", "2021-1-1", "2021-1-1", "2020-6-1", "2020-9-1")),
        altura = c(550,450,250,120,15,15)) 

p2 + geom_label_repel(aes(x = fecha, y = altura, label = etiqueta),
            data = labels,
            size =6, vjust = 0, hjust =0, nudge_x = -50,nudge_y = 10, inherit.aes = F,
            arrow =  arrow(length = unit(.01, "npc"))) +
        ### Falta mi marcota de agua, como chingados no
        annotate("text", x = as.Date('2020-5-1'), y = 500, label = "github.com/villiem", alpha = .2, size = 10)

