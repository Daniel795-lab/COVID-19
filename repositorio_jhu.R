#####1. IMPORT PACKAGES #####----
#MANIPULACION Y VISUALIZACION DE DATOS
rm(list= ls())
#install.packages("tidyverse")
#install.packages("ggrepel")
#install.packages("lubridate")
#install.packages("shiny")
#install.packages("htmlwidgets")
library(tidyverse) #es un universo de paquetes que integra ggplot, stringR, dplyr, tidyR(LONG-WIDE)
library(lubridate) #sirve para manipular y darle un data.type a las fechas.
library(ggrepel) #mejorar el etiquetado de los datos en un gr?fico
library(shiny) #para utilizar los colores de Colour Picker. "colourpicker" paquete de colores que almacena "shiny"
library(htmlwidgets)
setwd("C:/Users/Daniel/Desktop/COVID-19")
getwd()

#####2. GET AND CHECK DATA- Cargar y chequear los datos (Johns Hopkins University Virtual Repository COVID-19) #####----
url_confirmed <-'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
url_deaths <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv'
url_recovered <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv'

#read_csv: proveniente del paquete ReadR que se carga con Tidyverse (incluye listado con atributos del data.type de las variables, mejorando el manejo de los datos)
covid_confirmed <- read_csv(url_confirmed)
covid_deaths <- read_csv(url_deaths)
covid_recovered <- read_csv(url_recovered)
#Para ser guardada en el sistema operativo debe ocuparse la funci?n write.csv

glimpse(covid_confirmed)
summary(covid_confirmed)
sum(is.na(covid_confirmed)) #forma r?pida para conocer si el objeto tiene nulos


#####3. RENAME COLUMNS #####----
covid_confirmed <- covid_confirmed %>%
  rename(subregion = 'Province/State',
         country = 'Country/Region',
         lat = Lat,
         long = Long)

view(covid_confirmed)


covid_deaths <- covid_deaths %>%
  rename(subregion = 'Province/State',
         country = 'Country/Region',
         lat = Lat,
         long = Long)

covid_recovered <- covid_recovered %>%
  rename(subregion = 'Province/State',
         country = 'Country/Region',
         lat = Lat,
         long = Long)

#####4. WIDE to LONG #####----

covid_confirmed <- covid_confirmed %>%
  pivot_longer(cols = -c("country", "subregion", "lat", "long"),
               names_to = "date",
               values_to = "cases")

view(covid_confirmed)


covid_deaths <- covid_deaths %>%
  pivot_longer(cols = -c("country", "subregion", "lat", "long"),
               names_to = "date",
               values_to = "cases")

covid_recovered <- covid_recovered %>%
  pivot_longer(cols = -c("country", "subregion", "lat", "long"),
               names_to = "date",
               values_to = "cases")
View(covid_recovered)

#####5. CONVERT DATES #####----

typeof(covid_confirmed$date)
glimpse(covid_confirmed)

covid_confirmed <- covid_confirmed %>%
  mutate(date = mdy(date))

str(covid_confirmed)
glimpse(covid_confirmed)

covid_deaths <- covid_deaths %>%
  mutate(date = mdy(date))

covid_recovered <- covid_recovered %>%
  mutate(date = mdy(date))

str(covid_confirmed)
str(covid_deaths)
str(covid_recovered)

#####6. REORDER COLUMNS#####----

covid_confirmed <- covid_confirmed %>%
  select(country, subregion, everything())

covid_deaths <- covid_deaths %>%
  select(country, subregion, everything())
str(covid_deaths)

covid_recovered <- covid_recovered %>%
  select(country, subregion, everything())

#####7. CALCULATE DAILY CASES BY TYPE (Casos diarios por tipo)#####----

covid_confirmed <- covid_confirmed %>%
  mutate(layer = "confirmed") %>%
  arrange(country, subregion, date) %>% #ordena los datos en orden ascendente desde los datos mas tardios y al ?ltimo los casos m?s recientes
  group_by(country, subregion) %>% #reagrupa nuevamente la tabla en base a pais y subregion
  mutate(daily_cases_by_layer = cases - lag(cases)) %>% #datos diarios=casos acumulados dia - casos registrados dia anterior
  ungroup()

covid_confirmed %>%
  filter(country == "Chile") %>%
  view()

covid_deaths <- covid_deaths %>%
  mutate(layer = "deaths") %>%
  arrange(country, subregion, date) %>%
  group_by(country, subregion) %>%
  mutate(daily_cases_by_layer = cases - lag(cases)) %>%
  ungroup()

covid_deaths %>%
  filter(country== "Chile") %>%
  view

covid_recovered <- covid_recovered %>%
  mutate(layer = "recovered") %>%
  arrange(country, subregion, date) %>%
  group_by(country, subregion) %>%
  mutate(daily_cases_by_layer = cases - lag(cases)) %>%
  ungroup()

covid_recovered %>%
  filter(country=="Chile") %>%
  view()

#####8. JOIN DATA#####----

### BIND ROWS
covid_data <- bind_rows(covid_confirmed, covid_deaths, covid_recovered)
view(covid_data)


#####9. DATA VISUALISATION#####----

### TOTAL DE CASOS ACUMULADOS GLOBAL
# combinamos %>%  con ggplot para agilizar el proceso
(a <- covid_data %>%
   filter(layer == "confirmed") %>%
   group_by(date) %>%
   summarise(n = sum(cases)/1000000) %>%   #mostramos las cifras en millones
   #summarise(n = log(sum(cases))) %>%   #podemos aplicar una escala logaritmica
   ggplot(aes(x = date,y = n)) +
   geom_line(colour ="#00CD66") +
   geom_point(colour = "#008B45") +
   xlab(NULL) +
   ylab("(en millones) \n") +
   labs(title = "Total de casos acumulados",
        subtitle = paste0("Fecha actualizacion: ", Sys.Date()-1)) + #toma el dia anterior al de hoy
   theme_minimal() +  #Plantilla "Minimal"
   theme(legend.title = element_blank())) #eliminar la leyenda del gr?fico
#Sys.Date: dia de hoy (se le resta un dia)
ggsave(paste("total_global",".jpeg", sep = ""),
       plot = a, width = 9.69, height = 6.55, units = c("in"), dpi = 500)


# Tambi?n se puede generar una tabla resumen para
# para no usar los datos directamente de la base en bruto
plot_1 <- covid_data %>%
  filter(layer == "confirmed")  %>%
  group_by(date) %>%
  summarise(n = sum(cases)/1000000)

(ggplot(plot_1, aes(x=date,y= n)) +
    geom_line(colour ="#00CD66") +
    geom_point(colour = "#008B45") +
    xlab(NULL) +
    ylab("(en millones) \n") +
    labs(title = "Total de casos acumulados",
         subtitle = paste0("Fecha actualizaci?n: ", Sys.Date()-1)) +
    theme_minimal() +
    theme(legend.title = element_blank()))


### TOTAL DE CASOS ACUMULADOS GLOBAL, POR TIPO
# usamos los datos directamente de covid_data

# definici?n colores manuales
manual_colours <- c("#4876FF", "#FF6A6A", "#00CD66")

(b <- covid_data %>%
    group_by(layer, date) %>%
    summarise(n = sum(cases)/1000000) %>%
    ggplot(aes(x = date,y = n, group = layer, colour = layer)) +
    geom_line() +
    geom_point() +
    scale_colour_brewer(palette = "Set2") +            #usamos la paleta "Set2" de colorbrewer
    #scale_colour_manual(values = manual_colours) +     #definimos manualmente los colores
    xlab(NULL) + #remueve la etiqueta del eje x
    ylab("(en millones) \n") +
    labs(title = "Casos acumulados por tipo",
         subtitle = paste0("Fecha actualizaci?n: ", Sys.Date()-1)) +
    theme_minimal() +
    theme(legend.title = element_blank()))

ggsave(paste("total_global_tipo",".jpeg", sep = ""),
       plot = b, width = 9.69, height = 6.55, units = c("in"), dpi = 500)



# Hacemos facetting para crear un gr?fico por tipo
(c <- covid_data %>%
    group_by(layer, date) %>%
    summarise(n = sum(cases)/1000000) %>%
    ggplot(aes(date, n, group = layer, colour = layer)) +
    geom_line() +
    geom_point() +
    #scale_colour_brewer(palette = "Set2") +
    #scale_colour_manual(values = manual_colours) +
    xlab(NULL) +
    ylab("(en millones) \n") +
    labs(title = "Casos acumulados por tipo",
         subtitle = paste0("Fecha actualizacion: ", Sys.Date()-1)) +
    theme_minimal() +
    theme(legend.title = element_blank()) +
    #theme(legend.position = "bottom") +   #podemos mover la leyenda abajo
    facet_wrap(~ layer))

ggsave(paste("total_global_tipo_facetting",".jpeg", sep = ""),
       plot = c, width = 9.69, height = 6.55, units = c("in"), dpi = 500)



### TOTAL DE CASOS ACUMULADOS (LATAM)

#se define vector con nombre de pa?ses
latam <- c("Argentina", "Chile", "Brazil", "Peru", "Bolivia", "Paraguay",
           "Uruguay", "Ecuador", "Venezuela", "Colombia", "Honduras",
           "Costa Rica", "Cuba", "El Salvador", "Guatemala", "Mexico",
           "Nicaragua", "Panama")

#Se crea tabla resumen de paises LATAM a plotear
latam_n <- covid_data %>%
  filter(country %in% latam & layer == "confirmed") %>%
  group_by(date) %>%
  summarise(n = sum(cases)) %>%
  filter(n > 0) %>% #se excluyen fechas sin casos confirmados
  arrange(date)

(d <- ggplot(latam_n, aes(date, n/1000)) +
    geom_line(colour ="#00CD66") +
    geom_point(colour = "#008B45") +
    xlab(NULL) +
    ylab("(en miles) \n") +
    labs(title = "Total de casos acumulados - LATAM",
         subtitle = paste0("Fecha actualizaci?n: ", Sys.Date()-1)) +
    scale_y_continuous(limits = c(0,300), breaks = c(0,100,200,300)) +
    theme_minimal())

ggsave(paste("total_latam",".jpeg", sep = ""),
       plot = d, width = 9.69, height = 6.55, units = c("in"), dpi = 500)


### CASOS ACUMULADOS POR TIPO - LATAM

#Se crea tabla resumen a plotear
latam_n_tipos<- covid_data %>%
  filter(country %in% latam) %>%
  group_by(layer, date) %>%
  summarise(n = sum(cases)) %>%
  filter(n > 0) %>%
  arrange(layer, date)

# definici?n colores manuales
manual_colours <- c("#4876FF", "#FF6A6A", "#00CD66")
?scale_color_gradient
(e <- ggplot(latam_n_tipos,
             aes(date, n/1000,         #mostramos los casos en miles
                 group = layer, colour = layer)) +
    geom_line() +
    geom_point() +
    #scale_colour_brewer(palette = "Set2") +
    scale_colour_manual(values = manual_colours) +
    xlab(NULL) +
    ylab("(en miles) \n") +
    labs(title = "Casos acumulados por tipo - LATAM",
         subtitle = paste0("Fecha actualizacion: ", Sys.Date()-1)) +
    theme_minimal() +
    theme(legend.title = element_blank()))

ggsave(paste("total_global_tipo_latam",".jpeg", sep = ""),
       plot = e, width = 9.69, height = 6.55, units = c("in"), dpi = 500)


### N?MERO DE CASOS ACUMULADOS - x PAIS (>10 K)

#cuales son los pa?ses con m?s casos acumulados a la fecha?
covid_data %>%
  filter(country %in% latam & layer == "confirmed") %>%
  group_by(country) %>%
  summarise(sum_n = max(cases)) %>% #Para saber el valor maximo de casos por pais
  arrange(desc(sum_n)) %>%
  view()


#vector con paises top5
latam_top <- c("Brazil", "Peru", "Ecuador", "Mexico", "Chile")

#Se crea tabla resumen a plotear
latam_top5 <- covid_data %>%
  filter(country %in% latam_top & layer == "confirmed") %>%
  group_by(country, date) %>%
  summarise(n = sum(cases)) %>%
  filter(n > 0) %>%
  arrange(country, date)



(f <- ggplot(latam_top5, aes(x= date,y= n/1000,
                             group = country, colour = country)) +
    geom_line() +
    geom_point() +
    xlab(NULL) +
    ylab("(en miles) \n") +
    labs(title = "Casos acumulados top 5 - LATAM",
         subtitle = paste0("Fecha actualizaci?n: ", Sys.Date()-1)) +
    geom_label_repel(data = filter(latam_top5, date == max(date)), #etiqueta del pais se agregue a la curva en el punto final
                     aes(label = country)) +
    theme_minimal() +
    theme(legend.position="None")) #saco la leyenda


ggsave(paste("total__tipo_latam5",".jpeg", sep = ""),
       plot = f, width = 9.69, height = 6.55, units = c("in"), dpi = 500)


### Chile

color_manual <- c("royalblue1", "royalblue4")

(h <- covid_data %>%
    filter(country == "Chile" & layer == "confirmed" & cases > 0) %>%
    ggplot(aes(date, cases/1000, group = layer, colour = layer)) +
    geom_line() +
    geom_point() +
    scale_colour_manual(values = color_manual) +
    xlab(NULL) +
    ylab("(en miles) \n") +
    labs(title = "Casos acumulados en Chile",
         subtitle = paste0("Fecha actualizaci?n: ", Sys.Date()-1)) +
    theme_minimal() +
    theme(legend.title = element_blank()))

ggsave(paste("total_casos_confirmados_chile",".jpeg", sep = ""),
       plot = h, width = 9.69, height = 6.55, units = c("in"), dpi = 500)


#####10. Algunos ejercicios :D #####----
covid_data_Chile <- covid_data %>% filter(country == "Chile" & cases > 0)
view(covid_data_Chile)

estadis_casos_chile_tipo <- covid_data_Chile %>%
  group_by(country, layer) %>%
  summarise(n_casos = sum(daily_cases_by_layer)) %>%
  ungroup() %>%
  view
# Casos acumulados por tipo en Chile (en 1 gr?fico)
color_manual2 <- c("#FF3030", "#FFD700", "#7FFF00")
(i <- covid_data_Chile %>%
    group_by(layer, date) %>%
    summarise(n = sum(cases)/1000) %>%
    ggplot(aes(x = date,y = n, group = layer, colour = layer)) +
    geom_line() +
    geom_point() +
    scale_colour_manual(values = color_manual2) +
    #scale_colour_brewer(palette = "Set2") #usamos la paleta "Set2" de colorbrewer
    #scale_colour_manual(values = manual_colours) + #definimos manualmente los colores
    xlab(NULL) + #remueve la etiqueta del eje x
    ylab("(en miles) \n") +
    labs(title = "Casos acumulados por tipo en Chile",
         subtitle = paste0("Fecha actualizacion: ", Sys.Date()-1)) +
    theme_minimal() +
    theme(legend.title = element_blank()))

ggsave(paste("total_casos_tipo_chile",".jpeg", sep = ""),
       plot = i, width = 9.69, height = 6.55, units = c("in"), dpi = 500)
# Casos acumulados confirmados en Chile
Chile_confirmados <- covid_data %>%
  filter(country == "Chile" & layer == "confirmed" & cases > 0) %>%
  group_by(country, date) %>%
  summarise(n = sum(cases)) %>%
  filter(n > 0) %>%
  arrange(country, date)

(j <- ggplot(Chile_confirmados, aes(x= date,y= n/1000,
                                    group = country, colour = country)) +
    geom_line() +
    geom_point() +
    xlab(NULL) +
    ylab("(en miles) \n") +
    labs(title = "Casos confirmados en Chile",
         subtitle = paste0("Fecha actualizaci?n: ", Sys.Date()-1)) +
    geom_label_repel(data = filter(Chile_confirmados, date == max(date)), #etiqueta del pais se agregue a la curva en el punto final
                     aes(label = country)) +
    theme_minimal() +
    theme(legend.position="None")) #saco la leyenda

ggsave(paste("total_confirmados_chile",".jpeg", sep = ""),
       plot = j, width = 9.69, height = 6.55, units = c("in"), dpi = 500)
# Casos fallecidos en Chile
Chile_fallecidos <- covid_data %>%
  filter(country == "Chile" & layer == "deaths" & cases > 0) %>%
  group_by(country, date) %>%
  summarise(n = sum(cases)) %>%
  filter(n > 0) %>%
  arrange(country, date)

(k <- ggplot(Chile_confirmados, aes(x= date,y= n,
                                    group = country, colour = country)) +
    geom_line() +
    geom_point() +
    xlab(NULL) +
    ylab("(en miles) \n") +
    labs(title = "Casos fallecidos en Chile",
         subtitle = paste0("Fecha actualizaci?n: ", Sys.Date()-1)) +
    geom_label_repel(data = filter(Chile_fallecidos, date == max(date)), #etiqueta del pais se agregue a la curva en el punto final
                     aes(label = country)) +
    theme_minimal() +
    theme(legend.position="None")) #saco la leyenda

ggsave(paste("total_fallecidos_chile",".jpeg", sep = ""),
       plot = k, width = 9.69, height = 6.55, units = c("in"), dpi = 500)
# Casos confirmados en Chile, Espa?a, Italia y UK
unique(covid_data$country)
chesituk <- c("Chile", "Spain", "Italy", "United Kingdom")

chesituk4 <- covid_data %>%
  filter(country %in% chesituk & layer == "confirmed") %>%
  group_by(country, date) %>%
  summarise(n = sum(cases)) %>%
  filter(n > 0) %>%
  arrange(country, date)

(l <- ggplot(chesituk4, aes(x= date,y= n/1000,
                            group = country, colour = country)) +
    geom_line() +
    geom_point() +
    xlab(NULL) +
    ylab("(en miles) \n") +
    labs(title = "Casos confirmados: Chile, Espa?a, Italia y UK",
         subtitle = paste0("Fecha actualizaci?n: ", Sys.Date()-1)) +
    geom_label_repel(data = filter(chesituk4, date == max(date)), #etiqueta del pais se agregue a la curva en el punto final
                     aes(label = country)) +
    theme_minimal() +
    theme(legend.position="None")) #saco la leyenda
ggsave(paste("Casos confirmados en Chile, Espa?a, Italia y UK",".jpeg", sep = ""),
       plot = l, width = 9.69, height = 6.55, units = c("in"), dpi = 500)

# Casos fallecidos en Chile, Espa?a, Italia y UK
chesituk5 <- covid_data %>%
  filter(country %in% chesituk & layer == "deaths") %>%
  group_by(country, date) %>%
  summarise(n = sum(cases)) %>%
  filter(n > 0) %>%
  arrange(country, date)

(m <- ggplot(chesituk5, aes(x= date,y= n/1000,
                            group = country, colour = country)) +
    geom_line() +
    geom_point() +
    xlab(NULL) +
    ylab("(en miles) \n") +
    labs(title = "Casos fallecidos: Chile, Espa?a, Italia y UK",
         subtitle = paste0("Fecha actualizaci?n: ", Sys.Date()-1)) +
    geom_label_repel(data = filter(chesituk5, date == max(date)), #etiqueta del pais se agregue a la curva en el punto final
                     aes(label = country)) +
    theme_minimal() +
    theme(legend.position="None")) #saco la leyenda
ggsave(paste("Casos fallecidos en Chile, Espa?a, Italia y UK",".jpeg", sep = ""),
       plot = m, width = 9.69, height = 6.55, units = c("in"), dpi = 500)
