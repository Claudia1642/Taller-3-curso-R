# Taller-3-curso-R
# taller3 ----

## Claudia Garzon - Diego Villaizan


## 1 -----
library(tidyverse)
install.packages("naniar")
library(naniar)
all_csv_sorted<- read_csv("C:/Users/HP/Downloads/archive/all_csv sorted.csv")
datos<-all_csv_sorted
colnames(datos)
nrow(datos)
Primero_10<-datos %>%
  head(10)
## 2 ----


datos_nulos<-data.frame(miss_var_summary(datos))
datos_nulos
## 3 ----


cambio_porcentual<- datos %>%
  mutate(Cambio_porcentual = (as.numeric(`Average price of 1GB (USD  at the start of 2021)`)/as.numeric(`Average price of 1GB (USD – at start of 2020)`))*100 - 100)%>%
  arrange(desc(Cambio_porcentual))
View(Primero_10)
cambio_porcentual%>%
  head(10)
## 4 ----

moda <- function(x) {
  return(as.numeric(names(which.max(table(x)))))
}

datos_restaurados <- datos %>%
  mutate(velocidad = replace_na(`Avg 
(Mbit/s)Ookla`, moda(`Avg 
(Mbit/s)Ookla`)))

velocidad_internet <-datos_restaurados %>%
  group_by(`Continental region`)%>%
  summarise(promedio=mean(velocidad))%>%
  arrange(promedio)%>%
  head(1)

velocidad_internet
## 5 ----


library(ggplot2)

ggplot(datos_restaurados, aes(`velocidad`,`Internet users`)) + geom_point()

